library(tidyverse)
library(rpart)
library(magrittr)
library(stringi)
library(scales)
library(rattle)
library(ROSE)
library(ROCR)
library(reshape)
library(kernlab)

set.seed(123)

# Load the clean data
data <- read_csv("Data\\cleanData.csv")

data %>%
  count(Rain.tomorrow)


## Set the target variable
target <- "Rain.tomorrow"

## Set the columns to be used for modeling
mvars <- c("Rain.tomorrow",
           "Rainfall.mm.", "X3pm.Temperature.C.","X3pm.relative.humidity.",
           "X3pm.cloud.amount.oktas.","X3pm.wind.direction",
           "X3pm.wind.speed.km.h.","X3pm.MSL.pressure.hPa.")


# Construct the formulation of the modelling to undertake.
form <- data[mvars] %>% formula()

data <- ovun.sample(form, data = data[mvars], method = "both")$data

data %>%
  count(Rain.tomorrow)

data$Rain.tomorrow <- rescaler(data$Rain.tomorrow, "range")

# Create the Train, Test, Validation data
numRows <- data %>% nrow()

## training = 70%
train <- numRows %>% sample(0.70*numRows)
## Val
val <- numRows %>%  seq_len() %>% setdiff(train) %>% sample(0.15*numRows)
## test
test <- numRows %>% seq_len() %>% setdiff(union(train, val)) 


## construct the dataset used for modeling
treeData <- data[mvars]

## Cache the various actual values for target and risk.
trVals <- treeData[train,][[target]]
vaVals <- treeData[val,][[target]] 
teVals <- treeData[test,][[target]]


svmModel <- ksvm(as.factor(Rain.tomorrow) ~ .,
                 data=treeData[train,mvars],
                 kernel="rbfdot",
                 prob.model=TRUE)

vaClass <- svmModel %>% 
  kernlab::predict(newdata=treeData[val, mvars], type="class") %>%
  set_names(NULL)

vaClass <- kernlab::predict(svmModel, newdata=treeData[val, mvars])
