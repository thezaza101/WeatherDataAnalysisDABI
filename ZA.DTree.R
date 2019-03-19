library(tidyverse)
library(rpart)
library(magrittr)
library(stringi)
library(scales)
library(rattle)
library(ROSE)
library(ROCR)

set.seed(123)

# Load the clean data
data <- read_csv("Data\\cleanData.csv")

data %>%
  count(Rain.tomorrow)


## Set the target variable
target <- "Rain.tomorrow"

## Convert the target to a factor
data[[target]] %<>% as.factor()


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

treeData$X3pm.wind.direction  %<>% as.factor()

## Cache the various actual values for target and risk.
trVals <- treeData[train,][[target]]
vaVals <- treeData[val,][[target]] 
teVals <- treeData[test,][[target]]




# Train a decision tree model.
loss <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
treeModel <- rpart(form, treeData[train,mvars], parms=list(loss=loss), control = list(maxdepth = 4))

ggVarImp(treeModel, log=TRUE)

# Visualise the tree
fancyRpartPlot(treeModel, palettes="Blues")


vaClass <- treeModel %>% 
  predict(newdata=treeData[val, mvars], type="class") %>%
  set_names(NULL)

## Val Accuracy
sum(vaClass == vaVals) %>%
  divide_by(length(vaVals)) %>%
  percent(.)

## Val error Matrix
vaMatrix <- errorMatrix(vaVals, vaClass)


teClass <- treeModel %>% 
  predict(newdata=treeData[test, mvars], type="class") %>%
  set_names(NULL)

## Test accuracy
sum(teClass == teVals) %>%
  divide_by(length(teVals)) %>%
  percent(.)

## test error Matrix
teMatrix <- errorMatrix(teVals, teClass)


## Create additional variables for the ROC Chart

vaProb <- treeModel %>% 
  predict(newdata=treeData[val, mvars], type="prob") %>%
  .[,2] %>%
  set_names(NULL) %>%
  round(2)

teProb <- treeModel %>% 
  predict(newdata=treeData[test, mvars], type="prob") %>%
  .[,2] %>%
  set_names(NULL) %>%
  round(2)

teRec <- (teMatrix[2,2]/(teMatrix[2,2]+teMatrix[2,1]))
tePre <- (teMatrix[2,2]/(teMatrix[2,2]+teMatrix[1,2]))
teFsc <- ((2 * tePre * teRec)/(teRec + tePre))

teAuc <- teProb %>%
  prediction(teClass) %>%
  performance("auc") %>%
  attr("y.values")  %>%
  .[[1]] 

teRates <- teProb %>%
  prediction(teClass) %>%
  performance("tpr", "fpr")

data_frame(tpr=attr(teRates, "y.values")[[1]],
           fpr=attr(teRates, "x.values")[[1]]) %>%
  ggplot(aes(fpr, tpr)) +
  geom_line() +
  annotate("text", x=0.875, y=0.125, vjust=0,
           label=paste("AUC =", percent(teAuc))) +
  labs(title="ROC Curve - " %s+% "decision tree" %s+% " - Test Dataset",
       x="False Positive Rate (1-Specificity)",
       y="True Positive Rate (Sensitivity)")
