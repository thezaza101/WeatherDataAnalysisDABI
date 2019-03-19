library(tidyverse)
library(rpart)
library(magrittr)
library(stringi)
library(scales)
library(rattle)
library(randomForest)
library(Matrix)
library(xgboost)
library(ROSE)

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



# Train a random forest model.
m_rf <- randomForest(form,
                     data=treeData[train,mvars],
                     na.action=na.roughfix,
                     importance=TRUE)
model <- m_rf
mtype <- "randomForest"
mdesc <- "random forest"

ggVarImp(model, log=TRUE)


model %>% predict(newdata=treeData[val, mvars], type="prob") %>%
  .[,2] %>%
  set_names(NULL) %>%
  round(2) %T>%
  {head(., 20) %>% print()} ->
  va_prob

model %>%
  predict(newdata=treeData[val, mvars], type="response") %>%
  set_names(NULL) %T>%
  {head(., 20) %>% print()} ->
  va_class


vaClass <- model %>% 
  predict(newdata=treeData[val, mvars], type="class") %>%
  set_names(NULL)

sum(vaClass == vaVals, na.rm=TRUE) %>%
  divide_by(vaClass %>% is.na() %>% not() %>% sum()) %T>%
  {
    percent(.) %>%
      sprintf("Overall accuracy = %s\n", .) %>%
      cat()
  } ->
  va_acc

sum(vaClass != vaVals, na.rm=TRUE) %>%
  divide_by(vaClass %>% is.na() %>% not() %>% sum()) %T>%
  {
    percent(.) %>%
      sprintf("Overall error = %s\n", .) %>%
      cat()
  } ->
  va_err


errorMatrix(vaVals, vaClass)


