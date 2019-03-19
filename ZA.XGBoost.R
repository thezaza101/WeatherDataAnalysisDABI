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
data <- read_csv("Data\\CleanDataROSE.csv")

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


formula(target %s+% "~ .-1") %>%
  sparse.model.matrix(data=treeData[mvars] %>% na.roughfix()) %T>%
  {dim(.) %>% print()} %T>%
  {head(.) %>% print()} ->
  sds

treeData[target] %>%
  unlist(use.names=FALSE) %>%
  equals("TRUE") %T>%
  {head(., 20) %>% print()} ->
  label

m_xg <- xgboost(data=sds[train,],
                label=label[train],
                nrounds=100,
                print_every_n=15,
                objective="binary:logistic")

model <- m_xg
mtype <- "xgboost"
mdesc <- "extreme gradient boosting"

#xgb.importance(feature_names=colnames(sds), model)

ggVarImp(model, feature_names=colnames(sds), n=20)

model %>%
  predict(newdata=sds[val,]) %>%
  set_names(NULL) %>%
  round(2) %T>%
  {head(., 20) %>% print()} ->
  va_prob


va_prob %>%
  is_greater_than(0.5) %>%
  ifelse("TRUE", "FALSE") %T>%
  {head(., 20) %>% print()} ->
  vaClass

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



errorMatrix(vaVals, vaClass, count=T)


model %>%
  predict(newdata=sds[test,]) %>%
  set_names(NULL) %>%
  round(2) %T>%
  {head(., 20) %>% print()} ->
  te_prob


te_prob %>%
  is_greater_than(0.5) %>%
  ifelse("TRUE", "FALSE") %T>%
  {head(., 20) %>% print()} ->
  teClass

errorMatrix(teVals, teClass, count=T)

