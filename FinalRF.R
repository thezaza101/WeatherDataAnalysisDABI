#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2018-10-23 14:33:31 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'theza'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 

# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2018-10-23 14:33:46 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///D:/Users/theza/Documents/DABI/Assignment/Data/CleanDataROSE.csv" 
crs$dataset <- read.csv(fname,
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2018-10-23 14:33:46 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=425 train=298 validate=64 test=63

set.seed(123)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# The following variable selections have been noted.

crs$input     <- c("Rainfall.mm.", "X3pm.Temperature.C.",
                   "X3pm.relative.humidity.",
                   "X3pm.cloud.amount.oktas.", "X3pm.wind.direction",
                   "X3pm.wind.speed.km.h.", "X3pm.MSL.pressure.hPa.")

crs$numeric   <- c("Rainfall.mm.", "X3pm.Temperature.C.",
                   "X3pm.relative.humidity.",
                   "X3pm.cloud.amount.oktas.",
                   "X3pm.wind.speed.km.h.", "X3pm.MSL.pressure.hPa.")

crs$categoric <- "X3pm.wind.direction"

crs$target    <- "Rain.tomorrow"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2018-10-23 14:33:52 x86_64-w64-mingw32 

# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Rescale Rain.tomorrow.

crs$dataset[["R01_Rain.tomorrow"]] <- crs$dataset[["Rain.tomorrow"]]

# Rescale to [0,1].

if (building)
{
  crs$dataset[["R01_Rain.tomorrow"]] <-  rescaler(crs$dataset[["Rain.tomorrow"]], "range")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R01_Rain.tomorrow"]] <- (crs$dataset[["Rain.tomorrow"]] - 0.000000)/abs(1.000000 - 0.000000)
}

#=======================================================================
# Rattle timestamp: 2018-10-23 14:33:52 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("Rainfall.mm.", "X3pm.Temperature.C.",
                   "X3pm.relative.humidity.",
                   "X3pm.cloud.amount.oktas.", "X3pm.wind.direction",
                   "X3pm.wind.speed.km.h.", "X3pm.MSL.pressure.hPa.")

crs$numeric   <- c("Rainfall.mm.", "X3pm.Temperature.C.",
                   "X3pm.relative.humidity.",
                   "X3pm.cloud.amount.oktas.",
                   "X3pm.wind.speed.km.h.", "X3pm.MSL.pressure.hPa.")

crs$categoric <- "X3pm.wind.direction"

crs$target    <- "R01_Rain.tomorrow"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- "Rain.tomorrow"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:09 x86_64-w64-mingw32 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(as.factor(R01_Rain.tomorrow) ~ .,
                                     data=crs$dataset[crs$train, c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=2,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

varImp(crs$rf)

# Generate textual output of the 'Random Forest' model.

crs$rf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))FALSE

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 0.38 secs

#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:17 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$R01_Rain.tomorrow, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$R01_Rain.tomorrow, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:21 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$R01_Rain.tomorrow, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$R01_Rain.tomorrow, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:24 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$R01_Rain.tomorrow)
print(riskchart(crs$pr, 
                na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$R01_Rain.tomorrow, 
                title="Performance Chart Random Forest CleanDataROSE.csv [validate] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:27 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$R01_Rain.tomorrow)
print(riskchart(crs$pr, 
                na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$R01_Rain.tomorrow, 
                title="Performance Chart Random Forest CleanDataROSE.csv [test] ", show.lift=TRUE, show.precision=TRUE, legend.horiz=FALSE))


#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:33 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on CleanDataROSE.csv [validate].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$R01_Rain.tomorrow)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest CleanDataROSE.csv [validate] R01_Rain.tomorrow")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$R01_Rain.tomorrow)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

#=======================================================================
# Rattle timestamp: 2018-10-23 14:34:38 x86_64-w64-mingw32 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on CleanDataROSE.csv [test].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]),
                  type    = "prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$R01_Rain.tomorrow)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest CleanDataROSE.csv [test] R01_Rain.tomorrow")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$R01_Rain.tomorrow)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")