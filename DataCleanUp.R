# Load the required libraries
library(tidyverse)
library(magrittr)
library(lubridate) 

# list the relevent data files
files_list <- list.files('Data\\Canberra\\')

# Load the files into a dataframe
for (file in files_list){
  # if the"rawData" dataframe doesnt exist, create it and read the file into it
  if (!exists("rawData")){
    # The data starts at line 8, this is why we skip the first 7 lines
    rawData <- read_csv(paste("Data", "Canberra", file, sep="\\"),skip=7)
  }
  # if the "rawData" dataframe exists, add the contents of the file into it
  if (exists("rawData")){
    temp_data <- read_csv(paste("Data", "Canberra", file, sep="\\"),skip=7)
    rawData<-rbind(rawData, temp_data)
    rm(temp_dataset)
  }
}

# Clean up the columns

## Drop the "X1" column
rawData$X1 <- NULL

## Make column names syntactically valid
names(rawData)<-make.names(names(rawData),unique = TRUE)

## Clean up the column names
names(rawData) <- gsub(x = names(rawData), pattern = "U.00B0", replacement = "")  
names(rawData) <- gsub(x = names(rawData), pattern = "[\\..]{2,}", replacement = ".") 

# Data Cleaning & Feature creation

## Format all the dates to D-M-Y
frmtdate <- as.Date(rawData$Date, format="%Y-%m-%d")
rawData$Date[!is.na(frmtdate)] <- format(frmtdate[!is.na(frmtdate)], "%d/%m/%Y")

## Create the "Rain.Tomorrow" column (feature) based on the next day's rainfall amount
rawData <- rawData %>% 
  filter(!(Rainfall.mm. %>% is.na())) %>% 
  arrange(as.Date(Date,format="%d/%m/%Y")) %>%
  mutate(Rain.tomorrow=lead(Rainfall.mm.)>0)

write_csv(rawData,"Data\\rawData.csv")

## This will remove any columns where all data is missing
data <- Filter(function(x)!all(is.na(x)), rawData) 

## create a list of seasons orderd by the month number
seasons <- c("Summer", "Summer", "Autumn",
             "Autumn","Autumn", "Winter",
             "Winter", "Winter", "Spring",
             "Spring", "Spring", "Summer")

## Create the "Season" column (feature)
data <- data %>%
  mutate(Season = month(as.Date(Date,format="%d/%m/%Y")) %>%
           as.integer() %>%
           sapply(function(x) seasons[x]) %>%
           as.factor())

## This will clean up the data to make is easier analyse
data <- data %>% 
  ### This removes any rows where Rain.tomorrow == NA
  filter(!(Rain.tomorrow %>% is.na())) %>% 
  ### This will replace the value "Calm" in the 'speed' columns
  mutate(X9am.wind.speed.km.h.=replace(X9am.wind.speed.km.h., X9am.wind.speed.km.h.=="Calm", 0)) %>%
  mutate(X9am.wind.speed.km.h.=replace(X9am.wind.speed.km.h., X9am.wind.speed.km.h. %>% is.na(), 0)) %>%
  mutate(X3pm.wind.speed.km.h.=replace(X3pm.wind.speed.km.h., X3pm.wind.speed.km.h.=="Calm", 0)) %>%
  ### This will replace the "NA" values with 0 for the cloud amount columns
  mutate(X9am.cloud.amount.oktas.=replace(X9am.cloud.amount.oktas., X9am.cloud.amount.oktas. %>% is.na(), 0)) %>%
  mutate(X3pm.cloud.amount.oktas.=replace(X3pm.cloud.amount.oktas., X3pm.cloud.amount.oktas. %>% is.na(), 0)) %>%
  ### This will replace the "NA" values to "Calm", values are NA when wind speed == 0
  mutate(Direction.of.maximum.wind.gust=replace(Direction.of.maximum.wind.gust, Direction.of.maximum.wind.gust %>% is.na(), "Calm")) %>%
  mutate(X9am.wind.direction=replace(X9am.wind.direction, X9am.wind.direction %>% is.na(), "Calm")) %>%
  mutate(X3pm.wind.direction=replace(X3pm.wind.direction, X3pm.wind.direction %>% is.na(), "Calm")) %>%
  ### This will set any NA values in the temprature field as the average of the previous/next day
  mutate(Minimum.temperature.C.=replace(Minimum.temperature.C., Minimum.temperature.C. %>% is.na(), mean(c(lead(Minimum.temperature.C.),lag(Minimum.temperature.C.))))) %>%
  mutate(Maximum.temperature.C.=replace(Maximum.temperature.C., Maximum.temperature.C. %>% is.na(), mean(c(lead(Maximum.temperature.C.),lag(Maximum.temperature.C.))))) %>%
  ### Set the 'Speed.of.maximum.wind.gust.km.h.' to the grater of the 9am/3pm speeds 
  mutate(Speed.of.maximum.wind.gust.km.h.=replace(Speed.of.maximum.wind.gust.km.h., Speed.of.maximum.wind.gust.km.h. %>% is.na(), pmax(X9am.wind.speed.km.h., X3pm.wind.speed.km.h.))) %>%
  mutate(Direction.of.maximum.wind.gust=replace(Direction.of.maximum.wind.gust, Direction.of.maximum.wind.gust %>% is.na(), if (X9am.wind.speed.km.h.>X3pm.wind.speed.km.h.) X9am.wind.direction else X3pm.wind.direction)) %>%
  ### This will change the time of maximum wind gust to 00:00 when it is NA 
  mutate(Time.of.maximum.wind.gust=replace(Time.of.maximum.wind.gust,Time.of.maximum.wind.gust%>% is.na(), if (X9am.wind.speed.km.h.>X3pm.wind.speed.km.h.) 09:00:00 else 15:00:00)) %>%
  mutate(Change24HrRelativeHumidity=X3pm.relative.humidity.-lag(X3pm.relative.humidity.)) %>%
  mutate(Change24HrMSLpressurehPa=X3pm.MSL.pressure.hPa.-lag(X3pm.MSL.pressure.hPa.)) %>%
  mutate(Change24HrCloudAmountOktas=X3pm.cloud.amount.oktas.-lag(X3pm.cloud.amount.oktas.))


## Get the name of the wind direction columns
data %>% 
  select(contains("direction")) %>%
  names() ->
  compassData

## List of compass directions within the dataset
compassDir <- c("Calm",
                "N", "NNE", "NE", "ENE",
                "E", "ESE", "SE", "SSE",
                "S", "SSW", "SW", "WSW",
                "W", "WNW", "NW", "NNW")

## convert the wind direction variables to a factor
data[compassData] %<>%
  lapply(factor, levels=compassDir, ordered=FALSE) %>%
  data.frame() %>%
  tbl_df()

## Save the target variable name for our models
target <- "Rain.tomorrow"

## Convert the target to a factor
data[[target]] %<>% as.factor()

## drop the first/last row of the dataframe as it has incomplete values
data <- data[-nrow(data),] 
data <- data[-1,] 
## Make the rain tomorrow column the last column 

col_idxs <-1:ncol(data)
col_idx <- grep("Rain.tomorrow", names(data))
col_idxs <- col_idxs[!grepl(col_idx,unlist(col_idxs))]
col_idxs <- c(col_idxs, col_idx)
data <- subset(data, select=col_idxs)

## Write the clean data to a csv file
write_csv(data,"Data\\cleanData.csv")

## create a dataset that can be consumed by a back prop ANN
dataANN <- data
dataANN$Season <- as.numeric(dataANN$Season)
dataANN$Time.of.maximum.wind.gust <- NULL
dataANN$Direction.of.maximum.wind.gust <- NULL
dataANN$X9am.wind.direction <- NULL
dataANN$X3pm.wind.direction <- NULL
dataANN$Rain.tomorrow <- NULL
dataANN$Date <- NULL
dataANN$Rain.tomorrow <- as.numeric(data$Rain.tomorrow)
dataANN <- dataANN %>%
  select(Season, Change24HrRelativeHumidity, Change24HrMSLpressurehPa,Change24HrCloudAmountOktas,Rain.tomorrow)

write_csv(dataANN,"Data\\aNNData.csv")

# Clean up the workspace
rm(file,files_list,temp_data, frmtdate, rawData, compassDir, compassData,seasons,col_idxs,col_idx,dataANN)

