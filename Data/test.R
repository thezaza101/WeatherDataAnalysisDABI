library(tidyverse)
library(plotly)

dat <- read_csv("Data\\aNNData.csv")

p <- plot_ly(data, x = ~Change24HrRelativeHumidity, y = ~Change24HrMSLpressurehPa, z = ~Change24HrCloudAmountOktas, color =~Rain.tomorrow, size =1) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Relative Humidity'),
                      yaxis = list(title = 'MSL pressure hPa'),
                      zaxis = list(title = 'Cloud Amount oktas')))
