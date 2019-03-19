library(GGally) 
library(RColorBrewer) 
library(dplyr) 
library(ggplot2)
library(gridExtra)
library(lubridate)
library(magrittr)
library(randomForest)
library(rattle)
library(scales)
library(stringi)
library(stringr)
library(readr)

data <- read_csv("Data\\cleanData.csv")
data$X3pm.wind.direction %<>% as.factor()
blues2 <- brewer.pal(4, "Paired")[1:2]

##Vis 1
P1 <- data %>%
  ggplot(aes(x=Season, fill=Rain.tomorrow)) +
  geom_bar() +
  scale_fill_manual(values = blues2,
                    labels = c("No Rain", "Rain")) +
  scale_y_continuous(labels=comma) +
  theme(legend.title = element_text(colour="grey40"),
        legend.text = element_text(colour="grey40"),
        legend.background = element_rect(fill="transparent")) +
  labs(title = "Rain Expected by Season",
       subtitle = "Observations at Canberra Airport (02/08/2017 - 01/09/2018)",
       x = "Season",
       y = "Number of Days",
       fill = "Rain Tomorrow")


P2 <- data %>%
  ggplot(aes(x=factor(1), fill=Rain.tomorrow)) +
  geom_bar() +
  scale_fill_manual(values = blues2,
                    labels = c("No Rain", "Rain")) +
  theme(legend.position="none") +
  labs(title = "Distribution of Rain vs No Rain",
       caption = "Source: Australian Bureau of Meteorology {station 070351}",
       fill = "Tomorrow") +
  coord_polar(theta = "y") +
  theme_void() +
  guides(fill=FALSE)


multiplot(P1, P2,cols=2)



data %>%
  ggplot(aes(x=X3pm.relative.humidity., fill=Rain.tomorrow)) +
  geom_density(alpha=0.55) +
  labs(title="Distribution relative humidity at 3pm",
       subtitle="Observations at Canberra Airport (02/08/2017 - 01/09/2018)",
       caption="Source: Australian Bureau of Meteorology {station 070351}",
       x="Relative humidity at 3pm",
       y="Density",
       fill = "Rain Tomorrow")

data %>%
  ggplot(aes(x=X3pm.relative.humidity.)) +
  geom_density(aes(fill=Rain.tomorrow), alpha=0.55) +
  labs(title="Distribution relative humidity at 3pm",
       subtitle="Observations at Canberra Airport (02/08/2017 - 01/09/2018)",
       caption="Source: Australian Bureau of Meteorology {station 070351}",
       x="Relative humidity at 3pm",
       y="Density",
       fill = "Rain Tomorrow")


dataRain <- data %>%
  filter(Rain.tomorrow)
dataNoRain <- data %>%
  filter(not(Rain.tomorrow))
# overlay histogram and normal density


G1 <- data %>%
  ggplot(aes(x=X3pm.cloud.amount.oktas.,fill=Rain.tomorrow )) +
  geom_histogram(aes(y = stat(density)), bins = 18, binwidth=0.5) +
  scale_fill_manual(values = blues2) +
  coord_cartesian(ylim=c(0,1)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(x=dataRain$X3pm.cloud.amount.oktas.), sd = sd(x=dataRain$X3pm.cloud.amount.oktas.)), 
    lwd = 1, 
    col = '#1e43b3') +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(x=dataNoRain$X3pm.cloud.amount.oktas.), sd = sd(x=dataNoRain$X3pm.cloud.amount.oktas.)), 
    lwd = 1, 
    col = '#a6b5e3') +
  labs(title="Distribution cloud cover at 3pm",
       subtitle="Observations at Canberra Airport (02/08/2017 - 01/09/2018)",
       x="Relative humidity at 3pm",
       y="Density",
       fill = "Rain Tomorrow") +
  scale_x_continuous(breaks = 0:8,
                     labels = paste0(c("0", "12.5", "25","37.5", "50","62.5", "75","87.5", "100"), "%")) +
  scale_y_continuous(limits=c(0, 1),minor_breaks =1) 

G2 <- data %>%
  ggplot(aes(x=X3pm.MSL.pressure.hPa.,fill=Rain.tomorrow )) +
  geom_histogram(aes(y = stat(density))) +
  scale_fill_manual(values = blues2) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(x=dataRain$X3pm.MSL.pressure.hPa.), sd = sd(x=dataRain$X3pm.MSL.pressure.hPa.)), 
    lwd = 1, 
    col = '#1e43b3') +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(x=dataNoRain$X3pm.MSL.pressure.hPa.), sd = sd(x=dataNoRain$X3pm.MSL.pressure.hPa.)), 
    lwd = 1, 
    col = '#a6b5e3') +
  labs(title="Distribution MSL pressure at 3pm",
       x="Relative humidity at 3pm",
       y="Density",
       fill = "Rain Tomorrow")+
  guides(fill=FALSE)

G3 <- data %>%
  ggplot(aes(x=X3pm.relative.humidity.,fill=Rain.tomorrow )) +
  geom_histogram(aes(y = stat(density))) +
  scale_fill_manual(values = blues2) +
  coord_cartesian(xlim=c(0,100)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(x=dataRain$X3pm.relative.humidity.), sd = sd(x=dataRain$X3pm.relative.humidity.)), 
    lwd = 1, 
    col = '#1e43b3') +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(x=dataNoRain$X3pm.relative.humidity.), sd = sd(x=dataNoRain$X3pm.relative.humidity.)), 
    lwd = 1, 
    col = '#a6b5e3') +
  labs(title="Distribution relative humidity at 3pm",
       x="Relative humidity at 3pm",
       y="Density",
       fill = "Rain Tomorrow") +
  scale_x_continuous(limits=c(0, 100),minor_breaks =100)+
  guides(fill=FALSE)

G4 <- data %>%
  ggplot(aes(x=factor(X3pm.wind.direction), fill=Rain.tomorrow)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = blues2,
                    labels = c("No Rain", "Rain")) +
  scale_y_continuous(labels=percent) +
  theme(legend.title = element_text(colour="grey40"),
        legend.text = element_text(colour="grey40"),
        legend.background = element_rect(fill="transparent")) +
  labs(title = "Rain Expected by Wind Direction at 3pm",
       caption = "Source: Australian Bureau of Meteorology {station 070351}",
       x = "Wind Direction 3pm",
       y = "% of observation",
       fill = "Rain Tomorrow") +
  guides(fill=FALSE)

multiplot(G1, G2, G3, G4, cols=2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


