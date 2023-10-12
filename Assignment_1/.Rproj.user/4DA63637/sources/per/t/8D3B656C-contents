library(shiny)
library(shinyjs)
library(vcd)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(pls)
library(ggplot2)
library(devtools)
library(car)
#library(tabplot)
library(textdata)
library(seriation)
library(visNetwork)
library(leaflet)
library(shinycssloaders)
library(DT)
library(plotly)

library(data.table)
library(jstable)
library(GGally)
library(summarytools)

# data import and processing
data = read.csv(file = 'Ass1Data.csv', header = TRUE)

data$Date = as.Date(data$Date)

# convert cols to factors
data = data %>%  
  mutate(Operator = as.factor(Operator), Priority = as.factor(Priority),
         Price = as.factor(Price), Speed = as.factor(Speed),
         Duration = as.factor(Duration), Temp = as.factor(Temp),
         Location = as.factor(Location), Agreed = as.factor(Agreed),
         State = as.factor(State), Class = as.factor(Class),
         Surface = as.factor(Surface)
         )
num_data = data %>% select_if(is.numeric)

#assign variable types to variables
allVars <- colnames(data)
factVars <- colnames(data)[unlist(lapply(data, is.factor))]
numVars <- colnames(data)[unlist(lapply(data, is.numeric))]

#get colnames to make choice variables
choices_cat = colnames(as.data.frame(data %>% select(c(3,5:14))))
choices_pairs = colnames(as.data.frame(data %>% select(-ID, -Date)))

domChoices <- c("l","f","r","t","i","p")
