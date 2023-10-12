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
library(ggstatsplot)
library(naniar)
library(rpart)
library(rpart.plot)
library(caret)
library(glmnet)
library(recipes)



# data import and processing
data = read.csv("Ass2Data.csv", header = TRUE, na.strings =
             c('NA','-99', '--'), stringsAsFactors = TRUE)


#Test train split 
#train <- data[data$OBS_TYPE == "Train"]
#test <- data[data$OBS_TYPE == "Test"]


########### CLEANING ###############

#copy dataset
covid = data


#categorical missing values that are "Not Applicable" and create new level NONE
covid$POLITICS <- as.character(covid$POLITICS) #convert away from factor
covid$POLITICS[is.na(covid$POLITICS)] <- "NONE"
covid$POLITICS <- as.factor(covid$POLITICS) # convert back to factor
covid$HEALTHCARE_BASIS <- as.character(covid$HEALTHCARE_BASIS) #convert away from factor
covid$HEALTHCARE_BASIS[is.na(covid$HEALTHCARE_BASIS)] <- "NONE"
covid$HEALTHCARE_BASIS <- as.factor(covid$HEALTHCARE_BASIS) # convert back to factor

# numeric missing values that are “Not Applicable” and create new level 0
# dat_2$POPULATION_shadow <- as.numeric(is.na(dat_2$POPULATION)) 
# #dat_2$POPULATION[is.na(dat_2$POPULATION)] <- 0 
# dat_2$AGE25_PROPTN_shadow <- as.numeric(is.na(dat_2$AGE25_PROPTN))
# #dat_2$AGE25_PROPTN[is.na(dat_2$AGE25_PROPTN)] <- 0 
# dat_2$AGE50_PROPTN_shadow <- as.numeric(is.na(dat_2$AGE50_PROPTN)) 
# #dat_2$AGE50_PROPTN[is.na(dat_2$AGE50_PROPTN)] <- 0 
# dat_2$AGE_MEDIAN_shadow <- as.numeric(is.na(dat_2$AGE_MEDIAN)) 
# #dat_2$AGE_MEDIAN[is.na(dat_2$AGE_MEDIAN)] <- 0 
# dat_2$POP_DENSITY_shadow <- as.numeric(is.na(dat_2$POP_DENSITY)) 
# #dat_2$POP_DENSITY[is.na(dat_2$POP_DENSITY)] <- 0 
# dat_2$GDP_shadow <- as.numeric(is.na(dat_2$GDP)) 
# #dat_2$GDP[is.na(dat_2$GDP)] <- 0 
# dat_2$INFANT_MORT_shadow <- as.numeric(is.na(dat_2$INFANT_MORT)) 
# #dat_2$INFANT_MORT[is.na(dat_2$INFANT_MORT)] <- 0 
# dat_2$DOCS_shadow <- as.numeric(is.na(dat_2$DOCS)) 
# #dat_2$DOCS[is.na(dat_2$DOCS)] <- 0 
# dat_2$VAX_RATE_shadow <- as.numeric(is.na(dat_2$VAX_RATE)) 
# #dat_2$VAX_RATE[is.na(dat_2$VAX_RATE)] <- 0 
# dat_2$HEALTHCARE_COST_shadow <- as.numeric(is.na(dat_2$HEALTHCARE_COST)) 
# #dat_2$HEALTHCARE_COST[is.na(dat_2$HEALTHCARE_COST)] <- 0 
# dat_2$DEATH_RATE_shadow <- as.numeric(is.na(dat_2$DEATH_RATE)) 
# #dat_2$DEATH_RATE[is.na(dat_2$DEATH_RATE)] <- 0 

#infill NAs with 0 for healthcare_cost
#data_$num[is.na(dat$num)] <- 0

#This calculates the ratio of missingness of a vector.
#pMiss <- function(x){ sum(is.na(x))/length(x)*100 }


#get numeric data for pairs plot 
num_data = data %>% select_if(is.numeric)

#assign variable types to variables
allVars = colnames(data)
factVars = colnames(data)[unlist(lapply(data, is.factor))]
numVars = colnames(data)[unlist(lapply(data, is.numeric))]

#get col-names to make choice variables
choices_cat = colnames(as.data.frame(data %>% dplyr::select(c(2,12))))
choices_pairs = colnames(as.data.frame(data %>% dplyr::select(-CODE, -POLITICS, -HEALTHCARE_BASIS, -OBS_TYPE)))

choices_pairs_colour = colnames(as.data.frame(data %>% dplyr::select(POLITICS, HEALTHCARE_BASIS)))

domChoices <- c("l","f","r","t","i","p")
