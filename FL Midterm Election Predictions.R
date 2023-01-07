library(tidyverse)
library(lubridate)
library(haven)
library(dplyr)

setwd("/Users/arishleyka/Desktop/385 project")
getwd()

df<-read.csv("Cleaned Florida Dataset.csv")

col_names = c()
head(df)

fl.train <- df %>%
  select(
    Gender,
    Race,
    Birth_Date,
    Party_Affiliation,
    County_Code,
    Precint
  )

fl.train <- fl.train %>%
  mutate(
    Gender=as.integer(Gender == "F"),
    Race=ifelse(Race %in% c(1,2), "AAPI", Race),
    Race=ifelse(Race %in% c(3), "BNH", Race),
    Race=ifelse(Race %in% c(4), "HIS", Race),
    Race=ifelse(Race %in% c(5), "WNH", Race),
    Race=ifelse(Race %in% c(6:9), "OTH", Race),
    AGE=as.Date("2022-09-28")-mdy(Birth_Date)
  )

fl.train <- fl.train %>%
  mutate(
    AGE = as.integer(AGE/365)
  )

fl.train <- fl.train %>%
  filter(Party_Affiliation != "NPA") %>%
  mutate(
    Party_Affiliation=as.integer(Party_Affiliation=="DEM")
  )

lm1 <- glm(Party_Affiliation ~ AGE * Gender, family = binomial(link="logit"), data=fl.train)
summary(lm1)

## glm(response, predictor, .....)
## important to interpret 

presret <- read_csv("preselections_dan.csv")
presret

econ <- read_csv("GDPC1.csv") ####economy of the entire country, need one of just florida 
econ

plot(econ)
econ$GDP2 <- lag(econ$GDPC1, 4)
econ$growth <-  log(econ$GDPC1) - log(econ$GDP2)
hist(econ$growth)
econ <- econ %>%
  mutate(
    year=year(DATE),
    month=month(DATE)
  ) %>%
  filter(month==4) %>%
  select(year, growth)
econ
plot(econ)

pres <- presret %>% ###need to isolate only florida 
  mutate(DemVotes=as.numeric(gsub(",", "", DemVotes))) %>% ###error 
  group_by(year=Year) %>%
  summarize(
    dvotes=sum(DemVotes),
    total=sum(TotalVotes)
  ) %>%
  mutate(
    dempct=dvotes/total*100
  ) %>%
  left_join(econ) %>%
  mutate(
    dempct_lag = lag(dempct, 1)
  )
pres

incumb <- c( ###this is incumbency of the entire country, need to manually create a vector for FL 
  0,
  0,
  1,
  0,
  1,
  0,
  0,
  1,
  0,
  0,
  1,
  0,
  1,
  0,
  1
)


df$incumb <- incumb

lm1 <- lm(dempct ~ growth + dempct_lag + incumb, data=df)
summary(lm1)
