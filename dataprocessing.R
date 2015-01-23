rm(list=ls())
library(RJSONIO)
library(dplyr)
source("helper/helper.R")

## read in the data
raw.guns <- read.delim("data/GunsRawData.tsv", stringsAsFactors = FALSE)
raw.snakes <- read.delim("data/SnakesRawData.tsv", stringsAsFactors = FALSE)
raw.doors <- read.delim("data/DoorsRawData.tsv", stringsAsFactors = FALSE)
raw.controls <- read.delim("data/GunsControlRawData.tsv", stringsAsFactors = FALSE)

## make a long-form data frame containing all the trials
trials.guns <- plyr::ddply(raw.guns, c("workerid"), reread)
trials.snakes <- plyr::ddply(raw.snakes, c("workerid"), reread)
trials.doors <- plyr::ddply(raw.doors, c("workerid"), reread)
trials.controls <- plyr::ddply(raw.controls, c("workerid"), reread)

## add an experiment marker
trials.guns$expt <- "Face->Gun"
trials.snakes$expt <- "Face->Snake"
trials.doors$expt <- "Face->Door"
trials.controls$expt <- "Face->Control"

## clean up the data frames (not strictly necessary)
trials.guns <- trials.guns %>% 
  rename(prime = race, target = tool, 
         age = Age, gender = Male, ethnicity = Ethnicity) %>%
  mutate(target = ifelse(target=="t", "neutral", "threat")) %>%
  select(workerid, prime, target, accuracy, rt, responded, age, gender, ethnicity, expt)

trials.snakes <- trials.snakes %>% 
  rename(prime = race, target = snake, 
         age = Age, gender = Male, ethnicity = Ethnicity) %>%
  mutate(target = ifelse(target=="r", "neutral", "threat")) %>%
  select(workerid, prime, target, accuracy, rt, responded, age, gender, ethnicity, expt)

trials.doors <- trials.doors %>% 
  rename(prime = race, target = door, 
         age = Age, gender = Male, ethnicity = Ethnicity) %>%
  mutate(target = ifelse(target=="d", "neutral", "threat")) %>%
  select(workerid, prime, target, accuracy, rt, responded, age, gender, ethnicity, expt)

trials.controls <- trials.controls %>% 
  rename(target = tool, 
         age = Age, gender = Male, ethnicity = Ethnicity) %>%
  mutate(target = ifelse(target=="t", "neutral", "threat")) %>%
  select(workerid, target, accuracy, rt, responded, age, gender, ethnicity, expt)

### bind everything together
d <- bind_rows(trials.guns, trials.snakes, trials.doors, trials.controls)

## output 
write.csv(d, "data/consolidated_data.csv", row.names=FALSE)
