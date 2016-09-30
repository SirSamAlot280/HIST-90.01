#Initial setting
rm(list = ls())

library(dplyr)
library(readr)
library(tidyr)

#Located the "designated file"9_22.csv" from the "data" folder in the project folder
a <- read_csv('./data/9_22.csv') %>% filter(!STATEFIP %in% c(2,15))

#Read in crosswalk to create a dataframe titled "raced"
raced <- read_csv('./data/raced.csv',col_types = cols(RACED='i'))

#Add racial identity to dataframe with the left_join function
c <- left_join(a,raced,by='RACED')

#Use group by() and summarise() to determine the population by year and race
d <- c %>% group_by(YEAR,Race) %>% summarise(NUMBER= sum(PERWT))

#One row for each category of raced and one column for reach year
e <- d %>% spread(YEAR,NUMBER)

#Export to .csv
write_csv(e,'./data/year_race.csv')
