library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

#Read in selected IPUMs data; filter out data pertaining to Alaska and Hawaii, while
#also limiting the data to only individuals from the age 15-65
a <- read_csv('./data/9_29.csv') %>% 
  filter(AGE>=15 & AGE<=65 & !(STATEFIP %in% c(2,15)))

#Check to ensure that Alaska and Hawaii was removed from the dataset
a %>% filter(AGE<15|AGE>65|STATEFIP %in% c(2,15))

#Recode Race to condense the data to four racial groups
b <- a %>% mutate(Race=factor(ifelse(RACE==1,1,
                       ifelse(RACE==2,2,
                       ifelse(RACE==3,3,4))),
labels=c('White','Black','Native American','Asian')))

#Recode Occupation
c <- b %>% mutate(Occupation=factor(ifelse(OCC1950>979,1,
                                           ifelse(OCC1950==100 | OCC1950==123 |
                                            (OCC1950>790 & OCC1950<910),2,
                                            ifelse((OCC1950>490 & OCC1950<700) | 
                                              (OCC1950>840 & OCC1950<980),3,
                                            ifelse(OCC1950>123 & OCC1950<500,4,
                                            ifelse(OCC1950>690 & OCC1950<810,5,6))))),
labels=c('none','farmers and farm laborers','craftsmen/operatives/laborers',
         'managerial/clerical/sales' ,'service','professional')))

#Recode Sex to distinguish whether an obstacle is male or female
d <- c %>% mutate(Sex=ifelse(SEX==1, 'Male','Female'))

#Only want to focus on the variables I intend to graph
e <- d %>% select(YEAR,PERWT,Sex,Race,Occupation)

#For Figure 2, I want to group by YEAR, Sex, and Race
f2 <- e %>% group_by(YEAR,Sex,Race) %>% summarise(Number=sum(PERWT))

#For Figure 4, I want to group by Year, Race, and Occupation
f4 <- e %>% group_by(YEAR,Race,Sex,Occupation) %>% summarise(Number=sum(PERWT))

#Graphing Figure 2
ggplot(data = f2,aes(x=YEAR,y=Number,fill=Sex)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Population',fill='Sex',title='2.Population Aged 15-65 by Sex and Race, 1870-1920') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2',guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~Race,ncol=2,scales='free_y') +
  theme_bw()
                    
#Graphing Figure
ggplot(data=f4,aes(x=YEAR,y=Number,fill=Occupation)) +
  geom_bar(stat='identity',position='fill') +
  labs(x='Year',y='Percent of Population',fill='Occupation',title='4. Occupation for People Aged 15-65 by Race, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(Sex~.~Race) +
    theme_bw() + theme(legend.position='bottom')
