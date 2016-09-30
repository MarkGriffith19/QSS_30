#Mark Griffith
#QSS 30
#Lab 3
#9/29/16

#load packages
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

#read in IPUMS data, omit Alaska & Hawaii, limit to ages 15-65
a <- read_csv('./usa_00003.csv') %>% filter(AGE>=15 & AGE<=65 & !(STATEFIP %in% c(2,15)))

#recode RACE variable into Race
b <- a %>% mutate(Race=factor(ifelse(RACE==1,1,
                       ifelse(RACE==2,2,
                       ifelse(RACE==3,3,4))),
                  labels=c('white','black','Native American','Asian')))

#create occupation variable
c <- b %>% mutate(Occupation=factor(ifelse(OCC1950>=980, 1,
                                    ifelse((OCC1950>=100 & OCC1950<124) | (OCC1950>=810 & OCC1950<=840), 2,
                                    ifelse((OCC1950>=500 & OCC1950<691) | (OCC1950>=900 & OCC1950<=979), 3,
                                    ifelse((OCC1950>=200 & OCC1950<500), 4,
                                    ifelse((OCC1950>699 & OCC1950<800), 5, 6))))),
                  labels=c('none','farmers/farm laborers','craftsmen/operatives/laborers','managerial/clerical/sales','service','professional')))

#Recode SEX
d <- c %>% mutate(Sex=ifelse(SEX==1,'Male','Female'))

#keep only relevant variables
e <- d %>% select(YEAR,PERWT,Race,Occupation,Sex)

#group variables for graph 2
f2 <- e %>% group_by(YEAR,Sex,Race) %>% summarize(Number=sum(PERWT))
#group variables for graph 4
f4 <- e %>% group_by(YEAR,Sex,Race,Occupation) %>% summarize(Number=sum(PERWT))

#make stacked bar graph for pop. 15-65 by race, year & sex 1870-1920
png('Figure_2.png',height=500,width=1000)
ggplot(data=f2,aes(x=YEAR,y=Number,fill=Sex)) + 
  geom_bar(stat='identity') +
  labs(x='Year',y='Population',fill='Sex',title='2. Population Aged 15-65 by Race, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2',guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~Race,scales='free_y') +
  theme_bw()
dev.off()

#make stacked bar graph for occ. of ppl aged 15-65 by sex, race & year 1870-1920
png('Figure_4.png',height=500,width=1000)
ggplot(data=arrange(f4,Occupation),aes(x=YEAR,y=Number,fill=Occupation)) + 
  geom_bar(stat='identity',position='fill') +
  labs(x='Year',y='Percent of Population',fill='Occupation',title='4. Occupation of Persons Aged 15-65 by Sex, Race, and Year, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(Sex~.~Race) +
  theme_bw() + theme(legend.position='bottom')
dev.off()