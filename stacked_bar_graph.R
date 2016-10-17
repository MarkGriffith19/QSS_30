#occupations stacked bar graph
#Mark Griffith
#QSS 30
#Final Project

library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)

#read in IPUMS data, omit Alaska & Hawaii, limit to ages 15-65
ipums <- read_csv('./usa_00009.csv') %>% filter(AGE>=15 & AGE<=65 & (STATEFIP %in% c(12,36)) &
                                                BPLD %in% c(25000,26010,26020,26030))
#define Latin-Caribbean vs. Afro-Caribbean
Bplace <- ipums %>% mutate(Birthplace=factor(ifelse((BPLD==25000 | BPLD==26010),1,2),
                           labels=c('Latin-Caribbean (Cuba & Dominican Republic)','Afro-Caribbean (Haiti & Jamaica)')))

#create occupation variable
Occ <- Bplace %>% mutate(Occupation=factor(ifelse(OCC1990>=905, 1,
                                    ifelse((OCC1990>=473 & OCC1990<=498), 2,
                                    ifelse((OCC1990>=503 & OCC1990<=889), 3,
                                    ifelse((OCC1990>=405 & OCC1990<=469), 4, 5)))),
                                    labels=c('none','farmers/foresters/fishers','craftsmen/operatives/laborers','service','managerial/professional/technical/sales')))

#recode STATEFIP
NY_FL <- Occ %>% mutate(State=ifelse(STATEFIP==12,'Florida','New York'))

#keep only relevant variables
Relevant <- NY_FL %>% select(YEAR,PERWT,State,Birthplace,Occupation)

#group variables
figure1 <- Relevant %>% group_by(YEAR,State,Birthplace,Occupation) %>% summarize(Number=sum(PERWT))

#make stacked bar graph for occ. of ppl aged 15-65 by sex, race & year 1870-1920
png('Figure_1.png',height=500,width=1000)
ggplot(data=arrange(figure1,Occupation),aes(x=YEAR,y=Number,fill=Occupation)) + 
  geom_bar(stat='identity',position='fill') +
  labs(x='Year',y='Percent of Population',fill='Occupation',
       title='1. Occupation of Cuban-, Dominican-, Haitian-, and Jamaican-Born Persons Aged 15-65 by Region & Year, 1950-2000') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1950,1970,1990)) +
  scale_fill_brewer(palette='Accent') +
  facet_grid(State~.~Birthplace) +
  theme_bw() + theme(legend.position='bottom')
dev.off()