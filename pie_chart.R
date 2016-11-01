#race pie charts
#Mark Griffith
#QSS 30
#final project

library(dplyr)
library(readr)
library(plotly)
library(RColorBrewer)

#read in IPUMS data, look at only people in NY and FL born in Cuba, Dominican Republic, Haiti or Jamaica
ipums <- read_csv('./usa_00009.csv') %>% filter((STATEFIP %in% c(12,36)) &
                                                  BPLD %in% c(25000,26010,26020,26030))

#define Latin-Caribbean vs. Afro-Caribbean
Races <- ipums %>% mutate(Race=factor(ifelse(RACE==1,1,
                                             ifelse(RACE==2,2,3)),
                                             labels=c('White','Black','Mixed/Other')))
#recode race
Bplace <- Races %>% mutate(Birthplace=ifelse(BPLD==25000 | BPLD==26010,'Latin-Caribbean','Afro-Caribbean'))

#keep only relevant variables
Relevant <- Bplace %>% select(PERWT,Birthplace,Race) 

#group variables
figure2 <- Relevant %>% group_by(Birthplace,Race) %>% summarize(Number=sum(PERWT))
figure2b <- Relevant %>% group_by(Birthplace) %>% summarize(Total=sum(PERWT))
figure2 <- left_join(figure2,figure2b) %>% mutate(Percent=(Number/Total)*100)

#graph and export
png('Figure_1.png',height=500,width=1000)
ggplot(figure2,aes(x=factor(1),y=Percent,fill=Race)) + 
  geom_bar(width=1, stat='identity') +
  labs(x='',y='',title='1. Races of Afro-Caribbean (Haitian & Jamaican) and Latin-Caribbean (Cuban & Dominican) Immigrants to New York & Florida, 1950-2000') +
  scale_fill_brewer(palette='Set2') +
  facet_grid(.~Birthplace) +
  theme_bw() + theme(legend.position='bottom') + coord_polar(theta="y")
  dev.off()
