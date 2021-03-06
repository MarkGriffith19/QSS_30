#Mark Griffith
#double bar graph
#QSS 30
#final project

#read in packages
library(dplyr)
library(readr)
library(plotly)
library(RColorBrewer)

#read in IPUMS data
ipums <- read_csv('./usa_00015.csv', col_types = cols(CPI99=col_double()))
 
#look at only people in NY and FL btwn age 18-65 w/ positive incomes born in Cuba, Dominican Republic, Haiti or Jamaica
filtered_ipums <- ipums %>% filter((STATEFIP %in% c(12,36)) &
                                    BPLD %in% c(25000,26010,26020,26030) & 
                                    (AGE>=18 & AGE<=65) &
                                     (INCWAGE>0 & INCWAGE<999999))

#define Latin-Caribbean vs. Afro-Caribbean
Bplace <- filtered_ipums %>% mutate(Birthplace=factor(ifelse((BPLD==25000 | BPLD==26010),1,2),
          labels=c('Latin-Caribbean (Cuba & Dominican Republic)','Afro-Caribbean (Haiti & Jamaica)')))

#recode STATEFIP
NY_FL <- Bplace %>% mutate(State=ifelse(STATEFIP==12,'Florida','New York'))

#adjust income for inflation
income <- NY_FL %>% mutate(Inc=INCWAGE * CPI99)

#use SLWT in 1950 and PERWT in all other years
weight <- income %>% mutate(Weight=ifelse(YEAR!=1950,PERWT,SLWT))

#adjust for top-coding
adjincome<- weight %>% mutate(Inc=ifelse(Inc>70000,70000,Inc))

#create ranges of income
incomeranges <- adjincome %>% mutate(incrange=factor(ifelse(Inc<10000,1,
                                                  ifelse(Inc<20000,2,
                                                  ifelse(Inc<50000,3,
                                                  ifelse(Inc<70000,4,5)))),
                  labels=c('<10','<20','<50','<70','70+')))

#keep only relevant variables
Relevant <- incomeranges %>% select(YEAR,Weight,State,Birthplace,incrange)

#group variables
figure3a <- Relevant %>% group_by(YEAR,Birthplace,State,incrange) %>% summarize(Number=sum(Weight))
figure3b <- Relevant %>% group_by(YEAR,Birthplace,State) %>% summarize(Total=sum(Weight))
#create percentage values
figure3 <- left_join(figure3a,figure3b) %>% mutate(Percent=(Number/Total)*100)

#create data frame for all instances when the number of Afro-Caribbean incomes in a range is 0
#stops ggplot from graphing the same bars next to each other
YEAR = c(1950, 1950, 1960, 1960, 1970, 1970)
Birthplace = c("Afro-Caribbean (Haiti & Jamaica)", "Afro-Caribbean (Haiti & Jamaica)", 
               "Afro-Caribbean (Haiti & Jamaica)", "Afro-Caribbean (Haiti & Jamaica)", 
               "Afro-Caribbean (Haiti & Jamaica)", "Afro-Caribbean (Haiti & Jamaica)")
State = c("Florida", "Florida", "Florida", "Florida", "Florida", "Florida")
incrange = c("<20","<50","<50","<70","<70","70+")
Number = c(0,0,0,0,0,0)
Total = c(100,100,100,100,100,100)
Percent = c(0,0,0,0,0,0)
extra = data.frame(YEAR, Birthplace, State, incrange, Number, Total, Percent)

#bind this data frame to the figure 3 data frame
figure3new <- rbind(as.data.frame(figure3), as.data.frame(extra))

#make double bar graph
png('Figure_3.png',height=500,width=1000)
ggplot(figure3new,aes(x=incrange,y=Percent/100,fill=Birthplace)) + 
  geom_bar(stat='identity',position='dodge') +
  labs(x='Income in Thousands of U.S. Dollars',y='Percent of Population',fill='Region of Birth',
       title='3. Income of Latin-Caribbean vs. Afro-Caribbean Immigrants Aged 18-65 by Census Year, 1950-2000') +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(State~.~YEAR) +
  theme_bw() + theme(legend.position='bottom')
dev.off()
