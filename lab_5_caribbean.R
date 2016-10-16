library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maptools)
library(gtools)

#read in the map file
mapdata <- read_csv('map.csv')

#draw the map with a white bg and black outline
map1 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, 
               aes(x=long,y=lat,group=group),fill='white', color='black')
png('lab_5_map.png',width=1500,height=1000)
print(map1)
dev.off()

#read in the ipums data
ipums <- read_csv('usa_00007.csv', col_types = cols(PERWT=col_double()))

#keep only Dominican, Haitian & Jamaican immigrants
carib <- ipums %>% filter(BPLD %in% c(26010,26020,26030))

#aggregate by year and statefip
caribbean <- carib %>% group_by(YEAR,STATEFIP) %>% summarize(Number=sum(PERWT))

#create a variable with STATEFIP's data as integers
newmap <- mapdata %>% mutate(STATEI=as.integer(STATEFIP))

#join the two
caribmap <- left_join(caribbean,newmap,by=c('STATEFIP'='STATEI'))

#make sure it maps at all
map2 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, 
               aes(x=long,y=lat,group=group),fill='white', color='black') +
  geom_polygon(data=filter(caribmap,YEAR==2000),
               aes(x=long,y=lat,group=group,fill=Number),color='black')
png('caribbean.png',width=1500,height=1000)
print(map2)
dev.off()

#split the data into eight even sections
cuts <-quantcut(caribbean$Number,q=seq(0,1,.125))

#using cuts, determine the eight sections
caribcats <- caribbean %>% mutate(Population=factor(ifelse(Number<100,1,
                                                    ifelse(Number<200,2,
                                                    ifelse(Number<301,3,
                                                    ifelse(Number<504,4,
                                                    ifelse(Number<998,5,
                                                    ifelse(Number<2190,6,
                                                    ifelse(Number<8700,7,8)))))))))

#write the ranges how they will appear on the  legend
levels(caribcats$Population) <- c('1-99','100-199','199-300','301-503','504-997','998-2189','2190-8699','8700+')

#arrange by order
caribbeanmap <- left_join(caribcats,newmap,by=c('STATEFIP'='STATEI')) %>% arrange(order)

#create a map for each decade
map2 <- map1 + scale_fill_brewer(palette='Greens') +
  geom_polygon(data=filter(caribbeanmap),
  aes(x=long,y=lat,group=group,fill=Population),color='black') +
  labs(title=paste('Dominican-, Haitian-, and Jamaican-Born Persons in the United States')) +
  facet_wrap(~YEAR)
png(paste('final_map.png'),width=1500,height=1000)
print(map2)
dev.off()