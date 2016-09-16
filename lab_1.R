# data frames
author <- c('Shakespeare','Hemingway','Vonnegut','Kafka','Fitzgerald','Salinger','Collins','Meyer','Lewis','Rowling')
title <- c('Hamlet','A Farewell to Arms','Slaughterhouse 5','The Metamorphosis','The Great Gatsby','Catcher in the Rye','The Hunger Games','Twilight','Chronicles of Narnia','Harry Potter')
publishyr <- c(1892,1929,1969,1915,1925,1951,2008,2005,1950,1997)
stocknumber <- c(10,2,3,7,8,1,2,9,11,8)
price <- c(12,15,13,10,13,8,12,10,11,14)

#make a table of book information
books <- data.frame(author,title,publishyr,stocknumber,price)

library(dplyr)

#sort the books by descending publication date, then ascending author name if two share a publication date
bookssorted <- books %>% arrange(-publishyr,author)

#25% off on books published after 1960, 40% off on books w/ more than 7 in stock, 50% off books that fulfill both
sale <- bookssorted %>% mutate(saleprice=ifelse(publishyr<1960 & stocknumber>7,price*.5,ifelse(publishyr<1960,price*.75,ifelse(stocknumber>7,price*.6,price))))

#subset by rows
salesub <- sale %>% filter(publishyr<1960 | stocknumber>7)

#subset by columns
finaltable <- salesub %>% select(author,title,saleprice)