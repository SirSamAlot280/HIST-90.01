#data frames
book <- c('Can I Take A Nap Now','I Dont Know Her','When Doves Cry','I Like Turtles', 'I Dont Even Go Here','How To Save A Life','Bills Bills Bills','I Will Always Love You','Game of Loans','Formation')
author <- c('Cindy Ramirez','Mariah Carey','Prince','Ryan Andrews','Regina George','Fray Adams','Matthew Knowles','Whitney Houston','Martin George','Beyonce Knowles-Carter')
ypublished <- c(2015,2001,1984,2007,2005,2006,1974,2010,2011,2016)
nstock <- c(10,16,5,7,15,9,12,2,21,4)
price <- c(7.99,7.99,9.99,4.99,6.99,8.99,9.99,5.99,11.99,10.99)

bookstore <- data.frame(book,author,ypublished,nstock,price)

View(bookstore)

library(dplyr) 

#sort data frame
bookstoresorted <- bookstore %>% arrange(-ypublished)
head(bookstoresorted)

bookstore <- bookstore %>% arrange(-ypublished)
head(bookstore)

bookstore %>% arrange(author)
head(bookstore)

#create new variable for sorted bookstore
bookstore2 <- bookstore %>% arrange(-ypublished,author)
head(bookstore2)

View(bookstore2)

#book sale prices
sale <- bookstore %>% mutate(sale=ifelse(ypublished<2007 & nstock>11,price*.50,ifelse(ypublished<2007,price*.75,ifelse(nstock>11,price*.60,price))))

#subset for books on sale with author, title, & price
sub <- sale %>% filter(ypublished,nstock)
subsub <- sub %>% select(book,author,sale)
head(subsub)

subsub <- sale %>% filter(ypublished<2007 & nstock>11 | ypublished<2007 | nstock>11) %>% select(book,author,sale)
head(subsub)

#create new variable for subset
bookstore3 <- subsub %>% mutate(status = "on-sale")
head(bookstore3)

View(bookstore3)