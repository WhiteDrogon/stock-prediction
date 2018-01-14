#comparitive time series analysis using quandle tidy verse and tibble
#tibble helps us to perform similar operation on multiple groups
#quandl is used for downloading financial data
library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(timetk)
library(forcats)
library(stringr)
library(plyr)
library(stringr)
library(gridExtra)
Quandl.api_key("1dhx7CsQx7_QcjLRW6Co")

#retreive the data from the quandl website using the quandl api
IOB = Quandl("NSE/IOB",collapse="daily",start_date="2016-09-01",type="raw")
ICICI = Quandl("NSE/ICICIBANK",collapse="daily",start_date="2016-09-01",type="raw")
PNB = Quandl("NSE/PNB",collapse="daily",start_date="2016-09-01",type="raw")
SBI =  Quandl("NSE/SBIN",collapse="daily",start_date="2016-09-01",type="raw")
IOB <- cbind(IOB,stock="")
ICICI <- cbind(ICICI,stock="")
PNB <- cbind(PNB,stock="")
SBI <- cbind(SBI,stock="")

IOB$stock <- paste(IOB$stock,"IOB",sep="")
ICICI$stock <- paste(ICICI$stock,"ICICI",sep="")
PNB$stock <- paste(PNB$stock,"PNB",sep = "")
SBI$stock <- paste(SBI$stock,"SBI",sep="")
head(ICICI)

#put all the data under one variable
master_data <-rbind(IOB,ICICI,PNB,SBI)

#convert date to character so we can split the timings 

master_data$Date <- as.character(master_data$Date)

#split the data 

list <- strsplit(master_data$Date, "-")

library(plyr)

master1 <- ldply(list)
colnames(master1) <- c("year","month","day")

master_data <- cbind(master_data,master1)
str(master_data)

master_data$Date <- as.Date(master_data$Date)
#change the scale of the total traded quantity 
master_data$`Total Trade Quantity` <- master_data$`Total Trade Quantity`/100000
#visulaize with ggplot
 ggplot(master_data,aes(factor(stock),Close,color=stock,frame=month)) +
  geom_jitter(aes(size = Close, colour=stock, alpha=.02)) +
  ylim(0,1000)+
  labs(title = "Bank Stock Monthly Prices", x = "Banks", y= "Close Price") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
  theme(legend.position="none")
 
 #visualising the stock prizes of all the banks and IOB is the least the order is
 
#SBI > ICICI > PNB > IOB 

 #group by stock
 
 master_data <- master_data %>%
   tibble::as.tibble()%>%
   group_by(stock)
 
 master_data %>%
   ggplot(aes(x = Date, y = Close, color = stock)) +
   geom_point() +
   labs(title = "Daily Close Price", x = "Month",y="Close Price") +
   facet_wrap(~ stock, ncol = 3, scale = "free_y") +
   scale_fill_tq(fill="green4",theme="light") +
   theme_tq() +
   theme(panel.border = element_blank(),
         panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
         panel.grid.minor = element_blank(),
         axis.line=element_line(colour="black"),
         plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
   theme(legend.position="none")
 
#the daily stock close prize is directly related to the total traded quantity if there is any rapid increase or decrease

#density distribution of deviation of high price from open price 

 master_data_high <- master_data %>% mutate(dev_high=High-Open)
 master_data_low <- master_data %>% mutate(dev_low=Low-Open)
 
 #calculate the weekly average of the high prize
master_data_low_week <- master_data %>%
  tq_transmute(
   select = dev_low,
   FUN = mean,
   mutate_fun = apply.weekly,
   na.rm=TRUE,
   col_rename = "dev_low_mean"
 )
#calculate monthly high prize 
master_data_high_week <- master_data%>%
  tq_transmute(
    select = dev_high,
    na.rm=TRUE,
    FUN = mean,
    mutate_fun = apply.weekly,
    col_rename = "dev_high_mean"
  )

high<-master_data_high_week%>%ggplot(aes(x=dev_high_mean,color=stock))+
  geom_dotplot(binwidth=0.50,aes(fill=stock))+
  xlim(0,10)+
  scale_fill_manual(values=c("#999999", "#E69F00","#CC9933","#99FF00","#CC3399","#FF9933"))+
  labs(title="Distribution of High Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  scale_color_tq(values=c("#999999"))+
  theme_tq()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"))+
  theme(legend.position="none")
