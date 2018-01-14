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

Master_Data$Date<-as.Date(Master_Data$Date)

end<-ymd("2017-09-01")
start<-ymd("2016-09-01")

Master_Data<-Master_Data%>%
  tibble::as_tibble() %>%
  group_by(Stock)

## Visualisation of BBand in ggplot2 

Master_Data%>%filter(Stock=="ICICI"|Stock=="PNB")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")

Master_Data%>%filter(Stock=="Axis"|Stock=="SBI")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")


Master_Data%>%filter(Stock=="Canara"|Stock=="BOB")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")

## Download the data Set
PNB = Quandl("NSE/ICICIBANK",collapse="monthly",start_date="2016-09-01",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="monthly",start_date="2016-09-01",type="raw")

## Convert the PNB & Axis Data Set into df for regression model

PNB_df=PNB
Axis_df=Axis
colnames(PNB_df)<-c("Date","Open","High","Low","Last","Close","TTQ","Turnover")
colnames(Axis_df)<-c("Date","Open","High","Low","Last","Close","TTQ","Turnover")

## Change the scale of Trade quantity

PNB_df$TTQ<-PNB_df$TTQ/100000
Axis_df$TTQ<-Axis_df$TTQ/100000

## Regression models

m1=lm(PNB_df$Close~PNB_df$High+PNB_df$Low+PNB_df$TTQ)

p1.df=as.data.frame(predict(m1,interval="predict"))

m3=lm(Axis_df$Close~Axis_df$High+Axis_df$Low+Axis_df$TTQ)

p3.df=as.data.frame(predict(m3, interval="predict"))

## Forecast using ARIMA to take out the seasonality and cyclic part of the stock

m2=arima(diff(PNB_df$Close),order=c(1,0,0))
m4=arima(diff(Axis_df$Close),order=c(1,0,0))
p2.df=as.data.frame(predict(m2,n.ahead=3))
p4.df=as.data.frame(predict(m4,n.ahead=3))

## Combining the Random and Stock  together

p1.df=p1.df[1:3,]
p1.df$fit=p1.df$fit+p2.df$pred
p3.df=p3.df[1:3,]
p3.df$fit=p3.df$fit+p4.df$pred

## Create the date df for next three months

date<-as.data.frame(as.Date(c("2017-10-31","2017-11-30","2017-12-31")))
colnames(date)=c("date")

## Modify the predict dataset and add "key" variable for PNB

p1.df<-cbind(p1.df,date)
p1.df["Key"]<-"Predicted"
p1.df<-p1.df[,c("date","fit","lwr","upr","Key")]

## Modify the predict dataset for Axis and add variable "Key"

p3.df<-cbind(p3.df,date)
p3.df["Key"]<-"Predicted"
p3.df<-p3.df[,c("date","fit","lwr","upr","Key")]


## Rename the columns
colnames(p1.df)<-c("Date","Close","lwr","upr","Key")
colnames(p3.df)<-c("Date","Close","lwr","upr","Key")

## Modify the PNB_df dataset

PNB_df<-PNB%>%select("Date","Close")
Axis_df<-Axis%>%select("Date","Close")

## Add two variable for confidence interval "lwr" and "upr"
var<-c("lwr","upr")

PNB_df[var]<-NA
Axis_df[var]<-NA

## Add the Key variable for Actual data

PNB_df["Key"]<-"Actual"
Axis_df["Key"]<-"Actual"

## Rbind the predicted and actual value for both of the Stocks

PNB_com=rbind(PNB_df,p1.df)
PNB_com$Date<-as.Date(PNB_com$Date)

Axis_com=rbind(Axis_df,p3.df)
Axis_com$Date<-as.Date(Axis_com$Date)

## Visualisation

PNB_Plot<-ggplot(data=PNB_com,aes(x= Date, y = Close,color=Key,label=Close)) +
  # Prediction intervals
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Key), 
              fill = "khaki2", size = 0)+
  geom_line(size = 1.7) + 
  geom_point(size = 2)+
  labs(title = "Actual and Predicted Price, PNB", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = "honeydew")
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15))

Axis_Plot<- ggplot(data=Axis_com,aes(x= Date, y = Close,color=Key,label=Close)) +
 
  
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Key), 
              fill = "khaki2", size = 0)+
  geom_line(size = 1.7) + 
  geom_point(size = 2)+
  labs(title = "Actual and Predicted Price, Axis Bank", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = "honeydew")
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15))


grid.arrange(PNB_Plot,Axis_Plot,ncol = 1, nrow = 2)