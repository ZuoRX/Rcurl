rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
library(rvest)
library(magrittr)  #管道符
library(tcltk)
library(progress)
library(plyr)

cat("\014") 

urllist=1:24 
i=0
for (year in 2011:2018) {
  for (season in 1:4) {
    i=i+1
    urllist[i]<-paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/000825.phtml?year=" ,
                      year,"&jidu=",season,sep="")
  }
}

DATA<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 32, clear = FALSE, width= 60)

for (j in 1:32) {
  pb$tick()
  
web1<-read_html(urllist[j],encoding="gb2312",options = "HUGE")

#解析时间
time<-web1%>%html_nodes(xpath ="//table/tr/td[1]/div") %>% html_text()
time1<-gsub("\r\n|\t","",time)


#--开盘价--#
open<-web1%>%html_nodes(xpath ="//table/tr/td[2]/div") %>% html_text()


#--最高价--#
high<-web1%>%html_nodes(xpath ="//table/tr/td[3]/div") %>% html_text()

#--收盘价--#
close<-web1%>%html_nodes(xpath ="//table/tr/td[4]/div") %>% html_text()


#--最低价--#
low<-web1%>%html_nodes(xpath ="//table/tr/td[5]/div") %>% html_text()

# #--交易量--#
volume<-web1%>%html_nodes(xpath ="//table/tr/td[6]/div") %>% html_text()

# #--交易金额--#
money<-web1%>%html_nodes(xpath ="//table/tr/td[7]/div") %>% html_text()

data<-data.frame(time1,open,high,close,low,volume,money)

data1<-arrange(data,time1)

DATA<-rbind(DATA,data1)
}

DATA$time1[DATA$time1=="日期"]<-NA

d<-na.omit(DATA)

d1<-arrange(d,desc(time1))

d2<-d1[-1:-4,]
write.csv(d2,"c:/users/lenovo/desktop/stock/steel/000825_volumn.csv")
