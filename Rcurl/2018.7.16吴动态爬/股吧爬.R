rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
library(xml2)
library(xlsx)
library(dplyr)
library(readr)
library(stringi)
library(stringr)
library(rvest)
library(tcltk)
library(progress)
library(tidyverse)
library(httr)
library(devtools) 
library(RJSONIO)
library(Rwebdriver)
library(rdom)
library(binman)
library(wdman)
library(Rselenium)
cat("\014") 
#install.packages("XML")

#==========================#
#======主要矛盾：时间======#
#==========================#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  #"Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GBK"
)

dat1<-data.frame()
DAT1<-data.frame()

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 100, clear = FALSE, width= 60)
for (i in 1:100) try({
  i=1
  pb$tick()
  site1<-"http://bbs.jrj.com.cn/gupiao,"
  site2<-i
  # temp<-getURL(str_c(site1,site2),encoding="GBK",header="myheader")%>%gettext()
  # #temp1<-gettext(temp)
  # k2<-strsplit(temp,"\n")[[1]]
  # 
  #导出帖子首页中的参数
  web1<-read_html(str_c(site1,site2),encoding="GBK",options = "HUGE")
  
  #内容或主题
  content<-web1%>%html_nodes(xpath ="//tbody/tr/td/a[@class='acol']") %>% html_text()
  
  #发贴人
  name<-web1%>%html_nodes(xpath ="//tbody/tr/td/a[@name='users']") %>% html_text()
  
  #点击量
  click<-web1%>%html_nodes(xpath ="//tbody/tr/td[position()=3]/font") %>% html_text()
  
  #回复量
  reply<-web1%>%html_nodes(xpath ="//tbody/tr/td[position()=4]/font") %>% html_text()
  
  #不精准的时间t
  t<-web1%>%html_nodes(xpath ="//tbody/tr/td[@class='tc']") %>% html_text()
  
  #========================#
  #导出帖子的url，即网页链接
  url<-web1%>%html_nodes(xpath ="//tbody/tr/td/a[starts-with(@href,'/msg')]") %>% html_attrs()
  url1<-gsub(".html.*$|^.*?/msg,","",url)
  time_url<-paste0("http://bbs.jrj.com.cn/msg,",url1,".html",sep="")
  
  dat1<-data.frame(content,name,click,reply,t,time_url)
  DAT1<-rbind(DAT1,dat1)
  },
  silent = T)

#循环100页，需要6分钟
write.csv(DAT2,"c:/users/lenovo/desktop/DAT2.csv")
#==========================#
#=========再导出时间=======#
#==========================#
DAT1<-read.csv("c:/users/lenovo/desktop/DAT1.csv")

DAT2<-data.frame()
time_url<-DAT1$time_url

remDr <- remoteDriver(browserName = "chrome")
remDr$open()


pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 10, clear = FALSE, width= 60)
for (j in 1:10) try({
  pb$tick()
  #url<-'http://bbs.jrj.com.cn/msg,101275905.html'
  url<-time_url[j]
  
  
  remDr$navigate(url)
  web <- read_html(remDr$getPageSource()[[1]][1])
  #发帖人
  publisher<-web %>% html_nodes(xpath = "//div/p/b/a[@href]") %>%html_text()
  
  #具体到分秒的时间
  time<-web %>% html_nodes(xpath = "//div/p/span[@class='fr']") %>%html_text()
  time1<-time[1]
  
  #阅读量  
  read<-web %>% html_nodes(xpath = "//div/div/span") %>%html_text()
  read1<-gsub("回复\\s+\\d+举报\n\t\t\t\t\t收藏本页 \n\t\t\t\t","",read[1])
  
  #回复
  reply<-gsub("阅读\\s+\\d+|举报\n\t\t\t\t\t收藏本页 \n\t\t\t\t","",read[1])
  
  #主题
  theme<-web %>% html_nodes(xpath = "//div/div/h1/b") %>%html_text()
  
  data<-data.frame(publisher,time1,read1,reply,theme)
  #  DAT2<-DAT2[!duplicated(DAT2[,1]),]   #去除时间里面的重复数据
  DAT2<-rbind(DAT2,data)
  Sys.sleep(0.01)
},
silent = T)




write.csv(DAT1,"c:/users/lenovo/desktop/DAT1.csv")
write.csv(DAT2,"c:/users/lenovo/desktop/DAT2.csv")








