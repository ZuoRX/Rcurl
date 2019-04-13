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
  pb$tick()
  site1<-"http://bbs.jrj.com.cn/gupiao,"
  site2<-i
  temp<-getURL(str_c(site1,site2),encoding="GBK",header="myheader")%>%gettext()
  #temp1<-gettext(temp)
  k2<-strsplit(temp,"\n")[[1]]
  
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

#==========================#
#=========再导出时间=======#
#==========================#
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 7322, clear = FALSE, width= 60)
DAT2<-data.frame()
time_url<-DAT1$time_url
#time_url[1:20]
#点击每个帖子，导出其中的时间
for (j in 1:length(time_url)) try({
  pb$tick()
  web<-read_html(time_url[j],encoding="GBK",options = "HUGE")
  
  #发帖人
  publisher<-web %>% html_nodes(xpath = "//div/p/b/a[@href]") %>%html_text()
  
  #具体到分秒的时间
  time<-web %>% html_nodes(xpath = "//div/p/span[@class='fr']") %>%html_text()
  time1<-time[1]
  
  data<-data.frame(time1,publisher)
  #DAT2<-DAT2[!duplicated(DAT2[,1]),]   #去除时间里面的重复数据
  DAT2<-rbind(DAT2,data)
  Sys.sleep(0.1)
  },
  silent = T)


write.csv(DAT1,"c:/users/lenovo/desktop/DAT1.csv")
write.csv(dat2,"c:/users/lenovo/desktop/dat2.csv")

