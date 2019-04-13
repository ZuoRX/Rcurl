rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
library(xlsx)
library(dplyr)
library(readr)
library(stringi)
library(rvest)
library(tcltk)
library(progress)
#动态爬虫
library(RJSONIO)
library(Rwebdriver)
library(rdom)
library(binman)
library(wdman)
library("RSelenium", lib.loc="D:/R-3.5.1/library")
cat("\014") 


#-------------------------------------#
#----------第一步，爬评论链接---------#
#-------------------------------------#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="UTF-8"
)
url<-"https://futures.cngold.org/xiangjiao/list_2523_all.html"
temp<-getURL(url,header="myheader")  
temp<-gettext(temp)

web1<-read_html(url,encoding="utf-8",options = "HUGE")

#爬取链接
url1<-web1%>%html_nodes(xpath ="//div/ul/li/a") %>%html_attrs()
url2<-as.vector(unlist(url1))
url2<-url2[162:1723]
url3<-url2[1:1499]
#爬取对应时间
time<-web1%>%html_nodes(xpath ="//div/ul/li/a") %>% html_text()
time1<-time[58:1577]
time2<-time1[1:1499]

dat1<-data.frame(time2,url3)
write.csv(dat1,"c:/users/lenovo/desktop/fan/time_url.csv")

#-------------------------------------#
#----------第二步，爬评论头部---------#
#-------------------------------------#
rm(list=ls())
url<-read.csv("c:/users/lenovo/desktop/fan/time_url.csv")
url1<-as.character(url$url3)

pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 1499, clear = FALSE, width= 60)
DAT<-data.frame()

for (j in 1:length(url1)) try({
  pb$tick()
  url1[j]
  web<-read_html(url1[j],encoding="utf-8",options = "HUGE")
  
  #时间
  time<-web %>% html_nodes(xpath = "//div/div/div/ul/li/span[@class='fr']") %>%html_text()
  
  #作者
  author<-web %>% html_nodes(xpath = "//div/div/div/ul/li/span[@class='fr mr10']") %>%html_text()
  
  #主题内容
  #theme<-web %>% html_nodes(xpath = "//div/div/div/ul/li/a[@target='_blank']") %>%html_text()
  temp=read_html(url1[j])
  temp<-gettext(temp)
  k<-strsplit(temp,"\n")[[1]]
 
  theme=k[grep("<span class=\"fr mr10\">",k)+1]  
  theme1<-gsub( ".*blank\">","",theme)
  theme2<-gsub("</a>\r","",theme1)
  
  #链接
  #url_1<-web %>% html_nodes(xpath = "//div/div/div/ul/li/a") %>%html_attrs()
  url_1<-k[grep("<span class=\"fr mr10\">",k)+1]
  url_2<-gsub( "\" title.*","",url_1)
  url_3<-gsub( "\t\t\t\t\t<a href=\"","",url_2)
  
  data<-data.frame(time,author,theme2,url_3)
  #DAT2<-DAT2[!duplicated(DAT2[,1]),]   #去除时间里面的重复数据
  DAT<-rbind(DAT,data)
  Sys.sleep(0.1)
},
silent = T)

write.csv(DAT,"c:/users/lenovo/desktop/fan/url1_text.csv")

#-------------------------------------#
#----------第三步，爬评论摘要---------#
#-------------------------------------#
url<-read.csv("c:/users/lenovo/desktop/fan/url1_text.csv")
url1<-as.character(url$url_3)
url1<-gsub("<a href=\"","",url1)

remDr <- remoteDriver(browserName = "chrome")
remDr$open()


pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 100, clear = FALSE, width= 60)
DAT<-data.frame()

for (j in 1:100) try({
  
  remDr$navigate(url1[j])
  web <- read_html(remDr$getPageSource()[[1]][1])
  pb$tick()
  
  #web<-read_html(url1[j],encoding="utf-8",options = "HUGE")
  
  #时间
  time<-web %>% html_nodes(xpath = "//div/div/div/span") %>%html_text()
  time1<-time[grep("\\d+-\\d",time)]
  
  #摘要内容
  abstract<-web %>% html_nodes(xpath = "//div/div/div/p") %>%html_text()
  abstract1<-abstract[1]
  
  #全文内容   （可以选择性注释）
  text<-web %>% html_nodes(xpath = "//div/div/div/p") %>%html_text()
  text1<-text[-1]
  #思路一：定位“\r\n\t”,然后删除后面的文本
  #which(text1=="下一篇>已是最后一篇" )
  text2<-text1[1:which(text1=="下一篇>已是最后一篇" )-1]
  text3<-text2[-length(text2)]
  text4<-paste(text3,collapse = "")
 
  # #思路二：文本合并后，字符串处理
  # text1<-cat(text1)
  # text4<-gsub("   .*","",text1)       实验不行
  
  data<-data.frame(time1,abstract1,text4)
 
  DAT<-rbind(DAT,data)
  Sys.sleep(0.1)
},
silent = T)

write.csv(DAT,"c:/users/lenovo/desktop/fan/abstract.csv")


















