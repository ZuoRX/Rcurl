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


# # *表示匹配任何元素节点。（通配符）
# # @*表示匹配任何属性值。
# # node()表示匹配任何类型的节点。   txt（）：匹配节点后的文字
# # 多个并列路径
# a<- getNodeSet(doc,"//book/title | //book/price")    #|或符号

myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="UTF-8"
)
url<-"http://news.windin.com/ns/bulletin.php?code=64CF5B21B73D&id=95604912&type=1"
temp<-getURL(url,header="myheader")  
temp<-gettext(temp)

web1<-read_html(url,encoding="utf-8",options = "HUGE",header="myheader")

#爬取链接
announce<-web1%>%html_nodes(xpath ="//div[@class='contenttext']") %>%html_text()
announce1<-announce[[1]]

announce2<-web1%>%html_nodes(xpath ="text('股东')") %>%html_text()


































































