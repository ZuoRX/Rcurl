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


url<-"http://vip.stock.finance.sina.com.cn/moneyflow/#!ssfx!sz000800"

remDr <- remoteDriver(browserName = "chrome")
remDr$open()


pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 100, clear = FALSE, width= 60)
DAT<-data.frame()


remDr$navigate(url)
web <- read_html(remDr$getPageSource()[[1]][1])
web[1]




