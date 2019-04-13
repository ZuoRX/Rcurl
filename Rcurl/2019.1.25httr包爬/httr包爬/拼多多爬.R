library(httr)
library(rvest)
library(tcltk)
library(progress)

#传入拼多多cookie信息
#Cookie <- "RK=GMVWyK8bld; tvfe_boss_uuid=db2557df6d970ec9; pac_uid=1_2286747332; pvid=7628586552; flv=25.0 r0; AMCV_248F210755B762187F000101%40AdobeOrg=-1891778711%7CMCIDTS%7C17734%7CMCMID%7C12409515081908024712323869212451902083%7CMCAAMLH-1526914286%7C11%7CMCAAMB-1532176839%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1532184061s%7CNONE%7CMCSYNCSOP%7C411-17673%7CMCAID%7C2D7612D0052A1F0F-6000012A8003D0FE%7CvVersion%7C2.4.0; o_cookie=2286747332; ptcz=c7f6c780828590a58d25ff4cce09508d880dc697d929b1ec4e011c039abc4177; pgv_pvi=5668365312; pgv_pvid=7628586552; mm_lang=zh_CN; MM_WX_NOTIFY_STATE=1; MM_WX_SOUND_STATE=1; wxuin=1251823703; webwxuvid=c7197fcb004df4537947fc2aa813d811f98a8bc81076146c35c99a41d9a2eaac1d3157f4aeabc1874c45606c613fc287; last_wxuin=1251823703; wxsid=9Ri1td7syPCp7Er6; webwx_data_ticket=gSfgEmuSggcaQmsxGD1hHtTj; webwx_auth_ticket=CIsBEMSo6oEEGoABPtdOhv3h6CiHAh//Uvot+g7PalWG7hioU96QHv5sAA92vqdT1SS7ZsjkMUR3DgTtezIPvHF8ub7TgjeHsCoGjmj0oWKfqfqex/pAH9en3tRK7Oe/zCFO7nZeWlVh8lwOvpDVBvjDEJyODFlS8v/dothQE5L9HVxDtO60lQhwoPY=; login_frequency=2; wxloadtime=1547456504_expired; wxpluginkey=1547455561"
#构造请求头
headers <- c('Accept'='*/*',
             'Content-Type'='text/plain', 
             'User-Agent'='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36',
             'Referer'='http://mobile.yangkeduo.com/goods.html?goods_id=4626899036&thumbnail=%2F%2Ft00img.yangkeduo.com%2Fgoods%2Fimages%2F2018-12-24%2F48e447f8bb2d871876589c379f64f419.jpeg%3FimageMogr2%2Fsharpen%2F1%257CimageView2%2F2%2Fw%2F1300%2Fq%2F70%2Fformat%2Fwebp&refer_page_name=search_result&refer_page_id=10015_1548409047344_W2E0g3go9j&refer_page_sn=10015',
             'Connection'='keep-alive'
)

#实际请求的url
url<-"http://apiv3.yangkeduo.com/recommendation?goods_id=4626899036&referrer=goods_with_mall_rec&list_id=rec_list_54M0MK&page=1&app_name=goods_detail&pdduid=0"

#执行请求
pinduoduo <- GET(url,add_headers(.headers =headers))
pin<-content(pinduoduo)

temp<-gettext(pin)
temp[[2]]
#temp1<-gettext(temp)
k2<-strsplit(temp,"\n")[[1]]
head(k2,100)

#微信昵称
name<-k2[grep(pattern="goods_id",temp)]
name<-gsub("\"NickName\": \"|\",","",name)

#备注姓名
label<-k2[grep(pattern="RemarkName",k2)]
label<-gsub("\"RemarkName\": \"|\",","",label)

label1<-k2[grep(pattern="RemarkPYInitial",k2)]
label1<-gsub("\"RemarkPYInitial\": \"|\",","",label1)

label2<-k2[grep(pattern="RemarkPYQuanPin",k2)]
label2<-gsub("\"RemarkPYQuanPin\": \"|\",","",label2)


#性别
sex<-k2[grep(pattern="Sex",k2)]
sex<-gsub("\"Sex\": |,","",sex)

#省份
province<-k2[grep(pattern="Province",k2)]
province<-gsub("\"Province\": \"|\",","",province)

#城市
city<-k2[grep(pattern="City",k2)]
city<-gsub("\"City\": \"|\",","",city)

#签名
signature<-k2[grep(pattern="Signature",k2)]
signature<-gsub("\"Signature\": \"|\",","",signature)


wei<-data.frame(name,label,label1,label2,sex,province,city,signature)
write.csv(wei,"c:/users/lenovo/desktop/weixin.csv")


#http://mobile.yangkeduo.com/search_result.html?search_key=iphone&search_src=new&search_met=btn_sort&search_met_track=manual&refer_page_name=search&refer_page_id=10031_1548409039361_1w3rQv0n3W&refer_page_sn=10031&page_id=10015_1548409047344_W2E0g3go9j&list_id=Y28n4qMACM&flip=20%3B1%3B0%3B0%3Be38b1248-39f3-4ecb-bc37-fef6dda0d83a



rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
library(xml2)
library(xlsx)
library(magrittr)
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
library("RSelenium", lib.loc="D:/R-3.5.1/library")





#==========================#
#=========RSelenium========#
#==========================#

#--------股吧尝试--------#
url<-'http://mobile.yangkeduo.com/search_result.html?search_key=iphone&search_src=new&search_met=btn_sort&search_met_track=manual&refer_page_name=search&refer_page_id=10031_1548409039361_1w3rQv0n3W&refer_page_sn=10031&page_id=10015_1548409047344_W2E0g3go9j&list_id=Y28n4qMACM&flip=20%3B1%3B0%3B0%3Be38b1248-39f3-4ecb-bc37-fef6dda0d83a'

remDr <- remoteDriver(browserName = "chrome")
remDr$open()

  
  remDr$navigate(url)
  web <- read_html(remDr$getPageSource()[[1]][1])
  #web<-read_html()
  #发帖人
  name<-web %>% html_nodes(xpath = "//div/div]") %>% html_text()
  
  #具体到分秒的时间
  time<-web %>% html_nodes(xpath = "//div/p/span[@class='fr']") %>%html_text()
  time1<-time[1]
  









