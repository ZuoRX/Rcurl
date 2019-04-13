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
#library(Rselenium)#可能需要直接在右边packages栏目里点

#这个包安装比较艰难
#install.packages("RSelenium")
#install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
#install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
#install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")

#install.packages("RSelenium", repos = "https://github.com/ropensci/RSelenium.git")
# install_github("cpsievert/rdom")
 #install_github(repo= "crubba/RSelenium", username = "crubba")
# install_github(repo = "crubba/Rwebdriver", username = "crubba") 
cat("\014") 
#install.packages("XML")




#==========================#
#=========RSelenium========#
#==========================#

#--------股吧尝试--------#
DAT1<-read.csv("c:/users/lenovo/desktop/wxy/DAT1.csv")
#DAT1<-DAT1[!duplicated(DAT1$url),] 

time_url<-DAT1$url[50000:60000]

remDr <- remoteDriver(browserName = "chrome")
remDr$open()

DAT2<-data.frame()
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 2000, clear = FALSE, width= 60)
for (j in 8000:10000) try({
  pb$tick()
  url<-time_url[j]
  
  remDr$navigate(url)
  web <- read_html(remDr$getPageSource()[[1]][1])
  #web<-read_html()
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
  Sys.sleep(0.02)
  },
  silent = T)

write.csv(DAT2,"c:/users/lenovo/desktop/wxy/data8000_10000.csv")




#------demo北京空气质量-------#
weather.url <- "https://www.aqistudy.cn/historydata/monthdata.php?city=北京" %>% url_escape(reserved = "][!$&'()*+,;=:/?@#")
remDr <- remoteDriver(browserName = "chrome")
remDr$open()

remDr$navigate(weather.url)
table <- remDr$getPageSource()[[1]] %>% read_html(encoding = "UTF-8") %>% html_table(header = FALSE) %>% .[[1]]




#==========================#
#======主要矛盾：时间======#
#==========================#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  #"Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GBK")

dat1<-data.frame()
DAT1<-data.frame()
#4929-5921
#5921-6895
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 100, clear = FALSE, width= 60)
for (i in 4929:5921) try({
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

#==========================#
#=========get请求==========#
#==========================#
#参考网址：https://blog.csdn.net/kMD8d5R/article/details/78737454

Cookie<-"vjuids=-2057b3b08.164994b44c2.0.d88c47b11e039; bdshare_firstime=1531580273321; jrj_uid=1531580273327EdkBAF4Fpi; Hm_lvt_0359dbaa540096117a1ec782fff9c43f=1531704384; jrj_z3_newsid=28018; ASL=17730,zzjjk,71864f6171864f61d24a83c6d24a83c6d24a83c6; JSESSIONID=awW9FOUpAX-4; WT_FPC=id=284f6666a11fd834fcc1531580204117:lv=1531898779091:ss=1531898779091; channelCode=227396R2; ADVS=366c89e64e32f6; Hm_lvt_c77760c990c37624bb0be128c3735c2c=1531741319,1531790702,1531880047,1531898781; Hm_lpvt_c77760c990c37624bb0be128c3735c2c=1531898781; vjlast=1531580204.1531880047.11; Hm_lvt_bb0e0c506811076b16590fe782e7d2a7=1531741319,1531790702,1531880048,1531898782; Hm_lpvt_bb0e0c506811076b16590fe782e7d2a7=1531898782; ADVC=3669a42706001b"

headers<-c('Accept'='application/json, text/javascript, */*; q=0.01',
          'Content-Type'='text/html;charset=GBK',
          'User-Agent'='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36',
          'Referer'='http://bbs.jrj.com.cn/msg,101275905.html',
          'Connection'='keep-alive',
          'cookie'=Cookie)
url<-"http://bbs.jrj.com.cn/board/boardMsgList.jspa?fid=0&cid=1&type=1&num=15&tmp=1531899018523"

louwill<-GET(url,add_headers(.headers = headers))

web<-content(louwill)
publisher<-web %>% html_nodes(xpath = "//div/p/b/a[@href]") %>%html_text()



#------demo猎聘网招聘信息-------#
crawl_one_page <- function(key="data analyst", i = 1) {
  # key 为爬取岗位关键词， i 为页码
  base_url <- "https://www.liepin.com/zhaopin/?&fromSearchBtn=2"
  key <- iconv(key, to = "UTF-8") 
  # 构建URL并加码。注意：别忘了curPage是从0开始的。
  liepin_url <- str_c(base_url, "&key=", key, "&curPage=", (i-1)) %>% URLencode()
  # 由于网站信息可能随时更新，所以下载网页方便验证数据准确性。
  file_name <- str_c("data/", key, i, ".html") #构建文件路径，要事先建一个data文件夹
  download_html(liepin_url, file_name) 
  #下面就是真正从网页上抓取数据了，建议使用SelectorGadget定位信息节点，非常方便。
  lp_html <- read_html(file_name)
  position <- lp_html %>%     #岗位名称
    html_nodes(".job-info h3 a") %>%
    html_text() %>%
    str_trim() # 删除多余的空白
  company <- lp_html %>%      #公司名称
    html_nodes(".company-name a") %>%
    html_text()
  req_info <- lp_html %>%     #岗位要求，例如：12-18万_北京_大专及以上_5年以上
    html_nodes(".condition") %>%
    html_attr("title") # get title attribute
  req_tb<- str_split(req_info, "_", simplify = TRUE) %>% as_tibble()
  colnames(req_tb) <- c("salary", "location", "education", "experience")
  pos_tb <- tibble(position, company) %>% bind_cols(req_tb)
  return(pos_tb)
}
# 本来还想抓取行业信息，但发现有些公司对应两个行业，导致爬取时长度（length）与其他信息不同，
# 无法合并，规划以后想办法解决。

# 之后，构建爬取n个网页的function：
crawl_n_page <- function(key = "data analyst", n = 1){
  pos_lt <- vector("list", n) #事先分配存储空间，有利于提高效率——Hadley大神
  for (i in seq_len(n)) {
    pos_lt[[i]] <- crawl_one_page(key, i) 
    cat("Crawling page:", i, "\tProgress:", (i/n)*100, "%\n") # 显示进度
    Sys.sleep(5)
  }
  pos_tb <- bind_rows(pos_lt) #将list转换为tibble
  return(pos_tb)
}




