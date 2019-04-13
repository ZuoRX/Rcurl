rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
cat("\014") 

myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)

#-----------------------批量处理-------------------------------------------#
rm(list=ls())
#第一步，得到所有url， 即翻页处理
urllist=1:24 
i=0
for (year in 2011:2018) {
  for (season in 1:4) {
    i=i+1
    urllist[i]<-paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/000825.phtml?year=" ,
                      year,"&jidu=",season,sep="")
  }
}
urllist
#第二步，实验j=1时，导出time数据
#实际做的时候，先不循环，做一个，便于确定数据的位置
j=2
for (j in 1:32) {
  j=j+1
  temp<-getURL(urllist[j])
  k=strsplit(temp,"\r\n")[[1]]  #关键定位
  k
  grep("<a target='_blank'",k)
  
  #解析时间
  time1=k[grep("<a target='_blank'",k)+1]   #grep返回匹配的编码   +1后一行
  time1
  time<-substring(time1,4,13)
  as.data.frame(time)
  #write.table(time,"c:/users/lenovo/desktop/time.csv",append = TRUE,sep=",",col.names = F)
  
  
  #--开盘价--#
  openprice1=k[grep("<a target='_blank'",k)+3]
  openprice2<-gregexpr(">\\d+",openprice1)   # 只是用来确定位置，并不参与下一步运算程序
  openprice<-substring(openprice1,28,33)
  #write.table(openprice,"c:/users/lenovo/desktop/openprice.csv",append = TRUE,sep=",",col.names = F)

  #--收盘价--#
  closeprice1=k[grep("<a target='_blank'",k)+5]     
  closeprice2<-gregexpr(">\\d+",closeprice1)              
  closeprice2   #39指>的位置
  closeprice<-substring(closeprice1,28,33)
  closeprice
  
  #--最高价--#
  highprice1=k[grep("<a target='_blank'",k)+4]     
  highprice2<-gregexpr(">\\d+",highprice1)               
  highprice2   #27指>的位置
  highprice<-substring(highprice1,28,33)
  highprice
  
  #--最低价--#
  lowprice1=k[grep("<a target='_blank'",k)+6]     
  lowprice2<-gregexpr(">\\d+",lowprice1)              
  #lowprice2   #27指>的位置
  lowprice<-substring(lowprice1,40,45)  #数值要改
  #lowprice
  
  # #--交易量--#
  # volume1=k[grep("<a target='_blank'",k)+7]     
  # volume2<-gregexpr(">\\d+",volume1)              
  # volume2   #27指>的位置
  # volume<-substring(volume1,40,47)  #数值要改
  # volume
  
  #--交易量方法二--#
  volume1=k[grep("<a target='_blank'",k)+7] 
  volume1
  volume2<-gsub("\t\t\t<td class=\"tdr\"><div align=\"center\">","",volume1)
  volume3<-gsub("</div></td>","",volume2)
  volume<-as.data.frame(volume3)
  volume
  
  # #--交易金额--#
  # money1=k[grep("<a target='_blank'",k)+8] 
  # money1
  # money2<-gregexpr(">\\d+",money1)              
  # money2   #27指>的位置
  # money<-substring(money1,40,49)  #数值要改
  # money
  
  #------------交易金额方法二------#
  #优点：提取的数值长度可以不一致，避免过渡提取（字符）
  #缺点：格式必须一致
  money1=k[grep("<a target='_blank'",k)+8] 
  money3<-gsub("\t\t\t<td class=\"tdr\"><div align=\"center\">","",money1)
  money4<-gsub("</div></td>","",money3)
  money<-as.data.frame(money4)
  
  
  # 合并数据框
  Ren <- data.frame(time,openprice,highprice,closeprice,lowprice,volume,money)
  names(Ren) = c("time", "openprice","highprice","closeprice",
                 "lowprice","volume","money")
  write.table(Ren,"c:/users/lenovo/desktop/Ren.csv",append = TRUE,
              sep=",",col.names = F,row.names =T)
  #列名自己加，因为col.names=T会循环产生列名,导致多余的列名
}






#------------------正则匹配------------------------------#
#分步做
rm(list = ls())
temp <-getURL("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/603000.phtml?year=2018&jidu=1", 
              httpheader=myheader,
              encoding="“UTF-8")
k<-strsplit(temp,"\r\n")[[1]]  #一般网页切割
grep("<a target='_blank'",k)        #类似每组的一个定位
k[948]
#-----导出时间----#
time1=k[grep("<a target='_blank'",k)+1]   #grep返回匹配的编码   +1后一行
time1
time<-substring(time1,4,13)
time



#------------------xpath导向------------------------------#
rm(list=ls() )
temp <-getURL("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/603000.phtml?year=2018&jidu=1", 
              httpheader=myheader,
              encoding="“UTF-8")   
doc<-htmlTreeParse(temp)               # 解析网页格式
#doc1<-xmlParse(temp)                  # xml格式
a<-getNodeSet(doc,'//tr/td/div')       #路径不对容易报错          
# 多个并列路径
a<- getNodeSet(doc,"//book/title | //book/price")    #|或符号
write.csv(a,"C:/users/lenovo/desktop/a.csv")





































