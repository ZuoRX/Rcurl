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
#install.packages("curl")
#install.packages("devtools")
library(devtools)
install_github("xml2")

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
for (i in 1:50) try({
  pb$tick()
  site1<-"http://bbs.jrj.com.cn/gupiao,"
  site2<-i
  temp<-getURL(str_c(site1,site2),encoding="GBK",header="myheader")
  #temp1<-gettext(temp)
  k2<-strsplit(temp,"\n")[[1]]
  
  #导出帖子首页中的参数
  web1<-read_html(str_c(site1,site2),encoding="GBK",options = "HUGE")
  
  #内容或主题
  content<-web1 %>% html_nodes(xpath ="//tbody/tr/td/a[@class='acol']") %>% html_text()
  
  #发贴人
  name<-web1 %>% html_nodes(xpath ="//tbody/tr/td/a[@name='users']") %>% html_text()
  
  #点击量
  click<-web1 %>% html_nodes(xpath ="//tbody/tr/td[position()=3]/font") %>% html_text()
  
  #回复量
  reply<-web1 %>% html_nodes(xpath ="//tbody/tr/td[position()=4]/font") %>% html_text()
  
  #不精准的时间t
  t<-web1%>%html_nodes(xpath ="//tbody/tr/td[@class='tc']") %>% html_text()
  
  #========================#
  #导出帖子的url，即网页链接
  url<-web1 %>% html_nodes(xpath ="//tbody/tr/td/a[starts-with(@href,'/msg')]") %>% html_attrs()
  url1<-gsub(".html.*$|^.*?/msg,","",url)
  time_url<-paste0("http://bbs.jrj.com.cn/msg,",url1,".html",sep="")
  
  dat1<-data.frame(content,name,click,reply,t,time_url)
  DAT1<-rbind(DAT1,dat1)
},
silent = T)

#循环100页，需要4-6分钟

#==========================#
#=========再导出时间=======#
#==========================#
DAT1<-read.csv("c:/users/lenovo/desktop/DAT1.csv")
names(DAT1)
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 7322, clear = FALSE, width= 60)
DAT2<-data.frame()
time_url<-as.character(DAT1$time_url)
#time_url[1:20]
#点击每个帖子，导出其中的时间
for (j in 15:73){
  
  j=as.numeric(j)
  #pb$tick()
  web<-read_html(time_url[j],encoding="GBK",options ="HUGE")
  
  #发帖人
  publisher<-web %>% html_nodes(xpath = "//div/p/b/a[@href]") %>%html_text()
  
  #具体到分秒的时间
  time<-web %>% html_nodes(xpath = "//div/p/span[@class='fr']") %>%html_text()
  time1<-time[1]
  
  data<-data.frame(time1,publisher)
  #DAT2<-DAT2[!duplicated(DAT2[,1]),]   #去除时间里面的重复数据
  DAT2<-rbind(DAT2,data)
  Sys.sleep(5)
}


write.csv(DAT1,"c:/users/lenovo/desktop/DAT1.csv")
write.csv(dat2,"c:/users/lenovo/desktop/dat2.csv")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||#
#.....................................................#
#nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn#
#UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU#
#TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT#
#MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#
#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV#
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX#
#IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#*****************************************************#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++#
#=====================================================#
#traceback()  #查找错误

write.table(t1,"C:/Users/lenovo/Desktop/time.csv",
            append = TRUE,sep=",",col.names = F,row.names =T)
#?ϲ???ʽ
# else{t<-t}
# ifelse(t[1]=="",t1<-t[2],t1<-t[1])
#tt<-t[1]
#publish_time<-rbind(publish_time,t1)



publish_time<-data.frame()
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 103, clear = FALSE, width= 60)
for (j in 1:length(time_url)) {
  pb$tick()
temp1<-time_url[j]%>%getURL()%>%gettext()
k2<-strsplit(temp1,"\n")[[1]]
time=k2[grep("<div class=\"author\">",k2)+5]
time1<-gsub("</span></p>\r","",time)
time2<-gsub("^.*?<c6>ú￡o","",time1)%>%as.data.frame()  #中文乱码问题
publish_time<-rbind(publish_time,time2)
Sys.sleep(0.05)
}


#-------------#
#---rvest??---#
#-------------#
stocktalk<-data.frame()
for (i in 1:10) {
  i=1
  site1<-"http://bbs.jrj.com.cn/gupiao,"
  site2<-i
  web<-read_html(str_c(site1,site2),encoding="GBK")
  pp<-web %>% html_nodes("tbody") %>%html_text()
  M<-matrix(pp,nrow=105)
  M<-M[-1,]
  p<-gsub("^\\s+|\\s+$","",M) 
  
  theme<-gsub("\r\n\t\t\t\t.*$","",p)
  
  publisher<-web %>% html_nodes(xpath = "//tbody//td[@align]") %>%html_text()
  
  tick<-web %>% html_nodes(xpath = "//tbody//td[@font]") %>%html_text()
  
  M<-as.data.frame(M)
  P<-t(M)
  M[1:5]
  P<-gsub("^\\s+|\\s+$","",P) #????????ʽ^??ʾ??ʼλ?ã?$??ʾĩβ??+??ʾ????һ??
  P<-P[-1,]
  #P<-as.data.frame(P)
  stocktalk<-rbind(stocktalk,P)
}
url<-"http://bbs.jrj.com.cn/gupiao,1"
web<-read_html(url,encoding="GBK") #??ȡ???ݣ??涨????

position<-web %>% html_nodes("tbody") %>% html_text()
position[1:10]




#ԭ��????????ƥ??????ʱ??
# pattern<-"[\u4e00-\u9fa5]+: /^[1-9]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])\\s+(20|21|22|23|[0-1]\\d):[0-5]\\d:[0-5]\\d$/"
# tt<-regexec(pattern,t)





for (j in 1:length(time_url)) {
  j=2
  web<-read_html(time_url[j])
  t<-web %>% html_nodes(xpath = "//div//p//span[@class]") %>%html_text()
  if(t[1]==""){
    t1<-t[2]
  } else{
    t1<-t[1]
  }
  write.table(t1,"C:/Users/lenovo/Desktop/time.csv",
              append = TRUE,sep=",",col.names = F,row.names =T)
  
}


#----------------------------------#
install.packages("installr")
library(installr)
updateR()





library(rvest)
library(stringr)
#先提取7大类网址
URL<-"http://www.imooc.com" # 慕课网网址
web<-read_html(URL,encoding="utf-8")
site<-web%>%html_nodes('div.itema')%>%html_attrs()

for(i in 1:(length(site))){
  ifelse(i==length(site),site[i]<-site[[i]][2],site[i]<-site[[i]][1])
  WEB<-paste0(URL,site)}
#用WEB来储存七大类网址信息


DAT<-data.frame()#建立空的数据框，然后用rbind来封装数据（比较常用）
for(j in 1:length(WEB)){
  url<-WEB[j]
  web<-read_html(url)
  course1<-web%>%html_nodes("div.course-card-contenth3")%>%html_text()
  #判断每一页案列数目，判定是否需要翻页
  
 if(length(course1)>=22){
  page<-web%>%html_nodes('div.course-list div.page a')%>%html_attrs()
  #字符串处理提取页码，其中y就是页码数（需要as.numeric()处理）
  x<-page[[length(page)]][1]
  x<-str_split_fixed(x,"page=",2)
  y<-x[,2]
  y<-as.numeric(y)
  
  dat<-data.frame()
  for(i in 1:y){  
    url1<-"&page="
    #连接形成每一页的网址#网址大的连接
    web2<-paste0(url,url1,i)
    Site<-read_html(web2)
    #提取每一页的文本信息
    course<-Site%>%html_nodes("div.course-card-contenth3")%>%html_text()
    text<-Site%>%html_nodes("div.course-card-infospan")%>%html_text()
    rank<-text[seq(1,length(text),2)]
    num<-text[seq(2,length(text),2)]
    note1<-Site%>%html_nodes("div.clearfixp")%>%html_text()
    note<-note1[-c(1,length(note1))]
    #data装入数据框
    data<-data.frame(course,rank,num,note)
    #将循环的数据封装
    dat<-rbind(dat,data)} }
  
  else{
    Site<-read_html(url)
    #提取每一页的文本信息
    course<-Site%>%html_nodes("div.course-card-contenth3")%>%html_text()
    text<-Site%>%html_nodes("div.course-card-infospan")%>%html_text()
    rank<-text[seq(1,length(text),2)]
    num<-text[seq(2,length(text),2)]
    note1<-Site%>%html_nodes("div.clearfixp")%>%html_text()
    note<-note1[-c(1,length(note1))]
    #data装入数据框
    dat<-data.frame()
    data<-data.frame(course,rank,num,note)
    #将循环的数据封装
    dat<-rbind(dat,data)
    dat
  }
  DAT<-rbind(DAT,dat)
}
#设置最大输出行数
options("max.print"=10000)
print(DAT)
#保存文件
write.csv(DAT,file="DAT.csv")




