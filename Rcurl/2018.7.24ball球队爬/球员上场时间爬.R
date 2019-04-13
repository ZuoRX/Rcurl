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
cat("\014") 

#--------------------------#
#-------循环简洁版本-------#
#--------------------------#




#-------------------------#
#-----------第一步--------#
#-------------------------#
#整理所有队员薪酬，并统计球队上场球员总数，并--编号--

#-------------------------#
#-----------第二步--------#
#-------------------------#

#----------------先抓取每场比赛的url---------------#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="UTF-8"
)
url<-"http://www.stat-nba.com/query_team.php?crtcol=date_out&order=0&QueryType=game&GameType=season&Team_id=LAL&PageNum=1000&Season0=2016&Season1=2017"
temp<-getURL(url,header="myheader")  
temp<-gettext(temp)
k2<-strsplit(temp,"\n")[[1]]
#k2[1:20]  提取其他球队的时候，可以需要修改下面的代码数值
k3<-k2[-c(1:11)]
#grep("<tr>",k3)
pk=k3[grep("<tr>",k3)+6]  
url1<-gsub("<td class=\"normal result_out change_color col5 row\\d+\">","",pk)
url2<-gsub("[\u4e00-\u9fa5]\\w","",url1)
url3<-gsub("' target='_blank'>\\S+","",url2)  
url4<-gsub("<a href='.","",url3)
url5<-paste("http://www.stat-nba.com",url4,sep="")
url5<-url5[-1]
#url5[1]
remove("k2","k3","myheader","pk","temp","url","url1","url2","url3","url4")

#----------------再抓取每场比赛球员的上场时间---------------#

#第二步，实验j=1时，导出time数据
#实际做的时候，先不循环，做一个，便于确定数据的位置
#j=2
for (j in 1:82) {
 #urla<-"http://www.stat-nba.com/game/38937.html"
 tempa=read_html(url5[j])
 tempa<-gettext(tempa)
 kk2<-strsplit(tempa,"\n")[[1]]
 #kk2[1:20]

 #----上场队员姓名-----#
 name=kk2[grep("<tr class=\"sort\">",kk2)+2]  
 name1<-gsub( "[^\u4e00-\u9fa5]","",name)
 name1[name1=="洛尔邓"]<-1
 name1[name1=="季莫费莫兹戈夫"]<-2
 name1[name1=="乔丹克拉克森"]<-3
 name1[name1=="何塞卡尔德隆"]<-4
 name1[name1=="科里布鲁尔"]<-5
 name1[name1=="塔里克布莱克"]<-6
 name1[name1=="尼克杨"]<-7
 name1[name1=="德安吉洛拉塞尔"]<-8
 name1[name1=="布兰顿英格拉姆"]<-9
 name1[name1=="朱利叶斯兰德尔"]<-10
 name1[name1=="泰勒恩尼斯"]<-11
 name1[name1=="梅塔沃尔德皮斯"]<-12
 name1[name1=="拉里南斯"]<-13
 name1[name1=="托马斯罗宾逊"]<-14
 name1[name1=="伊维察祖巴茨"]<-15
 name1[name1=="大卫恩瓦巴"]<-16
 name1[name1=="路易斯威廉姆斯"]<-17
 name1[name1=="马赛洛赫尔塔斯"]<-18
 
 name2<-gsub("\\D+","-1",name1)
 name2<-as.numeric(name2)
 name2
 # name3<-name2[which(name2>=0)]
 # name3
 #---队员上场时间-----#
 time=kk2[grep("<tr class=\"sort\">",kk2)+4]
 time1<-gsub("<td class=\"normal mp change_color col2 row","",time)
 time2<-gsub("\\d\" rank=\"","",time1)
 time3<-gsub("\\d+\">","",time2)
 time4<-gsub("</td>","",time3)
 remove("time1","time2","time3")
 time4
 
 #----构建场次记录----#
 game<-rep(j,length(time4))
 
 #---- 合并数据框----#
 teamdata<- data.frame(game,name2,time4)
 teamdata<-teamdata[-23,]
 #teamdata1<-t(teamdata)
 teamdata1<-teamdata[which(teamdata$name2>=0),] #这个，不能忽视
 
 write.table(teamdata1,"c:/users/lenovo/desktop/teamdata.csv",append = TRUE,
            sep=",",col.names = F,row.names =T)
 # u <- 1:2000  
 # #开启进度条  
 # pb <- tkProgressBar("进度","已完成 %", 0, 100)  
 # for(i in u) {  
 #   info<- sprintf("已完成 %d%%", round(i*100/length(u)))  
 #   setTkProgressBar(pb, i*100/length(u), sprintf("进度 (%s)", info),info)  
 # }   
 # #关闭进度条  
 # close(pb) 
}

#-------------------------#
#-----------第三步--------#
#-------------------------#
#先要对爬虫数据进行排序处理

time_HuRen<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/time_HuRen.csv",1)
#----------把球员补全---------#
#写个循环
#18*82=1476
a1<-rep(1:82,each=18)
a2<-rep(1:18,82)
a3<-rep(0,1476)
l<-data.frame(a1,a2,a3)
#l<-matrix(rep(0,1476*3),ncol = 3)
for(j in 1:1476){#1476
  for(i in 1:873){
    if(l[j,2]==time_HuRen[i,2] &l[j,1]==time_HuRen[i,1] ){
      l[j,3]<-time_HuRen[i,3]
  }
 }
}

#-------------------------#
#-----------第四步--------#
#-------------------------#



#--把球员的薪酬加上-----#
l$s<-rep(0,1476)
l$s[l$a2==1 & l$a3 >0]<-1800
l$s[l$a2==2 & l$a3 >0]<-1600
l$s[l$a2==3 & l$a3 >0]<-1250
l$s[l$a2==4 & l$a3 >0]<-771
l$s[l$a2==5 & l$a3 >0]<-760
l$s[l$a2==6 & l$a3 >0]<-619
l$s[l$a2==7 & l$a3 >0]<-544
l$s[l$a2==8 & l$a3 >0]<-533
l$s[l$a2==9 & l$a3 >0]<-528
l$s[l$a2==10 & l$a3 >0]<-327
l$s[l$a2==11 & l$a3 >0]<-173
l$s[l$a2==12 & l$a3 >0]<-155
l$s[l$a2==13 & l$a3 >0]<-121
l$s[l$a2==14 & l$a3 >0]<-105
l$s[l$a2==15 & l$a3 >0]<-103
l$s[l$a2==16 & l$a3 >0]<-14
l$s[l$a2==17 & l$a3 >0]<-40
l$s[l$a2==18 & l$a3 >0]<-155
write.csv(l,"C:/Users/lenovo/Desktop/BALL/team-NBA/l_HuRen.csv",row.names = F)


#再切
#-------------------------#
#-----------第五步--------#
#-------------------------#

#先下载数据，删除部分变量， 文件名记为队名，如HuRen 是最原始的数据
#l_HuRen是list格式的爬虫数据含义


#------把球队的赛季数据加入--------#
l<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/l_HuRen.csv",1)
HuRen<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/HuRen.csv",1)
#remove(HuRen$球队,HuRen$场,HuRen$投篮,HuRen$出手,HuRen$三分,HuRen$出手.1,
#       HuRen$罚球,HuRen$出手.2,HuRen$前场,HuRen$后场)
HuRen$t1<-l$a3[which(l$a2==1)]
HuRen$t2<-l$a3[which(l$a2==2)]
HuRen$t3<-l$a3[which(l$a2==3)]
HuRen$t4<-l$a3[which(l$a2==4)]
HuRen$t5<-l$a3[which(l$a2==5)]
HuRen$t6<-l$a3[which(l$a2==6)]
HuRen$t7<-l$a3[which(l$a2==7)]
HuRen$t8<-l$a3[which(l$a2==8)]
HuRen$t9<-l$a3[which(l$a2==9)]
HuRen$t10<-l$a3[which(l$a2==10)]
HuRen$t11<-l$a3[which(l$a2==11)]
HuRen$t12<-l$a3[which(l$a2==12)]
HuRen$t13<-l$a3[which(l$a2==13)]
HuRen$t14<-l$a3[which(l$a2==14)]
HuRen$t15<-l$a3[which(l$a2==15)]
HuRen$t16<-l$a3[which(l$a2==16)]
HuRen$t17<-l$a3[which(l$a2==17)]
HuRen$t18<-l$a3[which(l$a2==18)]
#-------------添加时间变量------#
HuRen$s1<-l$s[which(l$a2==1)]
HuRen$s2<-l$s[which(l$a2==2)]
HuRen$s3<-l$s[which(l$a2==3)]
HuRen$s4<-l$s[which(l$a2==4)]
HuRen$s5<-l$s[which(l$a2==5)]
HuRen$s6<-l$s[which(l$a2==6)]
HuRen$s7<-l$s[which(l$a2==7)]
HuRen$s8<-l$s[which(l$a2==8)]
HuRen$s9<-l$s[which(l$a2==9)]
HuRen$s10<-l$s[which(l$a2==10)]
HuRen$s11<-l$s[which(l$a2==11)]
HuRen$s12<-l$s[which(l$a2==12)]
HuRen$s13<-l$s[which(l$a2==13)]
HuRen$s14<-l$s[which(l$a2==14)]
HuRen$s15<-l$s[which(l$a2==15)]
HuRen$s16<-l$s[which(l$a2==16)]
HuRen$s17<-l$s[which(l$a2==17)]
HuRen$s18<-l$s[which(l$a2==18)]
write.csv(HuRen,"C:/Users/lenovo/Desktop/BALL/team-NBA/HuRen1.csv",row.names = F)

#-------------------------#
#-----------第六步--------#
#-------------------------#

#---------------------------------------------#
#------对集合后的整体球队数据进行最后处理-----#
#---------------------------------------------#
#HuRen1已经把赛场上的数据和爬虫数据糅合在一起
HuRen1<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/HuRen1.csv",1)

names(HuRen1)[1:14]<-c("game","time","outcome","pk","23n","3n","fn","rebound",
                      "assist","steal","block","error","foul","score")
#---------对胜负替换成虚拟变量--------#
HuRen1$outcome<-gsub("胜",1, HuRen1$outcome)
HuRen1$outcome<-gsub("负",0, HuRen1$outcome)
HuRen1$outcome<-as.numeric(HuRen1$outcome)

#-----------取总比分的差值----------#
tscore<-gsub("76[\u4e00-\u9fa5]","",HuRen1$pk)
tscore<-gsub("[\u4e00-\u9fa5]","",tscore)
sleft<-gsub("-\\d+","",tscore)
sright<-gsub("\\d+-","",tscore)
#scoregap赋值一定要放在循环外面，否则一直循环出错
scoregap<-rep(0,nrow(HuRen1))
for (i in 1:nrow(HuRen1)) {
  scoregap[i]<-as.numeric(sright)[i] - as.numeric(sleft)[i]
}
#scoregap<-scoregap*0.02
HuRen1$scoregap<-scoregap
#-----对手分数处理----#
HuRen1$vs_s<-1/as.numeric(sleft)
#进攻得分
HuRen1$score3<-HuRen1$`3n`*3
HuRen1$score2<-(HuRen1$`23n`-HuRen1$`3n`)*2
HuRen1$score1<-HuRen1$fn*1
write.csv(HuRen1,"C:/Users/lenovo/Desktop/BALL/team-NBA/HuRen_f.csv")



#--------------------------#
#-------细节操作版本-------#
#--------------------------#
#（第一次操作实验）



#----------------先抓取每场比赛的url---------------#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="UTF-8"
)
url<-"http://www.stat-nba.com/query_team.php?crtcol=date_out&order=0&QueryType=game&GameType=season&Team_id=LAL&PageNum=1000&Season0=2016&Season1=2017"
temp<-getURL(url,header="myheader")  #"UTF-8" "ISO-8859-1"，"GB2312" "GBK"
# Encoding(temp)<-"UTF-8"
# Encoding(temp)
temp<-gettext(temp)
# stri_escape_unicode(temp)
# Encoding(temp)<-"utf-8"
# stri_conv(temp,"unknown","utf-8")
k2<-strsplit(temp,"\n")[[1]]#此处\r\n出现问题  \r即回车return   \n即换行new line
k2[1:20]
k3<-k2[-c(1:11)]
k3[6]
write.table(k3,"c://users/lenovo/desktop/1.txt")
grep("<tr>",k3)
# k3[443]
pk=k3[grep("<tr>",k3)+6]  
pk
#name1<-gsub("<a target=\"_blank\" href=\"http://bbs.hupu.com/16067126.html\">","",name)
#这里不能用gsub的原因：数字都不相同
url1<-gsub("<td class=\"normal result_out change_color col5 row\\d+\">","",pk)
url1  #所以此处用转义字符替代数字
url2<-gsub("[\u4e00-\u9fa5]\\w","",url1)
url2
url3<-gsub("' target='_blank'>\\S+","",url2)  #任意多个非空格，去除特定字符串后的所有字符
url3
url4<-gsub("<a href='.","",url3)
url4
url5<-paste("http://www.stat-nba.com",url4,sep="")
url5[-1]
url5

#----------------再抓取每场比赛球员的上场时间---------------#
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8,ISO-8859-1,GBK;q=0.7,*;q=0.7"
)
urla<-"http://www.stat-nba.com/game/38937.html"
tempa=read_html(urla)
tempa<-gettext(tempa)

Encoding(tempa)
kk2<-strsplit(tempa,"\n")[[1]]#此处\r\n出现问题  \r即回车return   \n即换行new line
kk2[1:20]
kk3<-kk2
kk3[292]
# grep("<tr class="sort">",k3)
# k3[443]
#----上场队员姓名-----#
name=kk3[grep("<tr class=\"sort\">",kk3)+2]  
name
name1<-gsub( "[\u4e00-\u9fa5]","",name)
name1

#---队员上场时间-----#
time=kk3[grep("<tr class=\"sort\">",kk3)+4]
time
time1<-gsub("<td class=\"normal mp change_color col2 row","",time)
time2<-gsub("\\d\" rank=\"","",time1)
time2
time3<-gsub("\\d+\">","",time2)
time3
time4<-gsub("</td>","",time3)
time4
write.csv(name,"c:/users/lenovo/desktop/name.csv")

# 合并数据框
Ren <- data.frame(name1,time4)
names(Ren) = c("name","time")
write.table(Ren,"c:/users/lenovo/desktop/Ren.csv",append = TRUE,
            sep=",",col.names = F,row.names =T)
#列名自己加，因为col.names=T会循环产生列名,导致多余的列名


















