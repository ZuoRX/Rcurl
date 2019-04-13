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

#------------------#
#-------老鹰-------#
#------------------#


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
url<-"http://www.stat-nba.com/query_team.php?crtcol=date_out&order=0&QueryType=game&GameType=season&Team_id=ATL&PageNum=1000&Season0=2016&Season1=2017"
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
  name1[name1=="德怀特霍华德"]<-1
  name1[name1=="保罗米尔萨普"]<-2
  name1[name1=="肯特贝兹莫尔"]<-3
  name1[name1=="艾森伊利亚索瓦"]<-4
  name1[name1=="迈克邓利维"]<-5
  name1[name1=="克里斯亨弗里斯"]<-6
  name1[name1=="萨波索夫洛萨"]<-7
  name1[name1=="丹尼斯施罗德"]<-8
  name1[name1=="马尔科姆德莱尼"]<-9
  name1[name1=="陶林普林斯"]<-10
  name1[name1=="蒂姆哈达威二世"]<-11
  name1[name1=="贾莱特杰克"]<-12
  name1[name1=="德安德烈本布里"]<-13
  name1[name1=="马克穆斯卡拉"]<-14
  name1[name1=="沃尔特塔瓦雷斯"]<-15
  name1[name1=="雷恩凯利"]<-16
  name1[name1=="何塞卡尔德隆斯"]<-17
  name1[name1=="拉玛尔帕特森"]<-18
  name1[name1=="加里尼尔"]<-19
  name1[name1=="马特科斯特洛"]<-20
  name1[name1=="凯尔科沃尔"]<-21
  
  
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
  
  write.table(teamdata1,"C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/time_LaoYing.csv",append = TRUE,
              sep=",",col.names = F,row.names =F)
}

#-------------------------#
#-----------第三步--------#
#-------------------------#
rm(list=ls())
#先要对表time_LaoYing中第二列进行排序处理

time_LaoYing<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/time_LaoYing.csv",1)
#----------把球员补全---------#
#写个循环
#21*82=1722
a1<-rep(1:82,each=21)
a2<-rep(1:21,82)
a3<-rep(0,1722)
l<-data.frame(a1,a2,a3)
#l<-matrix(rep(0,1476*3),ncol = 3)
for(j in 1:1722){#1476
  for(i in 1:839){
    if(l[j,2]==time_LaoYing[i,2] &l[j,1]==time_LaoYing[i,1] ){
      l[j,3]<-time_LaoYing[i,3]
    }
  }
}

#-------------------------#
#-----------第四步--------#
#-------------------------#



#--把球员的薪酬加上-----#
l$s<-rep(0,1722)
l$s[l$a2==1 & l$a3 >0]<-2318
l$s[l$a2==2 & l$a3 >0]<-2007
l$s[l$a2==3 & l$a3 >0]<-1573
l$s[l$a2==4 & l$a3 >0]<-840
l$s[l$a2==5 & l$a3 >0]<-484
l$s[l$a2==6 & l$a3 >0]<-400
l$s[l$a2==7 & l$a3 >0]<-385
l$s[l$a2==8 & l$a3 >0]<-271
l$s[l$a2==9 & l$a3 >0]<-250
l$s[l$a2==10 & l$a3 >0]<-232
l$s[l$a2==11 & l$a3 >0]<-228
l$s[l$a2==12 & l$a3 >0]<-155
l$s[l$a2==13 & l$a3 >0]<-150
l$s[l$a2==14 & l$a3 >0]<-102
l$s[l$a2==15 & l$a3 >0]<-100
l$s[l$a2==16 & l$a3 >0]<-42
l$s[l$a2==17 & l$a3 >0]<-39
l$s[l$a2==18 & l$a3 >0]<-13
l$s[l$a2==19 & l$a3 >0]<-7
l$s[l$a2==20 & l$a3 >0]<-5
l$s[l$a2==21 & l$a3 >0]<-524

#l是list意思，l_LaoYing里面包含所有队员的薪酬，时间
write.csv(l,"C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/l_LaoYing.csv",row.names = F)


#-------------------------#
#-----------第五步--------#
#-------------------------#

#先下载数据，删除部分变量， 文件名记为队名，如LaoYing 



#------把球队的赛季数据加入--------#
rm(list = ls())
l<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/l_LaoYing.csv",1)
LaoYing<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/LaoYing.csv",1)
#remove(LaoYing$球队,LaoYing$场,LaoYing$投篮,LaoYing$出手,LaoYing$三分,LaoYing$出手.1,
#       LaoYing$罚球,LaoYing$出手.2,LaoYing$前场,LaoYing$后场)
LaoYing$t1<-l$a3[which(l$a2==1)]
LaoYing$t2<-l$a3[which(l$a2==2)]
LaoYing$t3<-l$a3[which(l$a2==3)]
LaoYing$t4<-l$a3[which(l$a2==4)]
LaoYing$t5<-l$a3[which(l$a2==5)]
LaoYing$t6<-l$a3[which(l$a2==6)]
LaoYing$t7<-l$a3[which(l$a2==7)]
LaoYing$t8<-l$a3[which(l$a2==8)]
LaoYing$t9<-l$a3[which(l$a2==9)]
LaoYing$t10<-l$a3[which(l$a2==10)]
LaoYing$t11<-l$a3[which(l$a2==11)]
LaoYing$t12<-l$a3[which(l$a2==12)]
LaoYing$t13<-l$a3[which(l$a2==13)]
LaoYing$t14<-l$a3[which(l$a2==14)]
LaoYing$t15<-l$a3[which(l$a2==15)]
LaoYing$t16<-l$a3[which(l$a2==16)]
LaoYing$t17<-l$a3[which(l$a2==17)]
LaoYing$t18<-l$a3[which(l$a2==18)]
LaoYing$t19<-l$a3[which(l$a2==19)]
LaoYing$t20<-l$a3[which(l$a2==20)]
LaoYing$t21<-l$a3[which(l$a2==21)]



#-------------添加时间变量------#
LaoYing$s1<-l$s[which(l$a2==1)]
LaoYing$s2<-l$s[which(l$a2==2)]
LaoYing$s3<-l$s[which(l$a2==3)]
LaoYing$s4<-l$s[which(l$a2==4)]
LaoYing$s5<-l$s[which(l$a2==5)]
LaoYing$s6<-l$s[which(l$a2==6)]
LaoYing$s7<-l$s[which(l$a2==7)]
LaoYing$s8<-l$s[which(l$a2==8)]
LaoYing$s9<-l$s[which(l$a2==9)]
LaoYing$s10<-l$s[which(l$a2==10)]
LaoYing$s11<-l$s[which(l$a2==11)]
LaoYing$s12<-l$s[which(l$a2==12)]
LaoYing$s13<-l$s[which(l$a2==13)]
LaoYing$s14<-l$s[which(l$a2==14)]
LaoYing$s15<-l$s[which(l$a2==15)]
LaoYing$s16<-l$s[which(l$a2==16)]
LaoYing$s17<-l$s[which(l$a2==17)]
LaoYing$s18<-l$s[which(l$a2==18)]
LaoYing$s19<-l$s[which(l$a2==19)]
LaoYing$s20<-l$s[which(l$a2==20)]
LaoYing$s21<-l$s[which(l$a2==21)]


write.csv(LaoYing,"C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/LaoYing1.csv",row.names = F)

#-------------------------#
#-----------第六步--------#
#-------------------------#

#---------------------------------------------#
#------对集合后的整体球队数据进行最后处理-----#
#---------------------------------------------#
#LaoYing1已经把赛场上的数据和爬虫数据糅合在一起
LaoYing1<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/LaoYing1.csv",1)

names(LaoYing1)[1:14]<-c("game","time","outcome","pk","23n","3n","fn","rebound",
                         "assist","steal","block","error","foul","score")
#---------对胜负替换成虚拟变量--------#
LaoYing1$outcome<-gsub("胜",1, LaoYing1$outcome)
LaoYing1$outcome<-gsub("负",0, LaoYing1$outcome)
LaoYing1$outcome<-as.numeric(LaoYing1$outcome)

#-----------取总比分的差值----------#
tscore<-gsub("76[\u4e00-\u9fa5]","",LaoYing1$pk)
tscore<-gsub("[\u4e00-\u9fa5]","",tscore)
sleft<-gsub("-\\d+","",tscore)
sright<-gsub("\\d+-","",tscore)
#scoregap赋值一定要放在循环外面，否则一直循环出错
scoregap<-rep(0,nrow(LaoYing1))
for (i in 1:nrow(LaoYing1)) {
  scoregap[i]<-as.numeric(sright)[i] - as.numeric(sleft)[i]
}
#scoregap<-scoregap*0.02
LaoYing1$scoregap<-scoregap
#-----对手分数处理----#
LaoYing1$vs_s<-1/as.numeric(sleft)
#进攻得分
LaoYing1$score3<-LaoYing1$`3n`*3
LaoYing1$score2<-(LaoYing1$`23n`-LaoYing1$`3n`)*2
LaoYing1$score1<-LaoYing1$fn*1
write.csv(LaoYing1,"C:/Users/lenovo/Desktop/BALL/team-NBA/LaoYing/LaoYing_f.csv")




















