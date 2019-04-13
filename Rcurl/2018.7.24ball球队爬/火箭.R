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

#---------------------#
#-------Huojian-----#
#---------------------#


#-------------------------#
#-----------第一步--------#
#-------------------------#
#整理所有队员薪酬，并统计球队上场球员总数，并--编号--
#用球员的数据和薪金的数据综合

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
url<-"http://www.stat-nba.com/query_team.php?crtcol=date_out&order=0&QueryType=game&GameType=season&Team_id=HOU&PageNum=1000&Season0=2016&Season1=2017"
web<-read_html(url,encoding="UTF-8",options = "HUGE")
url_1<-web%>%html_nodes(xpath ="//tbody/tr/td[position()=6]/a[@href]") %>% html_attrs()
url1<-gsub(".html.*$|^.*?/game","",url_1)
url5<-paste0("http://www.stat-nba.com/game",url1,".html",sep="")

#----------------再抓取每场比赛球员的上场时间---------------#
#第二步，实验j=1时，导出time数据
#实际做的时候，先不循环，做一个，便于确定数据的位置
#j=2
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 82, clear = FALSE, width= 60)

for (j in 1:82) {
  tempa=read_html(url5[j])
  tempa<-gettext(tempa)
  kk2<-strsplit(tempa,"\n")[[1]]
  pb$tick()
  #----上场队员姓名-----#
  name=kk2[grep("<tr class=\"sort\">",kk2)+2]  
  name1<-gsub( "[^\u4e00-\u9fa5]","",name)
  name1<-gsub(" ","",name1)
  name1[name1=="詹姆斯哈登"]<-1
  name1[name1=="莱恩安德森"]<-2
  name1[name1=="埃里克戈登"]<-3
  name1[name1=="特雷沃阿里扎"]<-4
  name1[name1=="科里布鲁尔"]<-5
  name1[name1=="路易斯威廉姆斯"]<-6
  name1[name1=="帕特里克贝弗利"]<-7
  name1[name1=="麦克丹尼尔斯"]<-8
  name1[name1=="内内希拉里奥"]<-9
  name1[name1=="泰勒恩尼斯"]<-10
  name1[name1=="萨姆德克尔"]<-11
  name1[name1=="克林特卡佩拉"]<-12
  name1[name1=="蒙特雷斯哈雷尔"]<-13   #16-17无选秀球员数据
  name1[name1=="鲍比布朗"]<-14
  name1[name1=="凯尔维尔彻"]<-15  
  
  name_adj<-gsub("col0 row","",name1)
  name2<-gsub("\\D+","-1",name_adj)
  name2<-as.numeric(name2)
  # name3<-name2[which(name2>=0)]
  # name3
  #---队员上场时间-----#
  time=kk2[grep("<tr class=\"sort\">",kk2)+4]
  time1<-gsub("<td class=\"normal mp change_color col2 row","",time)
  time2<-gsub("\\d\" rank=\"","",time1)
  time3<-gsub("\\d+\">","",time2)
  time4<-gsub("</td>","",time3)
  remove("time1","time2","time3")
  
  #----个人技术指标----#                    #第一处需要添加的地方#
  #篮板
  rebound<-kk2[grep("<tr class=\"sort\">",kk2)+15]
  rebound1<-gsub("<td class=\"normal trb change_color col13 row\\d+\" rank=\"\\d+\">","",rebound)
  rebound2<-gsub("</td>","",rebound1)
  
  #前场篮板
  o_rebound<-kk2[grep("<tr class=\"sort\">",kk2)+16]
  o_rebound1<-gsub("<td class=\"normal orb change_color col14 row\\d+\" rank=\"\\d+\">","",o_rebound)
  o_rebound2<-gsub("</td>","",o_rebound1)
  
  #后场篮板
  d_rebound<-kk2[grep("<tr class=\"sort\">",kk2)+17]
  d_rebound1<-gsub("<td class=\"normal drb change_color col15 row\\d+\" rank=\"\\d+\">","",d_rebound)
  d_rebound2<-gsub("</td>","",d_rebound1)
  
  #助攻
  assist<-kk2[grep("<tr class=\"sort\">",kk2)+18]
  assist1<-gsub("<td class=\"normal ast change_color col16 row\\d+\" rank=\"\\d+\">","",assist)
  assist2<-gsub("</td>","",assist1)
  
  
  #抢断
  steal<-kk2[grep("<tr class=\"sort\">",kk2)+19]
  steal1<-gsub("<td class=\"normal stl change_color col17 row\\d+\" rank=\"\\d+\">","",steal)
  steal2<-gsub("</td>","",steal1)
  
  #盖帽
  block<-kk2[grep("<tr class=\"sort\">",kk2)+20]
  block1<-gsub("<td class=\"normal blk change_color col18 row\\d+\" rank=\"\\d+\">","",block)
  block2<-gsub("</td>","",block1)
  
  #失误
  error<-kk2[grep("<tr class=\"sort\">",kk2)+21]
  error1<-gsub("<td class=\"normal tov change_color col19 row\\d+\" rank=\"\\d+\">","",error)
  error2<-gsub("</td>","",error1)
  
  #犯规
  foul<-kk2[grep("<tr class=\"sort\">",kk2)+22]
  foul1<-gsub("<td class=\"normal pf change_color col20 row\\d+\" rank=\"\\d+\">","",foul)
  foul2<-gsub("</td>","",foul1)
  
  #得分
  score<-kk2[grep("<tr class=\"sort\">",kk2)+23]
  score1<-gsub("<td class=\"normal pts change_color col21 row\\d+\" rank=\"\\d+\">","",score)
  score2<-gsub("</td>","",score1)
  
  #----构建场次记录----#
  game<-rep(j,length(time4))
  #---- 合并数据框----#
  teamdata<- data.frame(game,name2,time4,rebound2,o_rebound2,  
                        d_rebound2,assist2,steal2,block2,
                        error2,foul2,score2)
  #teamdata<-teamdata[-23,]
  teamdata1<-teamdata[which(teamdata$name2>=0),] #这个，不能忽视 #//第一次复制结束
  
  write.table(teamdata1,"C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/time_Huojian.csv",append = TRUE,
              sep=",",col.names = F,row.names =F)
}

#-------------------------#
#-----------第三步--------#
#-------------------------#
rm(list=ls())                     #第二处操作的地方#
#先要对表time_Huojian中第二列进行排序处理

#第三处修改的地方#
time_Huojian<-read.table("C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/time_Huojian.csv",
                         header=FALSE,sep=",")
names(time_Huojian)<-c("game","name","time","p_rebound","p_o_rebound",  
                       "p_d_rebound","p_assist","p_steal","p_block",
                       "p_error","p_foul","s")      #第三处修改结束#
#----------把球员补全---------#
a1<-rep(1:82,each=15)
a2<-rep(1:15,82)
a3<-matrix(0,nrow = 1230,ncol = 10)  #第四处需要修改的小地方

l<-data.frame(a1,a2,a3)
pb <- progress_bar$new(format = "  完成百分比 [:bar] :percent 执行时间 :elapsed",
                       total = 826, clear = FALSE, width= 60)

for(j in 1:1230){ 
  for(i in 1:826){          #第五处需要修改的小地方  +1
    if(l[j,2]==time_Huojian[i,2] &l[j,1]==time_Huojian[i,1] ){
      l[j,3]<-time_Huojian[i,3]
      l[j,4]<-time_Huojian[i,4]#第六处需要添加数据的地方，导出个人技术指标
      l[j,5]<-time_Huojian[i,5]
      l[j,6]<-time_Huojian[i,6]
      l[j,7]<-time_Huojian[i,7]
      l[j,8]<-time_Huojian[i,8]
      l[j,9]<-time_Huojian[i,9]
      l[j,10]<-time_Huojian[i,10]
      l[j,11]<-time_Huojian[i,11]
      l[j,12]<-time_Huojian[i,12]   #第六处结束
      pb$tick()
    }
  }
}
names(l)<-c("a1","a2","a3")     #第六处补充修改


#-------------------------#
#-----------第四步--------#
#-------------------------#
#--把球员的薪酬加上-----#
l$s<-rep(0,1230)        #记得要改
l$s[l$a2==1 & l$a3 >0]<-2654
l$s[l$a2==2 & l$a3 >0]<-1874
l$s[l$a2==3 & l$a3 >0]<-1239
l$s[l$a2==4 & l$a3 >0]<-781
l$s[l$a2==5 & l$a3 >0]<-760
l$s[l$a2==6 & l$a3 >0]<-700
l$s[l$a2==7 & l$a3 >0]<-600
l$s[l$a2==8 & l$a3 >0]<-333
l$s[l$a2==9 & l$a3 >0]<-290
l$s[l$a2==10 & l$a3 >0]<-173
l$s[l$a2==11 & l$a3 >0]<-172
l$s[l$a2==12 & l$a3 >0]<-130
l$s[l$a2==13 & l$a3 >0]<-105
l$s[l$a2==14 & l$a3 >0]<-93
l$s[l$a2==15 & l$a3 >0]<-83                 #第7处需要修改变量名称
#l是list意思，l_Huojian里面包含所有队员的时间a3，薪酬,个人技术数据
names(l)<-c("a1","a2","a3","p_rebound","p_o_rebound",  
            "p_d_rebound","p_assist","p_steal","p_block",
            "p_error","p_foul","p_score","s")  #第7处结束
write.csv(l,"C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/l_Huojian.csv",row.names = F)

#-------------------------#
#-----------第五步--------#
#-------------------------#
#第8处需要导出82场比赛的数据
#先下载数据，删除部分变量， 加入前场和后场篮板，文件名记为队名，如Huojian 
#------把球队的赛季数据加入--------#
rm(list = ls())
l<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/l_Huojian.csv",1)
#Huojian下载的表，删除第84行，最右边两列，因为总出问题
Huojian<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/Huojian.csv",1)


Huojian$t1<-l$a3[which(l$a2==1)]
Huojian$t2<-l$a3[which(l$a2==2)]
Huojian$t3<-l$a3[which(l$a2==3)]
Huojian$t4<-l$a3[which(l$a2==4)]
Huojian$t5<-l$a3[which(l$a2==5)]
Huojian$t6<-l$a3[which(l$a2==6)]
Huojian$t7<-l$a3[which(l$a2==7)]
Huojian$t8<-l$a3[which(l$a2==8)]
Huojian$t9<-l$a3[which(l$a2==9)]
Huojian$t10<-l$a3[which(l$a2==10)]
Huojian$t11<-l$a3[which(l$a2==11)]
Huojian$t12<-l$a3[which(l$a2==12)]
Huojian$t13<-l$a3[which(l$a2==13)]
Huojian$t14<-l$a3[which(l$a2==14)]
Huojian$t15<-l$a3[which(l$a2==15)]

#-------------添加salary变量------#
Huojian$s1<-l$s[which(l$a2==1)]
Huojian$s2<-l$s[which(l$a2==2)]
Huojian$s3<-l$s[which(l$a2==3)]
Huojian$s4<-l$s[which(l$a2==4)]
Huojian$s5<-l$s[which(l$a2==5)]
Huojian$s6<-l$s[which(l$a2==6)]
Huojian$s7<-l$s[which(l$a2==7)]
Huojian$s8<-l$s[which(l$a2==8)]
Huojian$s9<-l$s[which(l$a2==9)]
Huojian$s10<-l$s[which(l$a2==10)]
Huojian$s11<-l$s[which(l$a2==11)]
Huojian$s12<-l$s[which(l$a2==12)]
Huojian$s13<-l$s[which(l$a2==13)]
Huojian$s14<-l$s[which(l$a2==14)]
Huojian$s15<-l$s[which(l$a2==15)]

#添加个人技术指标  15号新秀       #第9处需要复制粘贴的地方
Huojian$p_rebound  <-l$p_rebound[which(l$a2==15)]
Huojian$p_o_rebound<-l$p_o_rebound[which(l$a2==15)]
Huojian$p_d_rebound<-l$p_d_rebound[which(l$a2==15)]
Huojian$p_assist   <-l$p_assist[which(l$a2==15)]
Huojian$p_steal    <-l$p_steal[which(l$a2==15)]
Huojian$p_block    <-l$p_block[which(l$a2==15)]
Huojian$p_error    <-l$p_error[which(l$a2==15)]
Huojian$p_foul     <-l$p_foul[which(l$a2==15)]
Huojian$p_score    <-l$p_score[which(l$a2==15)]   #第9处结束

write.csv(Huojian,"C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/Huojian1.csv",row.names = F)

#-------------------------#
#-----------第六步--------#
#-------------------------#

#---------------------------------------------#
#------对集合后的整体球队数据进行最后处理-----#
#---------------------------------------------#
#Huojian1已经把赛场上的数据和爬虫数据糅合在一起
Huojian1<-read.csv("C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/Huojian1.csv",1)

#第10处修改变量名称的地方
names(Huojian1)[1:14]<-c("game","team","time","outcome","pk","rebound","o_rebound","d_rebound",
                         "assist","steal","block","error","foul","score")
#---------对胜负替换成虚拟变量--------#
Huojian1$outcome<-gsub("胜",1, Huojian1$outcome)
Huojian1$outcome<-gsub("负",0, Huojian1$outcome)
Huojian1$outcome<-as.numeric(Huojian1$outcome)

#-----------取总比分的差值----------#
tscore<-gsub("76[\u4e00-\u9fa5]","",Huojian1$pk)
tscore<-gsub("[\u4e00-\u9fa5]","",tscore)
sleft<-gsub("-\\d+","",tscore)
sright<-gsub("\\d+-","",tscore)
#scoregap赋值一定要放在循环外面，否则一直循环出错
scoregap<-rep(0,nrow(Huojian1))
for (i in 1:nrow(Huojian1)) {
  scoregap[i]<-as.numeric(sright)[i] - as.numeric(sleft)[i]
}
#scoregap<-scoregap*0.02
Huojian1$scoregap<-scoregap

#第11处，直接进行数据导出
#-----对手分数处理----#
# Huojian1$vs_s<-1/as.numeric(sleft)
#进攻得分
# Huojian1$score3<-Huojian1$`3n`*3
# Huojian1$score2<-(Huojian1$`23n`-Huojian1$`3n`)*2
# Huojian1$score1<-Huojian1$fn*1
write.csv(Huojian1,"C:/Users/lenovo/Desktop/BALL/team-NBA/Huojian/Huojian_f.csv")

