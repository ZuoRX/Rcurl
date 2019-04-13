library(bitops)
library(RCurl)
library(XML)
library(rvest)
library(magrittr)  #管道符
library(tcltk)
library(progress)
library(plyr)


#================#
#==爬取游记链接==#
#================#

url<-"http://you.ctrip.com/place/gulangyu120058.html"

web1<-read_html(url,encoding="utf-8",options = "HUGE")

#解析游记链接
tour<-web1%>%html_nodes(xpath ="//div[@class='journalslist cf']/a") %>% html_attrs()


tour1<-gsub("class = \"journal-item cf\", target = \"_blank\", href = \"|\"","",tour)
tour2<-gsub("c\\S","http://you.ctrip.com",tour1)
tour3<-gsub("html\\S","html",tour2)


#================#
#==爬取游记文本==#
#================#

#url1<-tour3[1]

Data<-data.frame()

for(i in 1:10){
web2<-read_html(tour3[i],encoding="utf-8",options = "HUGE")

#解析游记链接
content<-web2%>%html_nodes(xpath ="//div[@class='ctd_content']") %>% html_text()
content1<-gsub("\r\n|\\s+","",content)

Data<-rbind(Data,content1)
}

write.table(Data,"c:/users/lenovo/desktop/gulangyu/youji.txt")

#================#
#==游记词频分析==#
#================#
library(jiebaR) 
library(Rwordseg)

#==========================#
#=====第二步：中文分词=====#
#==========================#

cutter <- worker(bylines = T,encoding='UTF-8',detect=T, user_weight = "max",
                 user = "c:/users/lenovo/desktop/gulangyu/userword.txt",
                 stop_word = "c:/users/lenovo/desktop/gulangyu/停词.txt")
#创建分词器，其中bylines是否按行来分，user用户词典，stop_word停用词典


comments_seg <- cutter["c:/users/lenovo/desktop/gulangyu/youji.txt"] 
#文件分词，直接输入文件地址，分完后自动保存成文件




#==================================#
#=====第三步：分词的数字化处理=====#
#==================================#

#将文本转化为向量，用数字来代替文本，从而方便运算。

#读取分词结果
comments_segged<- readLines("c:/users/lenovo/desktop/gulangyu/youji.segment.2019-02-14_20_46_16.txt",
                            encoding="UTF-8") 
#将向量转化为列表
comments <- as.list(comments_segged) 
#将每行文本，按照空格分开，每行变成一个词向量，储存在列表里
doc.list <- strsplit(as.character(comments),split=" ") 



#====================================#
#=====第四步：对每个分词进行编号=====#
#====================================#

#这里有两步，unlist用于统计每个词的词频；
#table把结果变成一个交叉表式的factor，原理类似python里的词典，key是词，value是词频
term.table <- table(unlist(doc.list)) 
#按照词频降序排列
term.table <- sort(term.table, decreasing = TRUE) 


#============================#
#=====第五步：分词再清洗=====#
#============================#

# 用stop_words去掉了数字、标点符号、虚词等，由于现代汉语里用单字表示一个词语的词已经很少了，
# 这里为了提高建模效果，我们可以将单字去掉，同时也可以把出现次数少于5次的词去掉。

del <- term.table < 5| nchar(names(term.table))<2 #把不符合要求的筛出来
term.table <- term.table[!del] #去掉不符合要求的
vocab <- names(term.table) #创建词库













