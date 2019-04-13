rm(list=ls())#清除R环境中的所有东西
#加载程序包
library(text2vec)
library(plotrix)
library(arules)
library(NLP)
library(tm)
library(rJava)
library(Rwordseg)
library(SnowballC)
library(MASS)
library(RColorBrewer)
library(wordcloud)
library(pcaPP)
library(rainbow)
library(Rcpp)
library(cluster)
library(mvtnorm)
library(hdrcde)
library(locfit)
library(ash)
library(KernSmooth)
library(misc3d)
library(rgl)
library(ks)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(grid)
library(vcd)
library(topicmodels)
library(randomForest)
library(rFerns)
library(ranger)
library(Boruta) 
library(lattice)
library(caret)
library(slam)
library(Matrix)
library(foreach)
library(glmnet)
library(wordcloud2)
library(NbClust)
library(gtools)
library(ape)
library(sparcl)
library(igraph)
library(arulesViz)
library(arules)
library(reshape)
library(kernlab)
library(fpc)
library(xlsx)
########################################################################################################################
lht<-read.xlsx("C:\\Users\\Administrator\\Desktop\\library\\Q2\\INFORMATION TECHNOLOGY & PEOPLE.xlsx",1)
###########################数据预处理#######################################  
lht<-lht[lht$PY=='2017',]
abstract<- tm::Corpus(tm::VectorSource(lht$AB))
abstract<-tm_map(abstract,PlainTextDocument)
#abstract<-tm_map(abstract,stripWhitespace)
abstract<-tm_map(abstract,tolower)
abstract<-tm_map(abstract,removeWords,stopwords("en"))
abstract<-tm_map(abstract,removePunctuation)
abstract<-tm_map(abstract,removeNumbers)
abstract<-tm_map(abstract,stemDocument)
############################词频###################################################
term<-lapply(X=abstract,FUN = strsplit,' ')
term<-unlist(term)
df<-table(term)#建表
df
df1<-sort(df,decreasing = T)#降序排列
df1
seg3<-names(df1)
df2<-as.data.frame(df1)
df2
write.csv(df2,"path")
wordcloud(df2$term,df2$Freq,min.freq = 10)



#第一步：读取数据##############################################
jour<- read.xlsx("C:/Users/lenovo/Desktop/individual.xlsx",1,encoding="UTF-8")

#第二步：导入语料库
installDict(dictpath = 'D:\\library\\words\\社会学专业词库.scel',
            dictname = "社会学专业词库", dicttype = "scel", load = TRUE)
installDict(dictpath = 'D:\\library\\words\\项目管理词汇.scel',
            dictname = "项目管理词汇", dicttype = "scel", load = TRUE)
installDict(dictpath = 'D:\\library\\words\\心理学.scel',
            dictname = "心理学", dicttype = "scel", load = TRUE)
installDict(dictpath = 'D:\\library\\words\\社会工作专业词库.scel',
            dictname = "社会工作专业词库", dicttype = "scel", load = TRUE)
installDict(dictpath = 'D:\\library\\words\\中国职业名称大全.scel',
            dictname = "中国职业名称大全", dicttype = "scel", load = TRUE)
installDict(dictpath = 'D:\\library\\words\\心理学词库.scel',
            dictname = "心理学词库", dicttype = "scel", load = TRUE)
installDict(dictpath = 'D:\\library\\words\\教育教学综合词库.scel',
            dictname = "教育教学综合词库", dicttype = "scel", load = TRUE)
listDict()#查看已安装词典

#第三步：切割分词。在处理中文时，要把字符变量转换可读的，所以尽量用英文写变量名词
title<-as.character(jour$标题)
segmentCN(title)
titlewords=segmentCN(title)
#sink("D:\\library\\words\\titlewords.xlsx",append=TRUE,split=TRUE)#把数据导出到文件

#第四步：计算词频→建立数据框）
term<-lapply(X=titlewords,FUN = strsplit,' ')
term<-unlist(term)
df<-table(term)#建表
df

df1<-sort(df,decreasing = T)#降序排列
df1
seg3<-names(df1)
df2<-as.data.frame(df1)
df2



















































