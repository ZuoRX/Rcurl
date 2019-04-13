 rm(list=ls()) 
 ###packages################
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
 library(xlsx)
 library(igraph)
 library(wordcloud2)
 library(e1071)
 #第一步：读取数据##############################################
 jour<- read.xlsx("C:/Users/lenovo/Desktop/behavir.xlsx",1,encoding="UTF-8")

#第二步：导入语料库
installDict("D:\\library\\words\\心理学.scel","sougou")  
installDict("D:\\library\\words\\项目管理词汇.scel","sougou")   
installDict("D:\\library\\words\\社会学专业词库.scel","sougou")  
listDict()#查看已安装词典
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
write.csv(df2,"path")
wordsFreq<-wordsFreq[-which(nchar(wordsFreq[,1])<3),]
wordcloud(df2$term,df2$Freq,min.freq = 10)





 