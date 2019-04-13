
setwd('D:\\Rwork\\RStudio\\boss-shenzhen')# 设置工作目录

################################################## 开始爬取 ##################################################
rm(list = ls())
library(xml2)
library(rvest)

# 读取页面信息
site1 <- "https://www.zhipin.com/c101280600/h_101280600/?query=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&page="
site2 <- "&ka=page-"
page <- 1
for(page in 1:30){
  site <- paste(site1,page,site2,page,sep="")
  webpage <- read_html(site)  #将网页信息保存到变量中
  webpage
  # 抓取标题和公司名称
  name1 <- html_nodes(webpage,'.job-title')# 抓取标题
  name1
  name2 <- html_nodes(webpage,'.company-text a')# 抓取公司名称
  name2
  
  # 替换掉多余字符
  name1 <- gsub("</div>","",name1)
  name1 <- gsub("<div class=\"job-title\">","",name1)
  titlename <- as.data.frame(name1)
  titlename
  
  name2 <- gsub("<div class=\"company-text\">","",name2)
  name2 <- gsub("\n","",name2)
  name2 <- gsub("                                            ","",name2)
  name2 <- gsub("<h3 class=\"name\">","",name2)
  name2 <- gsub("href=\"/gongsi/298b5298bb20843a03d739m4.html\"","",name2)#gai
  name2 <- gsub("<h3 class=\"name\">","",name2)
  name2 <- gsub("<h3 class=\"name\">","",name2)
  name2 <- gsub("</a></h3>","",name2)
  name2
  name2 <- gsub("<a href=\"/gongsi/a42ac2c59e5658201HV62d0~.html\" ka=\"search_list_company_31_custompage\" target=\"_blank\">","",name2)
  companyname <- as.data.frame(name2)
  
  bossdata <- data.frame(companyname,titlename)
  bossdata

  # 抓取薪资
  salary <- html_nodes(webpage,'.red')
  salary <- gsub("<span class=\"red\">","",salary)
  salary <- gsub("</span>","",salary)
  salary
  
  bossdata <- cbind(bossdata,salary)
  bossdata

  # 抓取要求及公司信息
  message <- html_nodes(webpage,'p')
  message <- message[-1]
  message <- gsub("<p>","",message)
  message <- gsub("<em class=\"vline\"></em>",",",message)
  message <- gsub("</p>","",message)
  message <- gsub("<img",",",message)
  message
  
  i <- as.numeric(length(message))-6
  i
  message <- message[1:i]
  
  # 先设置三个变量，分别读取message中的不同数据
  xx <- c(1:as.numeric(length(message)/3))
  yy <- c(1:as.numeric(length(message)/3))
  zz <- c(1:as.numeric(length(message)/3))
  
  # xx用来存放message中的地点、经验、学历要求
  k1 <- 1
  l1 <- 1
  for(k1 in seq(1:(length(message)/3))){
    xx[k1] <- message[l1]
    l1 <- k1*3+1
  }
  xx
  
  # 可以看出xx虽然提取正确了，但是还需要进一步处理
  xx <- as.data.frame(xx)
  xx
  xx <- apply(as.data.frame(xx),1,strsplit,",")
  xx
  
  xx <- as.data.frame(xx)
  xx[1,]
  
  # 分别提取xx中的地点，经验，学历
  place <- t(as.data.frame(xx[1,]))
  experience <- t(as.data.frame(xx[2,]))
  education <- t(as.data.frame(xx[3,]))
  
  # 合并数据框
  bdata <- data.frame(place,experience,education)
  names(bdata) = c("place", "experience", "education")
  bossdata <- data.frame(bossdata, bdata)
  bossdata
  
  # yy用来存放行业，融资情况，公司规模
  k2 <- 1
  l2 <- 2
  for(k2 in seq(1:15)){
    yy[k2] <- message[l2]
    l2 <- k2*3+2
  }
  yy <- as.data.frame(na.omit(yy))
  yy
  
  # 拆分数据
  yy <- apply(as.data.frame(yy),1,strsplit,",")
  
  # 提取行业信息
  industry <- data.frame(NA)
  financing <- data.frame(NA)
  companysize <- data.frame(NA)
  aaa <- 1
  for (aaa in 1:15){
    ind <- as.data.frame(yy[aaa])
    names(ind) <- ""
    industry[aaa,] <- t(ind)[1]
    financing[aaa,] <- t(ind)[2]
    companysize[aaa,] <- t(ind)[3]
  }
  industry # 行业信息
  financing # 融资信息
  companysize # 公司规模信息
  
  # 填补空白的内容
  if(page == 2){
    financing5 <- financing[5,]
    financing13 <- financing[13,]
    
    financing[5,] <- NA
    financing[13,] <- NA
    companysize[5,] <- financing5
    companysize[13,] <- financing13
  }
  else {
    print("Hello World")
  }
  
  # 整合所有信息
  aqq <- as.data.frame(cbind(industry,financing,companysize),row.names = c(1:15))
  names(aqq) <- c("industry","financing","companysize")
  
  # 添加到数据框中
  bossdata <- data.frame(bossdata,aqq, row.names = c(1:length(bossdata[,1])))
  bossdata

# write.table(bossdata,file="bossdata.csv",sep=",",append=T,row.names = c(1:15),col.names = F)
  
  # zz用来存放联系人及职位
  k3 <- 1
  l3 <- 3
  for(k3 in seq(1:(length(message)/3))){
    zz[k3] <- message[l3]
    l3 <- (k3+1)*3
  }
  zz <- as.data.frame(na.omit(zz))
  zz
  # 拆分数据
  zz <- apply(as.data.frame(zz),1,strsplit,",")
  
  # 提取联系人信息
  linkm <- data.frame(NA)
  linkman <- data.frame(NA)
  position <- data.frame(NA)
  
  for (ccc in 1:15){
    linkm <- as.data.frame(zz[ccc])
    names(linkm) <- ""
    linkman[ccc] <- t(linkm)[1] #提取联系人信息
    position[ccc] <- t(linkm)[2] #提取联系人职位信息
      }
  linkman <- as.data.frame(t(linkman),row.names = F)
  position <- as.data.frame(t(position),row.names = F)
  
  # 添加到数据框中
  bossdata <- data.frame(bossdata,linkman,position)
  bossdata
  
# write.table(bossdata,file="bossdata.csv",sep=",",append=T,row.names = c(1:15),col.names = F)
  
  # 抓取关键词
  keywords <- html_nodes(webpage,'.job-tags span')
  keywords <- gsub("<span>","",keywords)
  keywords <- gsub("</span>","",keywords)
  keywords <- gsub("<span class=\"time\">.*","",keywords)
  
  write.table(bossdata,file="bossdata.csv",sep=",",append=T,row.names = c(1:15),col.names = F)
  
  write.table(keywords,file="keywords.csv",sep=",",append=T,row.names = F,col.names = F)
 }
################################################## 爬取结束 ##################################################

#### 读取、整理数据 ####
shenzhendata <- read.table("bossdata.csv",sep=",")
shenzhendata <- shenzhendata[,2:12]
names(shenzhendata) <- c("companyname","titlename","salary","place","experience",
                         "education","industry","financing","copanysize","linkman","position")
shenzhendata









