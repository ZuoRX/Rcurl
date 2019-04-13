rm(list=ls())
library(bitops)
library(RCurl)
library(XML)
cat("\014") 

url.exists("https://baidu.com")

#伪装自己的header
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
d = debugGatherer()  #声明作用，收集作用，紧跟后面的temp的debugfunction起作用
d
temp <-getURL("http://www.dataguru.cn/",
              httpheader=myheader,           #添加这一个命令
              debugfunction=d$update,
              verbose = TRUE)                #false可以隐藏交互信息，即不会输出d的信息
cat(d$value()[3])#提交给服务器的头信息


#----------------查看“身份”信息--------------#
d = debugGatherer()
url<-"http://www.stat-nba.com/query_team.php?crtcol=date_out&order=0&QueryType=game&GameType=season&Team_id=CHI&PageNum=1000&Season0=2016&Season1=2017"

#temp <-getURL("http://www.dataguru.cn/",debugfunction=d$update,verbose = TRUE)
temp <-getURL(url,debugfunction=d$update,verbose = TRUE)


#cat,输出对象，类似print   但按空格进行了分行处理，可视化效果更好
d
#d$value()  这种情况会返回大量无用信息
cat(d$value()[3])#提交给服务器的头信息
d$value()[2]
print(d$value()[3])
cat(d$value()[1])#服务器地址以及端口号
cat(d$value()[2])#服务器端返回的头信息

#查看服务器端返回的头信息
headers = basicTextGatherer()   ##字符串形式
txt=getURL("http://www.dataguru.cn/",headerfunction = headers$update)
names(headers$value())#说明是字符串形式
headers$value()

rm(list=ls())
#查看服务器端返回的头信息
h = basicHeaderGatherer()###列表形式
txtt=getURL("http://www.dataguru.cn/",headerfunction = h$update)
names(h$value())
h$value()

#-------------查看curl请求的访问信息-------------------#
rm(list=ls())
#即具体爬取数据过程中的信息
curl = getCurlHandle()
d=getURL("http://www.dataguru.cn/", curl = curl)
getCurlInfo(curl)$response.code   #返回状态码，如200=成功读取信息，404错误等
getCurlInfo(curl)

#--------------------getForm函数--------------#
#把提交的信息直接附加在URL后面;postForm会把信息加密提交（关键字啥的）
#信息分解
character<-c("https://www.baidu.com/s?wd=rcurl&rsv_spt=1&rsv_iqid=0x93a61f1800016191&issp=1&f=8&rsv_bp=1&rsv_idx=2&ie=utf-8&rqlang=cn&tn=request_18_pg&rsv_enter=0&oq=userAgent%2520iphone&inputT=8041&rsv_t=bd2fnGbMWmfOigFyR8ma5xfwKG2v%2FJOrz%2B9Mxh%2Fv1EbboiWdxnAp0jJY8rUoMc5lGFP5fA&rsv_pq=bb9de5700001606b&rsv_sug3=11&bs=userAgent%20iphone")
getFormParams(character)#查看提交上去的都有哪些参数

url<-getForm("https://www.baidu.com/s?wd=电影团购&rsv_spt=1&rsv_iqid=0x93a61f1800016191&issp=1&f=8&rsv_bp=1&rsv_idx=2&ie=utf-8&rqlang=cn&tn=request_18_pg&rsv_enter=0&oq=userAgent%2520iphone&inputT=8041&rsv_t=bd2fnGbMWmfOigFyR8ma5xfwKG2v%2FJOrz%2B9Mxh%2Fv1EbboiWdxnAp0jJY8rUoMc5lGFP5fA&rsv_pq=bb9de5700001606b&rsv_sug3=11&bs=userAgent%20iphone")
#课程里面是http，现在变成https，加密了
url<-getForm("http://www.x4jdm.com/index.php?m=vod-search",   
             "wd"="火影",
             rsv_spt ="1", 
             rsv_iqid="0x93a61f1800016191",  
             issp= "1",
             f ="8",
             rsv_bp="1",
             rsv_idx ="2",
             ie="utf-8", 
             rqlang="cn",
             tn="request_18_pg",
             rsv_enter="0",
             oq ="userAgent%2520iphone", 
             inputT ="8041",
             rsv_t= "bd2fnGbMWmfOigFyR8ma5xfwKG2v%2FJOrz%2B9Mxh%2Fv1EbboiWdxnAp0jJY8rUoMc5lGFP5fA",
             rsv_pq ="bb9de5700001606b", 
             rsv_sug3 ="11",
             bs = "userAgent%20iphone")
write.table(url,"C:/Users/lenovo/Desktop/url.txt")

#--------------------postForm函数--------------#
#加密登录，如微博
#https是目前处理加密登录的最大问题








#--------------#
#----第二周----#
#--------------#

# curl部分参数设置，总共有170多个
# verbose：输出访问的交互信息  设置为true时，才会输出，否则被隐藏
# httpheader：设置访问信息报头   会检测是否合法
# .encoding=“UTF-8””GBK”
# debugfunction,headerfunction,curl 搜集信息，身份与过程
# .params：提交的参数组  在getForm和postForm中用到，即需要用到哪些参数来完成请求
# dirlistonly：仅读取目录  设置为true，在ftp上比较出色
# ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/
#   followlocation：支持重定向   =true 即访问新的网页
# http://www.sina.com 
# maxredirs：最大重定向次数     防止跳入死循环或恶性循环 一般设置3或5



rm(list=ls())
# getBinaryURL()函数   以二进制方式下载
url<-"http://job.abchina.com/abb/download_accessory.do?action=accessory&pronunciamentoId=7806"
temp<-getBinaryURL(url)
note <-file("C:/users/lenovo/desktop/hellodata.csv", open = "wb")   #用wb打开
writeBin(temp,note)                         #用temp和note写入
close(note)




# 批量下载的实现   得到一个文件的url，如果得到批量URL，即可得到批量文件
# 结合XML，RCurl更强大
# getURL()
# strsplit()   字符串切割函数
Sys.time()     #举例子 ：切割系统时间
strsplit(as.character(Sys.time()),' ')#作为切割的符号，切割后会直接去除
unlist(strsplit(as.character(Sys.time()),' '))[1]#unlist 把其从list格式中解放出来
# lapply()
# paste()    做下剪切  把两个字符串连接起来
# getBinaryURL()
# Sys.sleep()  休息下，避免频繁访问  缓冲作用，防止被站点加入黑名单


rm(list=ls())
html<-getURL("http://rfunction.com/code/1202/")
html
temp<-strsplit(html,"<li><a href=\"")[[1]]  #此处两个中括号  因为源代码中就是这样（在最开头）
temp
files<-strsplit(temp,"\"")
files
files<-lapply(files,function(x){x[1]})
files<-unlist(files)
files<-files[-(1:2)]
#先令i=1，再进行for循环  把文件下载下来
i<-1  #先实现一个，再放到循环里面  
url<-"http://rfunction.com/code/1202/"
for (i in length(files)) {
  temp<-getBinaryURL(url)
  #note<-file("hellodata.xls",open="wb",append=TRUE) 避免文件覆盖两种方法之一
  note <-file(paste("1202",files[i],sep = '.'), open = "wb")   #用wb打开
  writeBin(temp,note)                         #用temp和note写入
  close(note)
  Sys.sleep(2)
}


#-------------------XML函数，主要抓取表格---------------------------#
# 网页解析工具
# 表格
# 网页节点
# 对标准XML 文件的解析函数xmlParse
# 对html的解析函数htmlTreeParse （处理网页）
# 缺点：windows下对中文的支持不理想  （lingos【待确认】对中文支持比较好）（可以抓英文界面）
# 
rm(list=ls())
url.exists("http://www.bioguo.org/AnimalTFDB/BrowseAllTF.php?spe=Mus_musculus")
url<-"http://www.bioguo.org/AnimalTFDB/BrowseAllTF.php?spe=Mus_musculus"
wp<-getURL(url)                         #先定位
doc <-htmlParse(wp, asText= TRUE)       #再解析
tables <-readHTMLTable(doc)             #抓表格
tables <-readHTMLTable(doc,which = 2)   #选择表格
tables
#出错！！！       网页不对，不出数据


#---------------- 抓取地震数据---------------#
rm(list=ls())
# 国家地震科学数据共享中心
# http://data.earthquake.cn/datashare/datashare_more_quickdata_new.jsp
url.exists("http://data.earthquake.cn/datashare/datashare_more_quickdata_new.js")#url不存在
url<-"http://fund.eastmoney.com/f10/jjjz_110022.html"
wp<-getURL(url)
doc <-htmlParse(wp, asText= TRUE)
tables <-readHTMLTable(doc,header=F)  #header=F,把页眉解析关掉   可以跳过字符串出错
tables
#参数which

# ---------------------XPath---------------------#
#构建一个路径，把rcurl引向资源
# 斜杠（/）作为路径内部的分割符。
# /：表示选择根节点
# //：表示选择任意位置的某个节点（全文匹配）
# @：表示选择某个属性
# *表示匹配任何元素节点。（通配符）
# @*表示匹配任何属性值。
# node()表示匹配任何类型的节点。   txt（）：匹配节点后的文字


url<-"http://www.w3school.com.cn/example/xmle/books.xml"  #网页源代码中夹带
doc<-xmlParse(url)
# 添加谓语条件  已知bookstore是根节点，book是子节点
getNodeSet(doc,'/bookstore/book[1]')              # 对解析后的doc文件进行处理
getNodeSer(doc,'/bookstore/book[last]')          #根节点，（最后一个）子节点
getNodeSet(doc,'/bookstore/book[position()<3]')  # 抓取前两个数据
getNodeSet(doc,'//title[@lang]')                 #属性   lang语言
getNodeSet(doc,'//book/price') 
# 多个并列路径
a<- getNodeSet(doc,"//book/title | //book/price")    #|或符号
write.csv(a,"C:/users/lenovo/desktop/a.csv")       #如何转化下载格式？？？？？？
a[1]


#----------------抓取电影团购——————-----------------#
rm(list=ls())
myheader<-c(
  "User-Agent"="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_3_1 like Mac OS X; ja-jp) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8G4 Safari/6533.18.5 ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")

temp<-getURL("http://t.dianping.com/list/guangzhou?q=%E7%94%B5%E5%BD%B1",httpheader=myheader)
write.table(temp,"c:/users/lenovo/desktop/temp.txt")   #问题！！！
k<-htmlParse(temp)  #解析
#下一步是写路径
#先把下下来的文件temp.txt打开，分析规律（直接看网页源代码比较方便）
getNodeSet(k,'//script[@type="text/javascript"]')  #路径写好后，就可以得到分类信息了
#<div class="tg-floor-item-wrap">
getNodeSet(k,'//div[@class="tg-floor-item-wrap"]')
#但得到的信息是list格式（要进行字符串处理，比较麻烦），进行unlist处理
sapply()   #字符串处理函数
youhui<-sapply(getNodeSet(k,'//div[@class="tg-floor-item-wrap"]'), xmlValue)
youhui
substr(youhui,1,2)<"\tab"
youhui[2]
strsplit(youhui,"\n")
list(youhui)
write.csv(youhui,"c:/users/lenovo/desktop/youhui1.csv")

youhui1<-lapply(youhui,function(x){x[5]})
youhui1
files<-unlist(files)
files<-files[-(1:2)]
youhui<-youhui[c(-1,-2,-3,-4)]
paste()


#翻页处理
#rul有奥妙http://t.dianping.com/list/guangzhou?q=%E7%94%B5%E5%BD%B1&pageIndex=1
#然后再构建一个循环函数
urllist=0   #存放多个URL
page<-1:5                       #paste函数把主要的url核心部分和page连在一起
urllist[page]<-paste("http://t.dianping.com/list/guangzhou?q=%E7%94%B5%E5%BD%B1&pageIndex=",page, sep="")
urllist
for (url in urllist) {
  k=htmlParse(url)
  youhui=sapply(getNodeSet(k,'//div[@class="tg-floor-item-wrap'), xmlValue)
}

for (i in length(files)) {
  temp<-getBinaryURL(url)
  #note<-file("hellodata.xls",open="wb",append=TRUE) 避免文件覆盖两种方法之一
  note <-file(paste("1202",files[i],sep = '.'), open = "wb")   #用wb打开
  writeBin(temp,note)                         #用temp和note写入
  close(note)
  Sys.sleep(2)
}




#--------------#
#----第三周----#
#--------------#

substr("abcdef", 2, 4)
substring("abcdef", 1:6, 1:6)
substr(rep("abcdef", 4), 1:4, 4:5)

# 字符串处理基础
# 赋值
# 长度、个数：nchar，length
# 替换：chartr（原始字符，替换字符，字符串）  #char-transform
# 连接：paste 参数sep，collapse（处理数组）    #sep=''无缝连接           
# 切割：strsplit        
# 比较：>、<、==、!=
#   并集、交集、补集：union，intersect，setdiff（setdifference）   A & B 交换位置的补集结果不同
# 截取：substr，substring
# 匹配：match 直接返回位置，完全匹配   pmatch（part），charmatch  部分匹配
#match(a,b) a 是否在b里面？
#python也是用这一套
# 正则表达式初识  
# \：转义字符   对特殊字符都赋予了新的含义，所以要加上转义字符
# .：除了换行以外的任意字符
# ^：一行字符串的起始
# $：一行字符串的结束
# *：零个或者多个之前的字符   sd*  sddddd
# +：一个或者多个之前的字符   sd+  sdsfsdadf   .+
# ?：零个或者一个之前的字符
# 保留字符都需要转义字符\来转义表示

#正则表达式初识
#方括号的新用法
# 方括号[]，代表可以匹配其中任何一个字符。而^在[]中代表“非”, -代表“之间”
# –[qjk]：q，j，k中任意一个字符
# –[^qjk]：非q，j，k的任意其它字符     （特殊，反义符号）
# –[a-z]：a至z中任意一个小写字符
# –[^a-z]：非任意一个a至z小写字符的其它字符（可以是大写字符）
# –[a-zA-Z]：任意一个英文字母
# –[a-z]+：一个或者多个小写英文字母
# |：或者
# 小括号() 把括号内当做一个整体    与花括号｛｝ 次数？             配合“|”使用
#  (sd)*  sdsdsd           d{1}:d   d{1,4}:d,dd,ddd,dddd   d{1,}: dddddddddddd
# 
# 正则表达式初识
# 常用的特殊转义字符含义
# \n：换行符         new line?
# \t：tab
# \w：任意字母（包括下划线）或者数字即[a-zA-Z0-9_]
# \W：\w的反义即[^a-zA-Z0-9_]
# \d：任意一个数字即[0-9]        （挺常用） digit数字
# \D：\d的反义即[^0-9]
# \s：任意一个空格，比如space, tab, newline 等
# \S：\s的反义，任意一个非空格

#-------------正则常用函数-----------------------#
# grepl：返回一个逻辑值 grep-logical
pattern="^[A-Za-z0-9\\._%+-]+@[A-Za-z0-9\\.-]+\\.[A-Za-z]{2,4}$" #邮箱地址   $要求一定以这个尾部结尾
#sunshine@163.com
list<-c("sunshine@163.com","hujiko","data@jik.kon","kjhfdh@ji.jikol")
grepl(pattern,list)
# grep：返回匹配的id


# 正则替换：sub和gsub      (globle-sub全局匹配的意思)   substitute
list1<-paste(list,collapse = ",")
list1
pattern2<-"\\w+@\\w+\\.[A-Za-z]{2,4}"
gsub(pattern2,"data",list1)


#regular-expression 正则表达式
# regexpr：返回一个数字，1表示匹配，-1表示不匹配，以及两个属性，匹配的长度和是否使用useBytes
regexpr(pattern,list)
# regexec：返回一个list，字符串中第一个匹配及其长度以及是否使用useBytes
regexec(pattern,list)    #[[1]]显示是list格式
# gregexpr：返回一个list，每一个匹配及其长度以及是否使用useBytes  ---global regular-
# 


#------新浪财经-----#
rm(list=ls())
myheader<-c(
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8,ISO-8859-1,GBK;q=0.7,*;q=0.7"
)
url<-"http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/603000.phtml?year=2018&jidu=1"
temp<-getURL(url,header="myheader")
k<-strsplit(temp,"\r\n")[[1]]   #一般网页切割
k[1:10]
grep("<a target='_blank'",k)
timeadr=k[grep("<a target='_blank'",k)+1]   #grep返回匹配的id   +1后一行
timeadr
time<-substring(timeadr,4,13)
time
#gsub-----#可以把 "2018-01-02" 中的- 替换成  .

fpriceadr=k[grep("<a target='_blank'",k)+3]     #+3 后面第三行
fpriceadr
fprice<-gregexpr(">\\d+",fpriceadr)                #\d 指数字， 加\表示转义
fprice   #27指>的位置
openprice<-substring(fpriceadr,28,33)
openprice
# 类似公式
# remove(i)
# for (i in 1:length(fpriceadr)){
#   tempp<-fprice[[i]]
#   fprices<-substring(fpriceadr[i],tempp+1,tempp+attr(tempp,'match.length')+3)
# }                  #attr返回字符串的长度 
# fprices


# 整合所有信息
aqq <- as.data.frame(cbind(industry,financing,companysize),row.names = c(1:15))
names(aqq) <- c("industry","financing","companysize")

# 添加到数据框中
bossdata <- data.frame(bossdata,aqq, row.names = c(1:length(bossdata[,1])))
bossdata
# 添加到数据框中
bossdata <- data.frame(bossdata,linkman,position)
bossdata
write.table(bossdata,file="bossdata.csv",sep=",",append=T,row.names = c(1:15),col.names = F)

#### 读取、整理数据 ####
shenzhendata <- read.table("bossdata.csv",sep=",")
shenzhendata <- shenzhendata[,2:12]
names(shenzhendata) <- c("companyname","titlename","salary","place","experience",
                         "education","industry","financing","copanysize","linkman","position")
shenzhendata


rm(list=ls())
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
  cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]

## Double all 'a' or 'b's;  "\" must be escaped, i.e., 'doubled'
gsub("([ab])", "\\1_\\1_", "abc and ABC")

txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")
( i <- grep("[gu]", txt) ) # indices
stopifnot( txt[i] == grep("[gu]", txt, value = TRUE) )

## Note that in locales such as en_US this includes B as the
## collation order is aAbBcCdEe ...
(ot <- sub("[b-e]",".", txt))
txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution

txt[gsub("g","#", txt) !=
      gsub("g","#", txt, ignore.case = TRUE)] # the "G" words

regexpr("en", txt)

gregexpr("e", txt)

## Using grepl() for filtering
## Find functions with argument names matching "warn":
findArgs <- function(env, pattern) {
  nms <- ls(envir = as.environment(env))
  nms <- nms[is.na(match(nms, c("F","T")))] # <-- work around "checking hack"
  aa <- sapply(nms, function(.) { o <- get(.)
  if(is.function(o)) names(formals(o)) })
  iw <- sapply(aa, function(a) any(grepl(pattern, a, ignore.case=TRUE)))
  aa[iw]
}
findArgs("package:base", "warn")

## trim trailing white space
str <- "Now is the time      "
sub(" +$", "", str)  ## spaces only
## what is considered 'white space' depends on the locale.
sub("[[:space:]]+$", "", str) ## white space, POSIX-style
## what PCRE considered white space changed in version 8.34: see ?regex
sub("\\s+$", "", str, perl = TRUE) ## PCRE-style white space

## capitalizing
txt <- "a test of capitalizing"
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt, perl=TRUE)
gsub("\\b(\\w)",    "\\U\\1",       txt, perl=TRUE)

txt2 <- "useRs may fly into JFK or laGuardia"
gsub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
sub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)

## named capture
notables <- c("  Ben Franklin and Jefferson Davis",
              "\tMillard Fillmore")
# name groups 'first' and 'last'
name.rex <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
(parsed <- regexpr(name.rex, notables, perl = TRUE))
gregexpr(name.rex, notables, perl = TRUE)[[2]]
parse.one <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ]
    substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
  }))
  colnames(m) <- attr(result, "capture.names")
  m
}
parse.one(notables, parsed)

## Decompose a URL into its components.
## Example by LT (http://www.cs.uiowa.edu/~luke/R/regexp.html).
x <- "http://stat.umn.edu:80/xyz"
m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
m
regmatches(x, m)
## Element 3 is the protocol, 4 is the host, 6 is the port, and 7
## is the path.  We can use this to make a function for extracting the
## parts of a URL:
URL_parts <- function(x) {
  m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
  parts <- do.call(rbind,
                   lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
  colnames(parts) <- c("protocol","host","port","path")
  parts
}
URL_parts(x)

## There is no gregexec() yet, but one can emulate it by running
## regexec() on the regmatches obtained via gregexpr().  E.g.:
pattern <- "([[:alpha:]]+)([[:digit:]]+)"
s <- "Test: A1 BC23 DEF456"
lapply(regmatches(s, gregexpr(pattern, s)),
       function(e) regmatches(e, regexec(pattern, e)))







