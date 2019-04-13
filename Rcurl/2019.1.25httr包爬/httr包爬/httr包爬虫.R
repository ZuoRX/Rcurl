
library(httr)
library(rvest)
library(tcltk)
library(progress)
#library(tidyverse)

#====================#
#=====微信好友爬=====#
#====================#
#get请求案例

#传入微信cookie信息
Cookie <- "RK=GMVWyK8bld; tvfe_boss_uuid=db2557df6d970ec9; pac_uid=1_2286747332; pvid=7628586552; flv=25.0 r0; AMCV_248F210755B762187F000101%40AdobeOrg=-1891778711%7CMCIDTS%7C17734%7CMCMID%7C12409515081908024712323869212451902083%7CMCAAMLH-1526914286%7C11%7CMCAAMB-1532176839%7CRKhpRz8krg2tLO6pguXWp5olkAcUniQYPHaMWWgdJ3xzPWQmdj0y%7CMCOPTOUT-1532184061s%7CNONE%7CMCSYNCSOP%7C411-17673%7CMCAID%7C2D7612D0052A1F0F-6000012A8003D0FE%7CvVersion%7C2.4.0; o_cookie=2286747332; ptcz=c7f6c780828590a58d25ff4cce09508d880dc697d929b1ec4e011c039abc4177; pgv_pvi=5668365312; pgv_pvid=7628586552; mm_lang=zh_CN; MM_WX_NOTIFY_STATE=1; MM_WX_SOUND_STATE=1; wxuin=1251823703; webwxuvid=c7197fcb004df4537947fc2aa813d811f98a8bc81076146c35c99a41d9a2eaac1d3157f4aeabc1874c45606c613fc287; last_wxuin=1251823703; wxsid=9Ri1td7syPCp7Er6; webwx_data_ticket=gSfgEmuSggcaQmsxGD1hHtTj; webwx_auth_ticket=CIsBEMSo6oEEGoABPtdOhv3h6CiHAh//Uvot+g7PalWG7hioU96QHv5sAA92vqdT1SS7ZsjkMUR3DgTtezIPvHF8ub7TgjeHsCoGjmj0oWKfqfqex/pAH9en3tRK7Oe/zCFO7nZeWlVh8lwOvpDVBvjDEJyODFlS8v/dothQE5L9HVxDtO60lQhwoPY=; login_frequency=2; wxloadtime=1547456504_expired; wxpluginkey=1547455561"
#构造请求头
headers <- c('Accept'='application/json',
             'Content-Type'='text/plain', 
             'User-Agent'='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36',
             'Referer'='https://wx.qq.com/',
             'Connection'='keep-alive',
             'cookie'=Cookie
)

#实际请求的url
url<-"https://wx.qq.com/cgi-bin/mmwebwx-bin/webwxgetcontact?r=1547456583474&seq=0&skey=@crypt_37a10742_3247b62bd18b5376d38a95835dd4e242"

#执行请求
louwill <- GET(url,add_headers(.headers =headers))
weixin<-content(louwill)

temp<-gettext(weixin)
#temp1<-gettext(temp)
k2<-strsplit(temp,"\n")[[1]]
head(k2,100)

#微信昵称
name<-k2[grep(pattern="NickName",k2)]
name<-gsub("\"NickName\": \"|\",","",name)

#备注姓名
label<-k2[grep(pattern="RemarkName",k2)]
label<-gsub("\"RemarkName\": \"|\",","",label)

label1<-k2[grep(pattern="RemarkPYInitial",k2)]
label1<-gsub("\"RemarkPYInitial\": \"|\",","",label1)

label2<-k2[grep(pattern="RemarkPYQuanPin",k2)]
label2<-gsub("\"RemarkPYQuanPin\": \"|\",","",label2)


#性别
sex<-k2[grep(pattern="Sex",k2)]
sex<-gsub("\"Sex\": |,","",sex)

#省份
province<-k2[grep(pattern="Province",k2)]
province<-gsub("\"Province\": \"|\",","",province)

#城市
city<-k2[grep(pattern="City",k2)]
city<-gsub("\"City\": \"|\",","",city)

#签名
signature<-k2[grep(pattern="Signature",k2)]
signature<-gsub("\"Signature\": \"|\",","",signature)


wei<-data.frame(name,label,label1,label2,sex,province,city,signature)
write.csv(wei,"c:/users/lenovo/desktop/weixin.csv")

###################微信分析################
weixin<-read.csv("c:/users/lenovo/desktop/weixin.csv")

#删除群聊
weixin$province[weixin$province==""]<-NA
weixin1<-weixin[!is.na(weixin$province),]

#删除公众号
weixin1$label[weixin1$label==""]<-NA
weixin2<-weixin1[!is.na(weixin1$label),]


#地域统计
place<-table(weixin2$province)







#====================#
#=====网易云课堂=====#
#====================#
#get请求案例

#传入cookie信息
Cookie <- "_ntes_nuid=6e185dffcc46e18f5253bcddd0518e7d; vjuids=11bb4e5580.15fb477d792.0.b2a9983fca025; _ga=GA1.2.1852744098.1509796114; P_INFO=hu_ql@163.com|1517297315|0|other|10&16|sxi&1517246098&mail163#sxi&610500#10#0#0|182360&0|mail163|hu_ql@163.com; __oc_uuid=160edaf0-c156-11e7-999f-97e5db0b5968; _ntes_nnid=6e185dffcc46e18f5253bcddd0518e7d,1540473601823; __utma=187553192.1852744098.1509796114.1530791989.1540473638.3; __utmz=187553192.1540473638.3.2.utmcsr=c.open.163.com|utmccn=(referral)|utmcmd=referral|utmcct=/search/search.htm; usertrack=ezq0o1vTyPJHB0aQJrCJAg==; vjlast=1510561274.1541771091.11; vinfo_n_f_l_n3=72d597210f451c2b.1.23.1510561273831.1540627012884.1541771297412; NTESSTUDYSI=9b320ce00ea6492e9bbf5ec417aee094; EDUWEBDEVICE=e061bbd0773a4567ae377660e4f5bdbf; utm=eyJjIjoiIiwiY3QiOiIiLCJpIjoiIiwibSI6IiIsInMiOiIiLCJ0IjoiIn0=|aHR0cDovL3d3dy5zby5jb20vbGluaz9tPWFMeEE5UEhXMjE0cWFRZDM3bURRb2VnWXkySEFDekN4cGVVSTIlMkJnRSUyRjk4bm0yU3k1VXlMWE5IV1hIcGRTb0dHRm14dXZLOTBaaGQzZ2Zlc1RDbk0xeWZ3ayUyQjFVM0UwWjNpQUpiUGdmWjNkSm5SUmtqdzR3V0ElMkIlMkJQOFclMkJIaVBzQQ==; eds_utm=eyJjIjoiIiwiY3QiOiIiLCJpIjoiIiwibSI6IiIsInMiOiIiLCJ0IjoiIn0=|aHR0cDovL3d3dy5zby5jb20vbGluaz9tPWFMeEE5UEhXMjE0cWFRZDM3bURRb2VnWXkySEFDekN4cGVVSTIlMkJnRSUyRjk4bm0yU3k1VXlMWE5IV1hIcGRTb0dHRm14dXZLOTBaaGQzZ2Zlc1RDbk0xeWZ3ayUyQjFVM0UwWjNpQUpiUGdmWjNkSm5SUmtqdzR3V0ElMkIlMkJQOFclMkJIaVBzQQ==; __utma=129633230.1852744098.1509796114.1547516694.1547516694.1; __utmc=129633230; __utmz=129633230.1547516694.1.1.utmcsr=so.com|utmccn=(referral)|utmcmd=referral|utmcct=/link; STUDY_UUID=f46212dd-8185-46a3-b2c3-6b52ef9ac327; STUDY_SESS='LmAGFCq3wuoHQvm1srygVhmVS41br/FfLwjvoGnhyO8rYmh1QQD6bJOSsO3qZ/jHDghadqui3SqBXKi1Dq37pyRPq6gSBklRne31xyq0bvLE+LuuoBSkz0id2I1M+flM191lqa7z1T1EdBMcSOB2Iv5Px/MAUcKGHwxqAm0hnUzKR2DeJMdhwENkua9AAfwP+P6MxCmnJEvne6pPMc9TTJJnThNrM7aj0X5LVpSBvjalyjQSI5bR6ne1ZuAvid6s2hhU23eeU6IpJWzONxdaRP2b8FjJLNw7DOKna6+A16BKlOh/Gwx6G1S/X4FQ7qd/icKMg/BGgjdZ/c2Zef6mQaEeckOtfKrVY81HqqNnP22vOBSapF/UqV9N8LdNPSNv'; STUDY_INFO=UID_FF6EE3E2080E586B0C2258B41C7729DC|4|1384035587|1547516782338; STUDY_MIND_TELBIND=1; NETEASE_WDA_UID=1384035587#|#1547516782240; NTES_STUDY_YUNXIN_ACCID=s-1384035587; NTES_STUDY_YUNXIN_TOKEN=84e18950ce4354316ddbbfcc6c32f039; STUDY_MIND_TELBIND_CLOSE=1; __utmb=129633230.11.7.1547516711275"
#构造请求头
headers <- c('Accept'='application/json',
             'Content-Type'='application/json', 
             'User-Agent'='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36',
             'Referer'='https://study.163.com/?from=study',
             'cookie'=Cookie
)

#实际请求的url
url<-"https://study.163.com/j/web/fetchPersonalData.json?categoryId=-1&t=1547516872685"

#执行请求
yun <- GET(url,add_headers(.headers =headers))
temp<-content(yun)


k2<-temp$result
head(k2[[1]])

#微信昵称
name<-k2[grep(pattern="moduleName",k2)]

##########还没有找到数据清洗的规律


#==============#
#=====微博=====#
#==============#
#post请求案例

#传入cookie信息
Cookie <- "SINAGLOBAL=7370051007409.983.1508932388378; login_sid_t=2450bd39106f06b1615904a4e6206e99; cross_origin_proto=SSL; _s_tentry=-; Apache=9184161138542.883.1547518579153; ULV=1547518579166:25:2:1:9184161138542.883.1547518579153:1546513631595; crossidccode=CODE-tc-1GJehD-2hby6j-zjl6Cw8ieIKEK8bfbf0d1; UOR=blog.csdn.net,widget.weibo.com,graph.qq.com; SCF=AlyGn9PJPI_OGq5tAR-DLHYWS8ltLMYCAGfmr-t1cYmqy3dC41R6Opq03R8MJ00WiByf4p9C6MGMc5_5hGaCyVY.; SUB=_2A25xOTKKDeThGeBG4lYW9CnIyjuIHXVSTyNCrDV8PUNbmtAKLVX7kW9NQfbKPAUdD8QFp36l3C9PfdQ_uMzuv8-f; SUBP=0033WrSXqPxfM725Ws9jqgMF55529P9D9WhFg1.usza6-R8_4GU6Lvei5JpX5KzhUgL.FoqR1KBNShMXeKM2dJLoIp-LxKBLB.zLB.zLxK-L1KzLBonRSo.t; SUHB=0N2lMhJ4J1bsU8; ALF=1579054682; SSOLoginState=1547518682; wvr=6; TC-Page-G0=784f6a787212ec9cddcc6f4608a78097; wb_view_log_6894747417=1920*10801"
#构造请求头
# headers <- c('Accept'='application/json',
#              'Content-Type'='application/json', 
#              'User-Agent'='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36',
#              'Referer'='https://study.163.com/?from=study',
#              'cookie'=Cookie
# )

headers <- c('Accept'='*/*',
             'Content-Type'='application/x-www-form-urlencoded',
             'User-Agent'='ozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.221 Safari/537.36 SE 2.X MetaSr 1.0',          
             #'edu-script-token'= '37aa682d1473455c8a77e6a4476e8f9e',
             'Referer'='https://d.weibo.com/', 
             'Connection'='keep-alive',
             'cookie'=Cookie
)
#POST请求需要构造请求头表单参数
payload<-list(
  'pageIndex'=1,
  'pageSize'=50, 
  'relativeOffset'=0,
  'frontCategoryId'=-1
)



#实际请求的url
url<-"https://study.163.com/j/web/fetchPersonalData.json?categoryId=-1&t=1547516872685"

#执行请求
yun <- GET(url,add_headers(.headers =headers))
temp<-content(yun)



#==============#
#=====奇趣统计=====#
#==============#
#post请求案例

#传入cookie信息
Cookie <- "__cfduid=d34f26932c00adda2de1d6fcd367e12621542674597; .AspNetCore.Antiforgery.nlUjaxz2h3Y=CfDJ8HunFzV1EulOq9mLop52iQmzTWo3XBgmwIzDYJs35NG8hjlqKIJYbbWsCxyGL2ThyuVyNSVakvOnbqp4ixQhijch1QLVuAVIQX_RvLLw1WU2Savd9UfBtDo8UX2YFRUFifbQqkuFV7S8G3PMVbA3gcQ"
#构造请求头
# headers <- c('Accept'='application/json',
#              'Content-Type'='application/json', 
#              'User-Agent'='Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36',
#              'Referer'='https://study.163.com/?from=study',
#              'cookie'=Cookie
# )

headers <- c('Accept'='*/*',
             'Content-Type'='application/x-www-form-urlencoded',
             'User-Agent'='ozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.221 Safari/537.36 SE 2.X MetaSr 1.0', 
             '__RequestVerificationToken'='CfDJ8HunFzV1EulOq9mLop52iQk_ExfWRZ2y0vcbZUBkcbN_Bage2F9wY8Ha9ZkNp959QawFhKQIk87BXYPln_mUrCL22LfQplyTnC9HlrOnBHS-H3zC2-aGNSFAOg0KrrvPIm2ipm8CJ0fLpxvPgSxuO2g',
             'Referer'='http://www.77tj.org/', 
             'Connection'='keep-alive',
             'cookie'=Cookie
)
#POST请求需要构造请求头表单参数
payload<-list(
  'pageIndex'=2,
  'pageSize'=50
)



#实际请求的url
url<-"http://www.77tj.org/"

#执行请求
qiqu <- POST(url,add_headers(.headers =headers),body =payload)
qiqu$content
temp<-content(qiqu)
temp[[2]]


#================#
#=====拼多多=====#
#================#
#post请求案例

#传入cookie信息
Cookie <- "api_uid=rBQh+VxK2DASHS4eBTXJAg=="

headers <- c('Accept'='*/*',
             'Content-Type'='application/json;charset=UTF-8',
             'User-Agent'=' Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.132 Safari/537.36', 
             'AccessToken'= 'V55GGNB7CID6N7SLYRZMN5KACG5GEUZPA3Q77PPGX6RCT3T5VHCQ101fea4',
             'Referer'='http://mobile.yangkeduo.com/search_result.html?search_key=iphone&search_src=new&search_met=btn_sort&search_met_track=manual&refer_page_name=login&refer_page_id=login_1548420861948_JHAlYvOTai&refer_page_sn=10031&page_id=10015_1548409047344_W2E0g3go9j&list_id=Y28n4qMACM&flip=40%3B1%3B0%3B20%3B1da2efab-ae5c-4ddf-9029-2293319b4ab9&item_index=21&sp=6136&is_back=1', 
             'Connection'='keep-alive',
             'cookie'=Cookie
)
#POST请求需要构造请求头表单参数
payload<-list(
  'pageIndex'=1,
  'pageSize'=40
)



#实际请求的url
url<-"http://apiv3.yangkeduo.com/api/espresso/nears_groups?pdduid=4919732680190&is_back=1&__json=1"

pin <- POST(url,add_headers(.headers =headers),body =payload, encode="json")
pin$list[[1]]
pin
temp<-content(pin)
temp[[2]]























