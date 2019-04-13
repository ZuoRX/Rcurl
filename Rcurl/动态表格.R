
library(magrittr)

http://www.77tj.org/css/css.min.css?v=3NsnAME8GfMnnQPLWd3CkR3KJtcaEou7QkiNBFOj4gQ
http://www.77tj.org/css/css.min.css?v=3NsnAME8GfMnnQPLWd3CkR3KJtcaEou7QkiNBFOj4gQ

remDr <- remoteDriver(browserName = "chrome")
remDr$open()
#url<-"http://www.csteelnews.com/xwzx/xydt/201711/t20171129_356623.html"
url<-"http://www.77tj.org/"
remDr$navigate(url)
web <- read_html(remDr$getPageSource()[[1]][1])

data_temp <- html_table(web, fill = T)


url1<-web %>% html_nodes(xpath = "//div/input/") %>%html_attr("title")


