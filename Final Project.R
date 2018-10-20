# 10427137 黃鴻志
# 10427149 沈敬堯

#R語言期末project
#使用Rfacebook package從facebook上抓取內容
#在依照讚數及留言數使用ggplot2繪圖
# 2017/06/10
rm(list=ls())
starpath<-setwd("C:/Users/Mchig/Desktop/WORK/R/Finale Project")
# ==================================================================== #
if (!require(ggplot2)) #使用ggplot2繪圖
{
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(stringr))#使用strinr來計算類別次數
{
  install.packages("stringr")
  library(stringr)
}

if (!require(ggthemes))#使用ggthemes來設定繪圖背景
{
  install.packages("ggthemes")
  library(ggthemes)
}

if (!require(scales))#使用scales來繪圖
{
  install.packages("scales")
  library(scales)
}

if (!require(lubridate))#使用lubridate來找出月份
{
  install.packages("lubridate")
  library(lubridate)
}

if (!require(devtools))#基本package
{
install.packages("devtools")
library(devtools)
}


if (!require(Rfacebook))#使用Rfacebook來抓取FACEBOOK上資料
{
  install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
  library(Rfacebook)
}

if (!require(wordcloud))#使用wordcloud來畫出詞雲(一堆詞語組成的圈圈)
{
  install.packages("wordcloud")
  library(wordcloud)
}

if (!require(RColorBrewer))#使用RColorBrewer來調出需要顏色
{
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if (!require(jiebaR))#使用jiebaRR中文斷詞
{
  install.packages("jiebaR")
  library(jiebaR)
}
#**********************************************************需要使用的package**********************************
fb.oauth <- fbOAuth( #需要上網註冊
  app_id="108651039742945",
  app_secret="53355037e7581d40165d2aaaa8d634aa",
  extended_permissions = TRUE)

token <- 'EAACEdEose0cBADXTVj28WlCaSeS1spaNG9vaodvnrUzi8p8StLPWR9weZCPSMAcQAylMTZCZBZBGs8aQRQeo4ivyZA5Psd1lfZAwMLFh9Ll5pzJKoXaXU2c49m5fxBBiAZCbFWMIDKmF4fbP5KGfGXFqt5b0N0iDIIyCZBf5FyZBoWQZCusbmXiVk7FZCvegeFT3FsZD'
#依步驟1.點選Graph API 測試工具網站 "https://developers.facebook.com/tools/explorer"
#      2.選取"取得權杖" 再點選需要的權限最後再按"取得存取權杖"
#      3.複製"存取權杖"
#**********token期限為兩小時，過期及失效*********************************************
me <- getUsers("me", fb.oauth, private_info = TRUE) #取得用戶名(自己的FACEBOOK)

page.id <- "allstreetwalker" # 請輸入你要的紛絲專業名稱
page <- getPage(page.id, fb.oauth, n = 200,since='2016/12/01', until='2017/5/31')
#這邊的1.n及為文章篇數
#      2.since從何時開始,util何時結束

## aggregate metric counts over month
## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

aggregate.metric <- function(metric) { #取出月份加入like, comment, share的次數到一個list
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
# visualize evolution in metric

平均每月的三項指標<-ggplot(df, aes(month, y = x,group = metric))+
       geom_line(aes(color = metric), position="identity") + scale_y_log10("Average count per month", 
       breaks = c(5, 100, 100, 1000,10000,50000))+theme_bw() + theme(axis.title.x = element_blank()) +
       labs(title="平均每月的三項指標",colour = "三項指標") 
ggsave("平均每月的三項指標.png") #以png檔格式輸出圖片
cls_m_data<-aggregate(cbind(likes_count, comments_count,shares_count) ~ month, page, mean)
write.csv(cls_m_data, file = "平均每月的三項指標.csv")


#*************************************************輸出每月的三項指標(圖形)***************************************
getLink <- function( id) { #利用id找出貼文網址
  
  return   (browseURL(page$link[which(page$id == id)]))
}

getLink <- function( id,u) {#利用id找出list中網址
  
  return   (browseURL(u$link[which(u$id == id)]))
}

getWebsite_id <- function(id) { #利用id找出網址
 return ( browseURL( paste("https://www.facebook.com/",id,sep="") ) )
}

getWebsite <- function(name,u) { #利用name找出list中網址
  temp<-u$id[which(u$names == name)]
  return ( browseURL( paste("https://www.facebook.com/",temp[1],sep="") ) )
}

getComment_id <- function(name) { #利用name找出回復的中網址
  return (replies$id[which(replies$from_name == name)] )
}

#**************************************************取得網址*******************************************
page<-page[order(page$month,page$likes_count,page$comments_count,decreasing=TRUE),] #將貼文按照讚數及留言數排序
month_level<-levels(as.factor(page$month))
getTop <-function( count) { #取得每個月的前幾名按數數及留言數
  
  for( x in c( 1:length(month_level))){
    if ( x == 1)
      temp<-length(which(page$month == month_level[x])) 
    else 
      temp <- c(temp,length(which(page$month == month_level[x])) )
  }
  
for( x in c( length(month_level):1 )) {
  

  
  if ( identical(which(temp <count),integer(0)) == TRUE ) {
    if ( x == length(month_level) ) {
      cls_top<-page[which(page$month == month_level[x])[1:count],]
    }
    else {
      cls_top<-rbind(cls_top,page[which(page$month == month_level[x])[1:count],])
    }
  }
  
  else {
    return ("Too Big")
  } #else
  
  if ( x == 1 ) {
    cls_top<-cbind(cls_top$id,cls_top$likes_count,cls_top$comments_count,cls_top$link,cls_top$month)
    cls_top<-`colnames<-`(cls_top,c("id","like_count","comments_count","link","date"))
    write.csv(cls_top, file = "每月讚數及留言數前幾名.csv")
    return (cls_top)
  }#if
}#for
}#getTop
a<-getTop(10) #function test
#*************************************************輸出每月讚數及留言數前幾名標(csV)***************************************

getKey<-function( key,i ) {#輸入關鍵字找出，找是否在貼文其中有就按照讚數及留言數輸出
if (   identical(grep(key,page$message ),integer(0))  == FALSE ) {
  key_top<-page[grep(key,page$message),]
  key_top<-key_top[order(key_top$likes_count,key_top$comments_count,decreasing=TRUE),] #貼文按照讚數留言數排序
  g<-ggplot(key_top, aes(month, fill=likes_count)) + geom_bar(position="dodge") +theme_economist() + scale_colour_economist()
  key<- str_replace_all(toString(c( "關鍵字 : ","[", key,"]","出現的月份 ","總共出現",length(grep(key,page$message )),"次" )),", ", "")
  
  (關鍵字每個月出現次數 <- g + labs(title=key, x="月份", y="類別出現次數"))
  關鍵字每個月出現次數
  ggsave("關鍵字每個月出現次數.png") #以png檔格式輸出圖片
  cat("已以Png檔形式輸出每個月關鍵字出現次數\n")
  
  if (  i > nrow(key_top) ) {
    cat("輸入值大於資料比數!將值設為資料筆數",nrow(key_top)) 
    i<-nrow(key_top)
  }
  
  key_top<-key_top[1:i,]
  key_top<-cbind(key_top$id,key_top$likes_count,key_top$comments_count,key_top$link,key_top$month)
  key_top<-`colnames<-`(key_top,c("id","like_count","comments_count","link","date"))
  write.csv(key_top, file = "關鍵字出現次數.csv")
  cat("已以CSV檔輸出關鍵字出現次數") 
  return (key_top)
}else {
  return (cat("輸入值並不在資料當中"))
}
}#getKey

getKey("臺南",10) #function Test
#***************************************輸入關鍵字輸出相關資料*************************************************************
#my_friends <- getFriends(token, simplify = TRUE)#必須朋友也有使用application
#mat <- getNetwork(token, format = "adj.matrix")#必須朋友也有使用application
#getInsights(page, token=page_token, metric = "page_impressions")#需要是紛絲專業主人才可使用
#

#getGroup(group_id=108074295885646, token=fb.oauth)#find fanpages
#************************************不能用的函示*****************************************************************
for ( x in c(1:nrow(page))) {#取得所有按讚數的名字及ID
  if ( x == 1 ) {
    Like<-getLikes(page$id[x], token = fb.oauth) #取出貼文讚數
  }
  else {
    Like<-rbind(Like,getLikes(page$id[x],token =  fb.oauth))
  }
}

Like_top<-aggregate(data.frame(count = Like$names), list(value = Like$names), length) #將Like中的名字及讚數取出
Like_top<-Like_top[order(Like_top$count,decreasing=TRUE),]#按照讚數大小排序


getLike<- function( count){ #輸入想要看的前幾名 輸出讚數排行榜的PNG及CSV
  if (  count > nrow(Like_top) ) {
    cat("輸入值大於資料比數!將值設為資料筆數",nrow(Like_top)) 
    count<-nrow(Like_top)
  }
  
  
  Like_top<-Like_top[1:count,]
  Like_top<-`colnames<-`(Like_top,c("names","count"))
  write.csv(Like_top, file = "按讚出現次數.csv")
  cat("已以CSV檔輸出按讚出現次數\n")
  讚數排行榜<-ggplot(Like_top, aes(x = names, y = count))  + geom_point(size = 4) + scale_x_discrete(labels = abbreviate) + theme_hc() + scale_color_hc()
  ggsave("讚數排行榜.png")
  return (Like_top)
  
}
a<-getLike(10)#輸入你要的大小 functionTest
getWebsite(a[5,][[1]],Like)#請選擇改變第一個陣列的值
#**************************找出讚數前幾名***************************************************************************************
## Downloading list of replies to first comment
for ( x in c(1:nrow(page))) { #把所有留言取出
 if ( x == 1 ) {
    post <- getPost(post=page$id[x], n=2000, token=fb.oauth) #取出該貼文
    replies<-post$comments #將貼文中的留言放入replies內
  }
  else {
    post <- getPost(post=page$id[x], n=2000, token=fb.oauth)
    replies<-rbind(replies,post$comments)#做row合併
    
  }
}

Replies_top<-aggregate(data.frame(count = replies$from_name), list(value = replies$from_name), length) #將replies中的名字與數量加入Replies_top
Replies_top<-Replies_top[order(Replies_top$count,decreasing=TRUE),] #按照名字出現次數排好

getReplies<- function( count){#找出留言的前幾名
  if (  count > nrow(Replies_top) ) {
    cat("輸入值大於資料比數!將值設為資料筆數",nrow(Replies_top)) 
    count<-nrow(Replies_top)
  }
  
  
  Replies_top<-Replies_top[1:count,]
  Replies_top<-`colnames<-`(Replies_top,c("names","count"))
  write.csv(Replies_top, file = "評論出現次數.csv")
  cat("已以CSV檔輸出評論出現次數\n")
  評論排行榜<-ggplot(Replies_top, aes(x = names, y = count))  + geom_point(size = 4) + scale_x_discrete(labels = abbreviate) + theme_hc() + scale_color_hc()
  ggsave("評論排行榜.png")
  return (Replies_top)
  
}

a<-getReplies(10) #function test

a<-getComment_id(a[,1][2]) # 請輸入getReplies裡的名字，會得此名字的id在所有的回覆中 PS:請改變第二個陣列內的植 function test
getWebsite_id(a[1]) #將得到到的id輸入 輸出此id相連的網址
#*****************************************************評論出現前幾名******************************************************


#將R環境設定成中文
Sys.setlocale(category = "LC_ALL", locale = "cht")
#在 worker() 內可以設定各種不同的全切分法模型與引用外部詞庫，在這裡直接使用預設的全切分法的混合模型，與 jieba 自帶的詞庫。
cc = worker()

for(x in c(1:nrow(replies))) { #取出所有留言中的中文字片段
  if ( x == 1) {
    try(    Comment<-cc[replies$message[x]], silent = TRUE) #try為跳過錯誤訊息 cc[[]]為切片斷

  } else {
    try(        Comment<-c(Comment,cc[replies$message[x]]), silent = TRUE)

  }
}

Comment_top<-aggregate(data.frame(count = Comment), list(value = Comment), length) #算出字源出現過幾次
Comment_top<-Comment_top[order(Comment_top$count,decreasing=TRUE),] #按照字源出現次數排好大小
  
getComment<- function( count,top){ #輸入count想要找的字元前幾名 top劃出的字元大於幾筆
  
  d <- data.frame(word = Comment_top$value, freq = Comment_top$count )
  
  評論關鍵字排行榜<-wordcloud(d$word, d$freq, min.freq = top, random.order = F, ordered.colors = F, 
                    colors = rainbow(length(row.names(Comment_top))))
  ggsave("評論關鍵字排行榜.png")
  
  if (  count > nrow(Comment_top) ) {
    cat("輸入值大於資料比數!將值設為資料筆數",nrow(Comment_top)) 
    count<-nrow(Comment_top)
  }
  
  write.csv(Comment_top, file = "評論關鍵字排行榜.csv")
  cat("已以CSV檔輸出評論關鍵字排行榜\n")
  return( Comment_top ) 
}
a<-getComment(10,100)
#*********************************評論關鍵字排行榜*************************************
  




