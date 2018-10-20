install.packages("rJava")
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
install.packages("tm")
install.packages("tmcn", repos="http://R-Forge.R-project.org", type="source")
install.packages("wordcloud")
install.packages("XML")
install.packages("RCurl")
install.packages("jiebaR")

if (!require(jiebaR))#使用ggthemes來設定繪圖背景
{
  #install.packages("jiebaR")
  library(jiebaR)
}

if (!require(wordcloud))#使用ggthemes來設定繪圖背景
{
  #install.packages("wordcloud")
  library(wordcloud)
}