install.packages("rJava")
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
install.packages("tm")
install.packages("tmcn", repos="http://R-Forge.R-project.org", type="source")
install.packages("wordcloud")
install.packages("XML")
install.packages("RCurl")
install.packages("jiebaR")

if (!require(jiebaR))#�ϥ�ggthemes�ӳ]�wø�ϭI��
{
  #install.packages("jiebaR")
  library(jiebaR)
}

if (!require(wordcloud))#�ϥ�ggthemes�ӳ]�wø�ϭI��
{
  #install.packages("wordcloud")
  library(wordcloud)
}