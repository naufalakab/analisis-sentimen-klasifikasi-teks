library(twitteR)
library(ROAuth)
library(tm)
library(rtweet)
library(wordcloud2)
library(wordcloud)

setup_twitter_oauth("dTiHemLMdX2QIiTgu5tUzAut9","1a8IzDpsAAd58aGB5gWdQIjTVdingyRtpKwbmvC7WMH0YdcKMf","2634677696-jxeYl0u8vrvoC36f2vH1fxv6CIe41a9kIIiiXbP","SMmCUIoxfiMmYG0LtHp8RmLdQq2yFLLTrhk1u7b48bG4t")

#ambil data dari twitter
data_twitter<-searchTwitter("vaksin", n=1000, lang= "id", retryOnRateLimit = 10e3)
View(data_twitter)

setwd(dir = "E:\\adam\\kuliah\\sem 5\\prak DS\\New folder\\PROJECT DS")

#save data mentah
saveRDS(data_twitter,file = 'data_mentah_vaksin.rds')

#load data set
data_twitter<-readRDS('data_mentah_vaksin.rds')
df_twitter=twListToDF(data_twitter) ##jadi dataframe
View(df_twitter)



#lihat data mentah
View(df_twitter)

#cleaning data
#hanya ambil tweet saja
komen<-df_twitter$text
komenc<-Corpus(VectorSource(komen))

#hapus tanda baca, link url, huruf aneh, dan emoji
removeURL <-function(x) gsub("http[^[:space:]]*", "", x)
twitclean <-tm_map(komenc,removeURL)

removeRT<-function(y) gsub("RT", "", y)
twitclean<-tm_map(twitclean,removeRT)

removeUN<-function(z) gsub("@\\w+", "", z)
twitclean<-tm_map(twitclean,removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)

twitclean<-tm_map(twitclean, removePunctuation)
twitclean<-tm_map(twitclean, tolower)

removeVaksin<-function(x) gsub("vaksin", "", x)
df_twitter_new<-tm_map(twitclean,removeVaksin)

#membuat nilai untuk masing-masing kata
{
  dtm<-TermDocumentMatrix(df_twitter_new)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  df_twitter_new<-data.frame(word=names(v),freq=v)
}
head(df_twitter_new,n=10)

set.seed(1234) # for reproducibility 
wordcloud(words = df_twitter_new$word, 
          freq = df_twitter_new$freq, 
          min.freq = 1,           
          max.words =200, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

## save data
dataframe<-data.frame(text=unlist(sapply(twitclean,'[')),stringsAsFactors = F)
View(dataframe)

write.csv(dataframe,file = 'data-bersih_vaksin_1.csv')
