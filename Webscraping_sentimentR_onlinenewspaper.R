##########################################################################################
# PROJECT 
# WEBSCRAPING & SENTIMENTAL ANALYSIS
##########################################################################################

##########################################################################################
# OBJECTIVES OF THE PROJECT

# Scrape online news paper business session and analyze the sentiment
##########################################################################################


##########################################################################################
# Libraries used for the project
##########################################################################################

library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(lubridate)
library(sentimentr)
library(ggplot2)

##########################################################################################
# Store website link 
##########################################################################################

link="https://www.khaleejtimes.com/business"
page=read_html(link)

##########################################################################################
# Find all heading published in online newspaper business session
##########################################################################################

headings=page %>% html_nodes(".post-title a") %>% 
 html_text(trim = T) %>% 
  str_squish()

##########################################################################################
# Find href attribute of all news content
##########################################################################################

href_headings=page %>% html_nodes(".post-title a") %>% 
  html_attr('href' )


newscontent=c()
newsheading=c()
newsdate=c()
sleep_key=runif(length(href_headings),2,2.75)

for (i in 1:length(href_headings)){
    page_head=read_html(href_headings[i])
    
    t=ifelse(
        length(str_squish(html_text(html_nodes(page_head,".article-paragraph-wrapper"),trim = T)))==0,"no content",
        str_squish(html_text(html_nodes(page_head,".article-paragraph-wrapper"),trim = T))
      )
    dt=ifelse(
      length(str_squish(html_text(html_nodes(page_head,".article-top-author-nw-nf-right p"),trim = T)))==0,"no content",
      str_squish(html_text(html_nodes(page_head,".article-top-author-nw-nf-right p"),trim = T))
    )
    
    newsdate=append(newsdate,dt)
    newscontent=append(newscontent,t)
    newsheading=append(newsheading,headings[i])
    
    
   
    Sys.sleep(sleep_key)
    
}

newsdate=str_squish(substr(newsdate,15,26)) 
df=data.frame(newsdate,newsheading,newscontent,size=nchar(newscontent))
df$newsdate=removePunctuation(df$newsdate)
df$newsdate=dmy(df$newsdate)

##########################################################################################
# Find the number of positive and negative sentimets available in online news paper
##########################################################################################


df %>% get_sentences() %>% sentiment_by() %>% filter(newscontent!="no content") %>% 
        mutate(senti_business=ifelse(ave_sentiment>0,"Positive",ifelse(ave_sentiment<0,"Negative","Neutral"))) %>% 
        group_by(senti_business) %>% 
        summarise(n=n()) %>% 
        ggplot(aes(senti_business,n))+
        geom_bar(stat="identity")+
        labs(title = "Sentiment Analysis of Khaleej Times news paper Business Edition",
             y="No. of Positive/negative sentiment",
             x="Sentiment")

##########################################################################################
#        SENTIMENT ANLAYSIS - POSITIVE OR NEGATIVE BASED ON DATE
##########################################################################################

df %>% filter(newscontent!="no content") %>% get_sentences() %>% sentiment_by(by=c("newsdate")) %>% 
  mutate(result=ifelse(ave_sentiment<0,"Negaive","Positive")) %>%
  ggplot(aes(as.character(newsdate),ave_sentiment))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))
  

##########################################################################################
#  ANALYZE THE EMOTIONS
##########################################################################################



df %>% get_sentences() %>% emotion() %>% 
  group_by(emotion_type) %>% 
  summarise(n=sum(emotion_count)) %>% 
  ggplot(aes(emotion_type,n))+
  geom_bar(stat = "identity")
  
  






