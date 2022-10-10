library(robotstxt)
library(rvest)
library(dplyr)

paths_allowed(paths = "https://www.propertyfinder.ae/en/rent/dubai/apartments-for-rent.html?page=1")


t_price=c()
t_type=c()
t_bed=c()
t_area=c()
t_loc=c()
t_cat=c()

link=paste0("https://www.propertyfinder.ae/en/rent/dubai/apartments-for-rent.html?page=",104:500)
 
for (i in link) {
   
    page=read_html(i)

    price=page%>% html_nodes(".card__price-value") %>% html_text()
    type=page %>% html_nodes(".card__property-amenity--property-type") %>% html_text()
    n.bed=page %>% html_nodes(".card__property-amenity--bedrooms") %>% html_text()
    #n.bath=page %>% html_nodes(".card__property-amenity--bathrooms") %>% html_text()
    area=page %>% html_nodes(".card__property-amenity--area") %>% html_text()
    loc=page %>% html_nodes(".card__location-text") %>% html_text()
    cat=page %>% html_nodes(".card__tag--top-corner") %>% html_text()



    t_price=c(t_price,price)
    t_type=c(t_type,type)
    t_bed=c(t_bed,n.bed)
    # t_bath=c(t_bath,n.bath)
    t_area=c(t_area,area)
    t_loc=c(t_loc,loc)
    t_cat=c(t_cat,cat)

    print(i)
    Sys.sleep(12)
}

df=data.frame(t_price,t_type,t_bed,t_area,t_loc,t_cat)
# Structure of dataset

str(df)
head(df)

# remove /n from t_price

df$t_price=gsub(" ","",df$t_price)
df$t_price=gsub("\n","",df$t_price)
df$t_price=gsub(",","",df$t_price)
df$t_price=gsub("AED/year","",df$t_price)


# Change data type

sum(is.na(df$t_price)==T)
df$t_price=as.numeric(df$t_price)


df=df %>% separate(t_loc,into = c("loc1","loc2","loc3","loc4"),sep = ",",fill = "left")

# Select required variables only
df=df %>% select(t_price,t_type,t_bed,t_area,loc2,loc3,t_cat)

# Rename variables 
df=df %>% rename(price=t_price,
                 type.residential=t_type,
                 no.beds=t_bed,
                 size.sq.meter=t_area,
                 apart.area.name=loc2,
                 area.name=loc3,
                 category=t_cat)

# Change size.sq.meter data type to numeric

df$size.sq.meter=gsub(",","",df$size.sq.meter)
df$size.sq.meter=gsub(" ","",df$size.sq.meter)
df$size.sq.meter=gsub("sqft","",df$size.sq.meter)

df$size.sq.meter=as.numeric(df$size.sq.meter)

master=rbind(master,df)

write.csv(master,file="propertyFinder.csv")

#-------------------------------------------------------------------------------
# DATA EXPLORATION
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Problem statement:

# I have a client they are planning to start a real estate business in Dubai and facing difficulty to fix the 
# price for their apartments. In this project we are trying to provide solution for the below mentioned questions.

# Apartment price based on the number of bedrooms and community
# 
# price and beds - completed
# price and sq.feet - completed
# bed and sq feet - completed
# bed and community -  completed
# sq feet and community -completed
# correlation of price - sq. feet based on bedrooms -compleated

# community - bed - price - completed
# community - bed - sq.feet - completed
# community - bed - sq.feet - completed





#-------------------------------------------------------------------------------
# LIBRARIES
#-------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
options(scipen=999)

#-------------------------------------------------------------------------------
# READ CSV FILE
#-------------------------------------------------------------------------------

df=read.csv("~/Desktop/tableau_projects/dubizzle/propertyFinder.csv")

#-------------------------------------------------------------------------------
# SUMMARY OF DATASET
#-------------------------------------------------------------------------------

summary(df)

#-------------------------------------------------------------------------------
# REMOVE NA VALUES FROM PRICE VARIABLE
# 117 NA are available in apart.area.name (not removed from data set )
#-------------------------------------------------------------------------------

df= df %>% filter(is.na(df$price)==F)   

#-------------------------------------------------------------------------------
# UNIVARIATE ANALYIS
#-------------------------------------------------------------------------------

# Price

df %>% ggplot(aes(price)) +
  geom_histogram(fill="royalblue",col="black",alpha=0.5 )

# no.beds

df %>% ggplot(aes(x=no.beds))+
  geom_bar(fill="royalblue",col="black",alpha=0.5)

# size.sq.meter
# remove outlier from size.sq.meter.
df=df[df$size.sq.meter!=64999,]

df %>% ggplot(aes(size.sq.meter)) +
  geom_histogram(fill="royalblue",col="black",alpha=0.5 )

# apart.area.name

df %>% group_by(apart.area.name) %>% 
  summarise(No=n()) %>% 
  arrange(desc(No)) %>% 
  top_n(50)%>% 
  ggplot(aes(apart.area.name,No))+
  geom_bar(stat = "identity",fill="cornflowerblue",col="black")+
  theme(axis.text.x = element_text(angle=90,vjust=-0,hjust = 1))+
  labs(title = "Top 50 communties based on the number of properties registred in website")

  
# area.name

df %>% group_by(area.name) %>% 
  summarise(No=n()) %>% 
  arrange(desc(No)) %>% 
  top_n(50) %>% 
  ggplot(aes(area.name,No))+
  geom_bar(stat = "identity",fill="cornflowerblue",col="black")+
  theme(axis.text.x = element_text(angle=90,vjust=-0))

#-------------------------------------------------------------------------------
# BIVARIATE ANALYSIS
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# relation ship between price and sq.meter
#-------------------------------------------------------------------------------

df %>% ggplot(aes(price,size.sq.meter)) +
  geom_point(alpha=0.3  )+
  geom_smooth(stat="smooth",method = "lm")+
  labs(title = "Relation ship between Price and Sq. meter of apartment")
  
# find cooreleation

cor(df$price,df$size.sq.meter,method = "pearson")
cor(df$price,df$size.sq.meter,method = "spearman")

#-------------------------------------------------------------------------------
# relationship between price and no: of bedrooms
#-------------------------------------------------------------------------------

df %>% ggplot(aes(no.beds,price))+
  stat_summary(geom = "bar",fun = "mean",fill="red",alpha=0.5)+
  stat_summary(geom = "bar",fun = "median",width=0.3,alpha=0.5,fill="royalblue")+
  labs(title = "Median and Mean of price based on bedrooms",subtitle = "Blue bar is median and red bar is mean of price")

df %>% ggplot(aes(no.beds,price))+
  geom_boxplot()

#-------------------------------------------------------------------------------
# relationship between sq.meter and no: of bedrooms
#-------------------------------------------------------------------------------

df %>% ggplot(aes(no.beds,size.sq.meter))+
  stat_summary(geom = "bar",fun = "mean",fill="red",alpha=0.5)+
  stat_summary(geom = "bar",fun = "median",width=0.3,alpha=0.5,fill="royalblue")+
  labs(title = "Mean and median of sq.feet of apartment based on bedrooms",
       subtitle = "Red bar represents mean and Blue bar reperesents median of sq.feet of apartment")

df %>% ggplot(aes(no.beds,size.sq.meter))+
  geom_boxplot()

df %>% ggplot(aes(no.beds,size.sq.meter))+
  geom_jitter(alpha=0.3)

#-------------------------------------------------------------------------------
df$no.beds=gsub(" ","",df$no.beds) # remove blank space of data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Analyse the frequency of no.of beds based on community
#-------------------------------------------------------------------------------

tmp=sort(table(df$area.name),decreasing = T)[1:50] # select top 50 frequency
tmp=names(tmp)


df %>% filter(area.name%in%tmp) %>% 
  ggplot(aes(area.name,fill=no.beds))+
  geom_bar()+
  labs(title="Analyse the number of bedrooms based on community")
  theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1))


#-------------------------------------------------------------------------------
# Analyze sq.feet based on community  
#-------------------------------------------------------------------------------

  df %>% filter(area.name%in%tmp) %>% 
    ggplot(aes(area.name,size.sq.meter))+
    geom_jitter(alpha=0.2,position = position_jitter(0.1))+
    stat_summary(geom = "crossbar",fun="mean",col="red",size=0.2)+
    stat_summary(geom = "crossbar",fun="median",col="blue",size=0.2)+
    theme(axis.text.x = element_text(angle = 90,vjust=0.5,hjust=1))+
    labs(title="Square feet of apartment based on top 50 communities",
         subtitle = "Red cross bar indiactes mean and blue cross bar indicates the median of sq. feet of apartment")

  
      
#-------------------------------------------------------------------------------
# Analyse apartment mean and median price based on top 50 community that registered in 
# property finder website
#-------------------------------------------------------------------------------



tmp=sort(table(df$area.name),decreasing = T)[1:50] # select top 50 frequency
tmp=names(tmp)

# all together using facet wrap

df %>% filter(area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  facet_wrap(~no.beds)
  

# 1 bedroom
df %>% filter(no.beds=="1" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="One bedroom apartments mean and median of the price based on the top 50 communities frequencies",
       subtitle = "Red color indicates the mean of price and blue color indicates the median price",
       x="Community Name",y="Price")

# 2 bedrooms

df %>% filter(no.beds=="2" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Two bedroom apartments mean and median of the price based on the top 50 communities frequencies",
       subtitle = "Red color indicates the mean of price and blue color indicates the median price",
       x="Community Name",y="Price")
# 3 bedrooms

df %>% filter(no.beds=="3" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Three bedroom apartments mean and median of the price based on the top communities frequencies",
       subtitle = "Red color indicates the mean of price and blue color indicates the median price",
       x="Community Name",y="Price")

# 4 bedrooms

df %>% filter(no.beds=="4" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Four bedroom apartments mean and median of the price",
       subtitle = "Red color indicates the mean of price and blue color indicates the median price",
       x="Community Name",y="Price")

# 5 bedrooms

df %>% filter(no.beds=="5" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Five bedroom apartments mean and median of the price",
       subtitle = "Red color indicates the mean of price and blue color indicates the median price",
       x="Community Name",y="Price")

# Studio apartment

df %>% filter(no.beds=="studio" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,price)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Studio apartments mean and median of the price",
       subtitle = "Red color indicates the mean of price and blue color indicates the median price",
       x="Community Name",y="Price")

#-------------------------------------------------------------------------------
# Analyse apartment mean and median of size based on top 50 community that registered in 
# property finder website
#-------------------------------------------------------------------------------

tmp=sort(table(df$area.name),decreasing = T)[1:50] # select top 50 frequency
tmp=names(tmp)

# all together using facet wrap

df %>% filter(area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  facet_wrap(~no.beds)

# 1 bedroom

df %>% filter(no.beds=="1" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="One bedroom apartments mean and median of the size based on the top 50 communities frequencies",
       subtitle = "Red color indicates the mean of size and blue color indicates the median size",
       x="Community Name",y="Size in sq feet")

# 2 bedrooms

df %>% filter(no.beds=="2" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Two bedroom apartments mean and median of the size based on the top 50 communities frequencies",
       subtitle = "Red color indicates the mean of size and blue color indicates the median size",
       x="Community Name",y="Size in sq feet")

# 3 bedrooms

df %>% filter(no.beds=="3" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Three bedroom apartments mean and median of the size based on the top communities frequencies",
       subtitle = "Red color indicates the mean of size and blue color indicates the median size",
       x="Community Name",y="Size in sq feet")

# 4 bedrooms

df %>% filter(no.beds=="4" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Four bedroom apartments mean and median of the size based on the top communities frequencies",
       subtitle = "Red color indicates the mean of size and blue color indicates the median size",
       x="Community Name",y="Size in sq feet")

# 5 bedrooms

df %>% filter(no.beds=="5" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Five bedroom apartments mean and median of the size based on the top communities frequencies",
       subtitle = "Red color indicates the mean of size and blue color indicates the median size",
       x="Community Name",y="Size in sq feet")

# Studio

df %>% filter(no.beds=="studio" & area.name%in%tmp ) %>% 
  ggplot(aes(area.name,size.sq.meter)) + 
  stat_summary(geom = "point",fun = mean,col="red")+
  stat_summary(geom = "point",fun = median,col="royalblue")+
  theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))+
  labs(title="Studio apartments mean and median of the size based on the top communities frequencies",
       subtitle = "Red color indicates the mean of size and blue color indicates the median size",
       x="Community Name",y="Size in sq feet")

#-------------------------------------------------------------------------------
# Analyze the correlation of price and size based on number of bedrooms
#-------------------------------------------------------------------------------

df %>% group_by(no.beds) %>% 
  summarise(correlation=cor(price,size.sq.meter)) %>% 
  ggplot(aes(no.beds,correlation))+
  geom_bar(stat = "identity")+
  labs(title = "Correlation of price and sq. feet based on no. of bedrooms")+
  geom_text(aes(label=round(correlation,2)),vjust=-0.5)



df %>% filter(area.name %in%tmp) %>% 
    group_by(area.name,no.beds) %>% 
    summarise(correlation=cor(price,size.sq.meter)) %>% 
    ggplot(aes(area.name,no.beds,fill=correlation))+
    geom_tile()+
    scale_fill_distiller(palette = "RdPu")+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))
  

  ggplot(aes(area.name,correlation)) +
    geom_bar(stat = "identity")+
    facet_wrap(~no.beds)+
    theme(axis.text.x = element_text(angle=90,vjust = 1,hjust = 1))
  

