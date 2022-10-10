# Libraries

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Open file

df=read.csv("~/Desktop/Kaggle/corona.csv",sep = ";")
str(df)

# Objectives of this project

# Perform Exploratory Data Analysis on Saudi Arabia Covid-19 Data collected since 2020-03-02 until 2022-08-26

# Convert data type of date
df$Date=as.Date(df$Date)


df$Cases..person.=as.numeric(df$Cases..person.)


df=df %>% filter(Daily...Cumulative=="Daily" & Indicator!="Tested" & City!="Total") %>% 
  select(-Daily...Cumulative,-Event)


df=df %>% pivot_wider(names_from = Indicator,values_from = Cases..person.)

glimpse(df)

df$Recoveries=ifelse(is.na(df$Recoveries)==T,0,df$Recoveries)
df$Mortalities=ifelse(is.na(df$Mortalities)==T,0,df$Mortalities)

glimpse(df)

# Find missing values
anyNA(df)
lapply(df, anyNA)

# Impute 0 to missing fields
df$Cases=ifelse(is.na(df$Cases)==T,0,df$Cases)

df$month_df=month(df$Date,label = T)
df$year_df=year(df$Date)
df$month_year=paste(df$month_df,df$year_df,sep = "_")

# write.csv(df,"saudi_covid.csv")
# Active cases  = Total cases - Recovered cases - Mortality
# df$Active=(df$Cases-df$Recoveries)-df$Mortalities

# Univariate Analysis

# ##############################################################
# Total no. of cases during 2020-03-02 until 2022-08-26
# ##############################################################

sum(df$Cases)

# ##############################################################
# Total no. of recoveries during 2020-03-02 until 2022-08-26
# ##############################################################

sum(df$Recoveries)

# ##############################################################
# Total no. of mortalities during 2020-03-02 until 2022-08-26
# ##############################################################

sum(df$Mortalities)

# ##############################################################
# Mortatlity rate
# ##############################################################

(sum(df$Mortalities)/sum(df$Cases))*100


# ##############################################################
# Average number of CASES per day
# ##############################################################

sum(df$Cases)/as.numeric(difftime("2022-08-26","2020-03-02",units = "days"))

# ##############################################################
# Average number of recoveries per day
# ##############################################################

sum(df$Recoveries)/as.numeric(difftime("2022-08-26","2020-03-02",units = "days"))

# ##############################################################
# Average deaths per day
# ##############################################################

sum(df$Mortalities)/as.numeric(difftime("2022-08-26","2020-03-02",units = "days"))

# ##############################################################
# No. of cases in cities during 2020-03-02 until 2022-08-26 / Select top 50 cities
# ##############################################################

df %>% group_by(City) %>% 
  summarise(No=sum(Cases)) %>% 
  slice_max(No,n=50) %>% 
  ggplot(aes(City,No))+
  geom_bar(stat = "identity",col="black",fill="blue")+
  scale_y_continuous(limits = c(0,200000))+
  theme_classic()+
  geom_text(aes(label=No),angle=90,hjust=-0.1)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(title = "Top 50 cities affected Corona based on number of cases",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       y="No. of cases")
  
# ##############################################################
# No. of recoveries in cities during 2020-03-02 until 2022-08-26 
# ##############################################################

df %>% group_by(City) %>% 
  summarise(No=sum(Recoveries)) %>% 
  slice_max(No,n=50) %>% 
  ggplot(aes(City,No))+
  geom_bar(stat = "identity",col="black",fill="blue")+
  scale_y_continuous(limits = c(0,200000))+
  theme_classic()+
  geom_text(aes(label=No),angle=90,hjust=-0.1)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(title = "TOP 50 CITIES RECOVERED FROM CORONA",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       y="No. of recovered persons")

# ##############################################################
# Total no. of deaths in cities during 2020-03-02 until 2022-08-26
# ##############################################################

df %>% group_by(City) %>% 
  summarise(No=sum(Mortalities)) %>% 
  slice_max(No,n=50) %>% 
  ggplot(aes(City,No))+
  geom_bar(stat = "identity",col="black",fill="blue")+
  scale_y_continuous(limits = c(0,2000))+
  theme_classic()+
  geom_text(aes(label=No),angle=90,hjust=-0.1)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(title = "TOP 50 CITIES BASED ON NUMBER OF DEATHS",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       y="No. of deaths")

# ##############################################################
# Total no. of cases in Region during 2020-03-02 until 2022-08-26
# ##############################################################

df %>% group_by(Region) %>% 
  summarise(No=sum(Cases)) %>% 
  ggplot(aes(Region,No))+
  geom_bar(stat = "identity",col="black",fill="pink")+
  scale_y_continuous(limits = c(0,250000))+
  theme_classic()+
  geom_text(aes(label=No),angle=90,hjust=-0.1)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(title = "REGIONS BASED ON NUMBER OF CASES",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       y="No. of cases")

# ##############################################################
# Total no. of recoveries in Region during 2020-03-02 until 2022-08-26 
# ##############################################################

df %>% group_by(Region) %>% 
  summarise(No=sum(Recoveries)) %>% 
  ggplot(aes(Region,No))+
  geom_bar(stat = "identity",col="black",fill="pink")+
  scale_y_continuous(limits = c(0,250000))+
  theme_classic()+
  geom_text(aes(label=No),angle=90,hjust=-0.1)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(title = "REGIONS BASED ON NUMBER OF RECOVERIES",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       y="No. of recoveries")

# ##############################################################
# Total no. of deaths in Region during 2020-03-02 until 2022-08-26
# ##############################################################

df %>% group_by(Region) %>% 
  summarise(No=sum(Mortalities)) %>% 
  ggplot(aes(Region,No))+
  geom_bar(stat = "identity",col="black",fill="pink")+
  scale_y_continuous(limits = c(0,3500))+
  theme_classic()+
  geom_text(aes(label=No),angle=90,hjust=-0.1)+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
  labs(title = "REGIONS BASED ON NUMBER OF DEATHS",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       y="No. of deaths")

# ##############################################################
# Find the number of cases based on month and year
# ##############################################################

df %>% group_by(year(Date),month(Date)) %>% 
  summarise(n_cases=sum(Cases)) %>% 
  arrange(`year(Date)`,`month(Date)`) %>% 
  ggplot(aes(as.character(`month(Date)`),n_cases,fill=as.character(`year(Date)`)))+
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
  labs(title = "NO. OF CASES BASED ON MONTH AND YEAR",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       x="Month",
       y="No. of cases",
       fill="Year")

# ##############################################################
# Find the number of Recoveries based on month and year
# ##############################################################

df %>% group_by(year(Date),month(Date)) %>% 
  summarise(n_cases=sum(Recoveries)) %>% 
  arrange(`year(Date)`,`month(Date)`) %>% 
  ggplot(aes(as.character(`month(Date)`),n_cases,fill=as.character(`year(Date)`)))+
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
  labs(title = "NO. OF RECOVERIES BASED ON MONTH AND YEAR",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       x="Month",
       y="No. of recoveries",
       fill="Year")

# ##############################################################
# Find the number of deaths based on month and year
# ##############################################################

df %>% group_by(year(Date),month(Date)) %>% 
  summarise(n_cases=sum(Mortalities)) %>% 
  arrange(`year(Date)`,`month(Date)`) %>% 
  ggplot(aes(as.character(`month(Date)`),n_cases,fill=as.character(`year(Date)`)))+
  geom_bar(stat = "identity",position = position_dodge(preserve = "single"))+
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
  labs(title = "NO. OF DEATHS BASED ON MONTH AND YEAR",
       subtitle = "Since 2020-03-02 until 2022-08-26",
       x="Month",
       y="No. of deaths",
       fill="Year")

# ##############################################################
# No. of cases, recovered and deaths analyze based month and year
# ##############################################################

df %>% pivot_longer(cols = 4:6,
                    names_to = "Indicator",
                    values_to = "No") %>% 
  ggplot(aes(month_df,No,fill=Indicator))+
  geom_bar(stat = "identity",position = position_dodge())+
  facet_wrap(~year_df)


x=table(df$month_year)
x=names(x)
sort(x)
x=c("Mar_2020","Apr_2020","May_2020","Jun_2020","Jul_2020","Aug_2020","Sep_2020","Oct_2020","Nov_2020","Dec_2020",
    "Jan_2021","Feb_2021","Mar_2021","Apr_2021","May_2021","Jun_2021","Jul_2021","Aug_2021","Sep_2021","Oct_2021",
    "Nov_2021","Dec_2021","Jan_2022","Feb_2022","Mar_2022", "Apr_2022","May_2022","Jun_2022","Aug_2022")

# No. of cases analyzed based on month, year and regions

df %>% group_by(month_year,Region) %>% summarise(no=sum(Cases)) %>% 
  ggplot(aes(month_year,Region,fill=-no,label=no))+
  geom_tile(stat="identity",col="white")+
  scale_x_discrete(limits=x)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(size=3)+
  labs(fill="Cases")
  
# No. of recoveries analyzed based on month, year and regions

df %>% group_by(month_year,Region) %>% summarise(no=sum(Recoveries)) %>% 
  ggplot(aes(month_year,Region,fill=-no,label=no))+
  geom_tile(stat="identity",col="white")+
  scale_x_discrete(limits=x)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(size=3)+
  labs(fill="Recoveries")

# No. of deaths analyzed based on month, year and regions

df %>% group_by(month_year,Region) %>% summarise(no=sum(Mortalities)) %>% 
  ggplot(aes(month_year,Region,fill=-no,label=no))+
  geom_tile(stat="identity",col="white")+
  scale_x_discrete(limits=x)+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(size=3)+
  labs(fill="Deaths")

# ##############################################################
# Mortality rate based on Region
# ##############################################################

df %>% group_by(Region) %>% 
  summarise(n_cases=sum(Cases),n_deaths=sum(Mortalities),rate_mortality=(n_deaths/n_cases)*100) %>% 
  ggplot(aes(x=n_cases,y=rate_mortality))+
  geom_point()+
  geom_text(aes(label=Region),vjust=0,hjust=0)

# ##############################################################
# Mortality rate based on Cities
# ##############################################################

df %>% group_by(City) %>% 
  summarise(n_cases=sum(Cases),n_deaths=sum(Mortalities),rate_mortality=(n_deaths/n_cases)*100) %>% 
  ggplot(aes(x=n_cases,y=rate_mortality))+
  geom_point(alpha=0.4)+
  geom_text(aes(label=City),vjust=0,hjust=0,size=2)

# ##############################################################
# Mortality rate based on Month & Year
# ##############################################################

df %>% group_by(month_year) %>% 
  summarise(n_cases=sum(Cases),n_deaths=sum(Mortalities),rate_mortality=(n_deaths/n_cases)*100) %>% 
  ggplot(aes(month_year,rate_mortality))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90))+
  scale_x_discrete(limits=x)
  
  ggplot(aes(x=n_cases,y=rate_mortality))+
  geom_point(alpha=0.4)+
  geom_text(aes(label=City),vjust=0,hjust=0,size=2)

# ##############################################################
# Mortality rate based on Region, Month and year
# ##############################################################
  
df %>% group_by(Region,month_year) %>%  
    summarise(no_cases=sum(Cases),no_deaths=sum(Mortalities),rate_mortality=(no_deaths/no_cases)*100) %>% 
    ggplot(aes(Region,month_year,label=round(rate_mortality,2),fill=-rate_mortality))+
    geom_tile()+
    geom_text(size=2)+
    scale_y_discrete(limits=x)
    
  
  # Percentage of increase/decrease of cases since Mar-2020 until Feb-2021
  # Percentage of increase/decrease of cases since Mar-2021 until Feb-2022
  
  