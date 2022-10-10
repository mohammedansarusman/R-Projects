library(rvest)

link=c("https://www.cars24.com/ae/buy-used-cars-dubai/",
       paste0("https://www.cars24.com/ae/buy-used-cars-dubai/?page=",1:75)
      )
       

t_model=c()
t_specs=c()
t_mileage=c()
t_cyl=c()
t_price=c()
t_emi=c()
t_dp=c()

for (i in link) {
  page=read_html(i)
  
  model=page %>% html_nodes(".RZ4T7") %>% html_text()
  specs=page %>% html_nodes("._1i1E6") %>% html_text()
  mileage=page %>% html_nodes("._1TV5f span") %>% html_text()
  cyl=page %>% html_nodes("._1TV5f+ ._1TV5f") %>% html_text()
  price=page %>% html_nodes(".aApXW span") %>% html_text()
  emi=page %>% html_nodes("strong") %>% html_text()
  dp=page %>% html_nodes("._2xBAf") %>% html_text()
  
    
  
  
  
  
  t_model=c(t_model,model)
  t_specs=c(t_specs,specs)
  t_mileage=c(t_mileage,mileage)
  t_cyl=c(t_cyl,cyl)
  t_price=c(t_price,price)
  t_emi=c(t_emi,emi)
  t_dp=c(t_dp,dp)
  
  
  
  
  print(i)
  Sys.sleep(10)
    
}

df=data.frame(t_model,t_specs,t_mileage,t_cyl,t_price,t_emi,t_dp)

#getwd()
#write.csv(df,"cars24data.csv")

