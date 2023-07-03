library(readr)
library(data.table)
library(stringr)
library("dplyr")
library(ggplot2)
library(corrplot)
#descriptive analysis
#gom nam theo tung cum, giong nhu trong paper, tao thanh 1 dataframe (done)
#lam sao de tach nam bang R (done)
#visualize gia nha theo tung cum nam do
#chon thu vien visualize
str(house_us_1)
install.packages("stringr")
library(stringr)
#cau lenh sau day thay doi du lieu trong cot date cua df house_us_1
house_us_1$Date <- substring(house_us_1$Date, first = 1, last =4)
#tao df tinh toan gia nha qua cac thoi ky kinh te
#trong R, co phan biet hoa thuong voi ten cot
df_house_price_index <- data.frame(house_us_1$Date, house_us_1$House_Price_Index, house_us_1$Mortgage_Rate)
head(df_house_price_index)

house_us_1$Date

#cau lenh giup chuyen loai du lieu tu string sang numeric doi voi cot Date
df_house_price_index$house_us_1.Date <- as.numeric(df_house_price_index$house_us_1.Date)
str(df_house_price_index)

house_us_1$Date <- as.numeric(house_us_1$Date)
str(house_us_1)

#cau lenh giup tao them cot chia giai doan
#su dung ham mutate trong thu vien dplyr
library("dplyr")
df_hpi <- df_house_price_index %>% mutate(period = case_when(df_house_price_index$house_us_1.Date >= 1975 & df_house_price_index$house_us_1.Date<= 1980 ~ "1975-1980",df_house_price_index$house_us_1.Date >= 1981 & df_house_price_index$house_us_1.Date<= 1990 ~ "1981-1990",df_house_price_index$house_us_1.Date >= 1991 & df_house_price_index$house_us_1.Date<= 2000 ~ "1991-2000",df_house_price_index$house_us_1.Date >= 2001 & df_house_price_index$house_us_1.Date<= 2010 ~ "2001-2010",df_house_price_index$house_us_1.Date >= 2011 & df_house_price_index$house_us_1.Date<= 2015 ~ "2011-2015",df_house_price_index$house_us_1.Date >= 2016 & df_house_price_index$house_us_1.Date<= 2021 ~ "2016-2021"))
head(df_hpi)

df_house_price_index %>%
  mutate(
    period = case_when(
      df_house_price_index$house_us_1.Date >= 1975 & df_house_price_index$house_us_1.Date<= 1980 ~ "1975-1980",
      df_house_price_index$house_us_1.Date >= 1981 & df_house_price_index$house_us_1.Date<= 1990 ~ "1981-1990",
      df_house_price_index$house_us_1.Date >= 1991 & df_house_price_index$house_us_1.Date<= 2000 ~ "1991-2000",
      df_house_price_index$house_us_1.Date >= 2001 & df_house_price_index$house_us_1.Date<= 2010 ~ "2001-2010",
      df_house_price_index$house_us_1.Date >= 2011 & df_house_price_index$house_us_1.Date<= 2015 ~ "2011-2015",
      df_house_price_index$house_us_1.Date >= 2016 & df_house_price_index$house_us_1.Date<= 2021 ~ "2016-2021"
    )
  )

#tinh gia nha trung binh trong tung giai doan
#su dung aggregate trong thu vien dplyr
df_hpi_avg<-aggregate(df_hpi$house_us_1.House_Price_Index,list(df_hpi$period),mean)
class(df_hpi_avg)
colnames(df_hpi_avg)[2] = "avg_hpi" #to change column name

head(df_hpi_avg)


# Plot the bar chart
plot(df_hpi_avg$avg_hpi,type = "o", col = "red", xlab = "period of year", ylab = "average HPI",main = "Average house price index over period")

#linear correlation
corrplot((cor(house_us_corr)), method = "number",type="upper")

#spearman 
cor.test(house_us_corr$house_us_1.House_Price_Index,house_us_corr$house_us_1.Mortgage_Rate,method="spearman")
#hai bien nguoc nhau nhung co tuong quan tot

#check if variable is normal distribution
#using histogram
hist(house_us_corr$house_us_1.House_Price_Index)
hist(house_us_corr$house_us_1.Stock_Price_Index)
hist(house_us_corr$house_us_1.Mortgage_Rate)


#scatter plot 2 variables
plot(house_us_1$House_Price_Index ~ house_us_1$Mortgage_Rate,main="Correlation of house price index and mortgage rate", xlab = "house price index", ylab = "mortgage rate", pch =19)
abline(lm(house_us_1$House_Price_Index ~ house_us_1$Mortgage_Rate), col="red")

plot(house_us_1$House_Price_Index ~ house_us_1$Real_GDP_normalized,main="Correlation of house price index and real GDP", xlab = "house price index", ylab = "mortgage rate", pch =19)
abline(lm(house_us_1$House_Price_Index ~ house_us_1$Real_GDP_normalized), col="red")

#using library car
library(car)

scatterplot(House_Price_Index ~ Mortgage_Rate, data=house_us_1,xlab="house price index", ylab="mortgage rate", main="Correlation of house price index and mortgage rate")
scatterplot(House_Price_Index ~ Stock_Price_Index, data=house_us_1,xlab="house price index", ylab="stock price index", main="Correlation of house price index and stock price index")
scatterplot(House_Price_Index ~ Consumer_Price_Index, data=house_us_1,xlab="house price index", ylab="consumer price index", main="Correlation of house price index and consumer price index")
scatterplot(House_Price_Index ~ Consumer_Price_Index, data=house_us_1,xlab="house price index", ylab="consumer price index", main="Correlation of house price index and consumer price index")
scatterplot(House_Price_Index ~ Unemployment_Rate, data=house_us_1,xlab="house price index", ylab="unemployment rate", main="Correlation of house price index and unemployment rate")
scatterplot(House_Price_Index ~ Population, data=house_us_1,xlab="house price index", ylab="Population", main="Correlation of house price index and Population")

#regression
house_us_1$House_Price_Index_normalized = scale(house_us_1$House_Price_Index)

reg_stock_price <- lm(house_us_1$House_Price_Index ~ house_us_1$Stock_Price_Index)
summary(reg_stock_price)
#0.19

reg_mortgage_rate <- lm(house_us_1$House_Price_Index ~ house_us_1$Mortgage_Rate)
summary(reg_mortgage_rate)
#-29.48

reg_real_gdp_2 <- lm(house_us_1$House_Price_Index_normalized ~ house_us_1$Real_GDP_normalized)
summary(reg_real_gdp_2)

house_us_1$House_Price_Index_normalized

house_us_1$Real_GDP_normalized
house_us_1$House_Price_Index_normalized
#run multiple regression xem sao
pairs(house_us_corr)
#sau khi ve pair thi thay hai bien population va unemployment rate khong co quan he tuyen tinh voi gia nha
mreg_house_us_1 <- lm(House_Price_Index~Stock_Price_Index+Consumer_Price_Index+Real_GDP_normalized+Mortgage_Rate,house_us_1)
summary(mreg_house_us_1)

#line chart of house price index
plot(df_house_price_index$house_us_1.House_Price_Index,type = "l", col = "red", xlab = "year", ylab = "House price index",main = "Average house price index over period")
#better plot
head(df_house_price_index)
ggplot(df_house_price_index,aes(x=house_us_1.Date,y=house_us_1.House_Price_Index))+geom_line()+labs(x="Year",y="House price index by year")
