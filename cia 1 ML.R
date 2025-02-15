library(dplyr)
library(caret)
#Performing the EDA in the data set
#Understanding the data set 
summary(Refrigerator)
#Used to find the summary of the entire data set
summarise(Refrigerator)
numericvalues = select_if(Refrigerator, is.numeric)
numericvalues
#We have found out those columns which has numeric values in the data set
vari = select(Refrigerator, ECOST, RSIZE,FEATURES)
vari
#We have selected those variables that influences the pricing of the refrigerator according to our research statement
brand = rename(Refrigerator, BRANDNAME=BRANDNAM)
brand   
#We have renamed the column 'BRANDNAM' to 'BRANDNAME' 
reord = select(Refrigerator, PRICE, ECOST, RSIZE, FEATURES, everything())
reord
#We have reorder the columns so that we can easily understand the data and identify the variables that we are using in this data set

#Cleaning the data
#Now as we have understood the data and made it according to our understanding we can clean the data set by clearing the unwanted variables.
mydata = select(Refrigerator, -S_SQ_FT)
mydata
#We have dropped the variable S_SQ_FT which we are not considering in finding the solution to the business prob.
mydata1 = select(Refrigerator, -BRANDNAM)
mydata1
#We have dropped the variable BRANDNAM  which we are not considering in finding the solution to the business prob.
na=sum(is.na(Refrigerator))
na
#We have identified that there is the 0 null values in the data set 
boxplot(Refrigerator$PRICE, main="Price box plot",ylab="price")
boxplot(Refrigerator$ECOST, main="ECOST box plot",ylab="ECOST")
boxplot(Refrigerator$RSIZE, main="RSIZE box plot",ylab="RSIZE")
boxplot(Refrigerator$FSIZE, main="FSIZE box plot",ylab="FSIZE")
boxplot(Refrigerator$SHELVES, main="SHELVES box plot",ylab="SHELVES")
boxplot(Refrigerator$S_SQ_FT, main="S_SQ_FT box plot",ylab="S_SQ_FT")
boxplot(Refrigerator$FEATURES, main="features box plot",ylab="features")

#Relationship  analysis
#After cleaning the data set by  removing the unwanted variables and values we can now find the relationshio analysis of the data set.
#To find the relationship of the variables we can use the correlation analysis 
install.packages(corrplot)
library(corrplot)
corr= cor(Refrigerator[,(1:7)])
corr
corrplot(corr, method= "pie")
corrplot(corr, method= "color")
corrplot(corr, method= "number")
head(round(corr,1))

corrplot(corr, type = "upper")

corrplot(corr, type= "lower")

corrplot(corr, type = "upper", col= c("black", "red"), bg= "blue")

cor(Refrigerator$PRICE,Refrigerator$ECOST)
#There is positive relation between the Price and ECOST of refrigerator.
cor(Refrigerator$PRICE,Refrigerator$RSIZE)
#There exists a negative relation between the Price and RSIZE of refrigerator.
cor(Refrigerator$PRICE,Refrigerator$FEATURES)
 #There is positive relation between the Price and Features of refrigerator.

#Visualization
library(ggplot2)
plot(Refrigerator$PRICE,Refrigerator$ECOST)
abline(lm(Refrigerator$ECOST~Refrigerator$PRICE),col="blue")
#By plotting scatter plot we are able to understand the relationship between Price and ECOST of refrigerator.
plot(Refrigerator$PRICE,Refrigerator$RSIZE)
abline(lm(Refrigerator$RSIZE~Refrigerator$PRICE),col="blue")
#By plotting scatter plot we are able to understand the relationship between Price and RSIZE of refrigerator.
plot(Refrigerator$PRICE,Refrigerator$FEATURES)
abline(lm(Refrigerator$FEATURES~Refrigerator$PRICE),col="blue")
#By plotting scatter plot we are able to understand the relationship between Price and Features of refrigerator.
boxplot(Refrigerator$PRICE,  main = "PRICE  Box Plot", ylab = "PRICE")
#By visualizing the box plot we are able to identify the outliers present in the price variable

#Pareto Chart of Price
library(qcc)
freq_distribution=table(Refrigerator$PRICE)
freq_distribution
cnt_distinct = length(freq_distribution)
cnt_distinct
pareto.chart(freq_distribution, main = "Price Pareto Chart", xlab = "Price", ylab = "Frequency", col = topo.colors(cnt_distinct))
abline(h = sum(freq_distribution) * 0.8, col = "red", lwd = 2)

#Histogram of ECOST
freq_distribution=table(Refrigerator$ECOST)
freq_distribution
cnt_distinct = length(freq_distribution)
cnt_distinct
hist(Refrigerator$ECOST, main = "ECOST HISTOGRAM", xlab = "ECOST", ylab = "Frequency", col = "orange")

#Bar Plot of RSIZE
freq_distribution=table(Refrigerator$RSIZE)
freq_distribution
cnt_distinct = length(freq_distribution)
cnt_distinct
pie_cnt = table(Refrigerator$RSIZE)
b = barplot(pie_cnt, main = "RSIZE Bar Plot", beside = T)
text(x = b, as.data.frame(pie_cnt)[,2]+3, format(as.data.frame(pie_cnt)[,2]), xpd = T, pos = 3, cex = 0.5)
grid()
box()

#Pie Chart of FEATURES
freq_distribution=table(Refrigerator$FEATURES)
freq_distribution
cnt_distinct = length(freq_distribution)
cnt_distinct
pie_cnt = table(Refrigerator$FEATURES)
pie_pct = pie_cnt * 100 / sum(pie_cnt)
pie_lbl = paste(pie_pct, "%", sep = ";")
pie_lbl = paste(row.names(pie_pct), pie_lbl, sep = "\n")
pie(pie_cnt, main = "FEATURES Pie Distribution", labels = pie_lbl)
box()

#Regression Analysis
simplereg = lm(PRICE~ECOST, data=Refrigerator)
summary(simplereg)
plot(simplereg)
#After finding simple regression of PRICE and ECOST we were able to find out that the p value is less than 0.05 which is acceptable and significant.There exits a linear relationship between the identified variables.
simplereg = lm(PRICE~RSIZE, data=Refrigerator)
summary(simplereg)
plot(simplereg)
#After finding simple regression of PRICE and RSIZE we were able to find out that the p value is greater than 0.05 which is not acceptable and does not have significance.There does not exits a linear relationship between the identified variables.
simplereg = lm(PRICE~FEATURES, data=Refrigerator)
summary(simplereg)
plot(simplereg)
#After finding simple regression of PRICE and Features we were able to find out that the p value is greater than 0.05 which is not acceptable and does not have significance.There does not exits a linear relationship between the identified variables.
multireg = lm(PRICE~ECOST+RSIZE+FEATURES, data=Refrigerator)
summary(multireg)
plot(multireg)

#After finding multiple regression we were able to find out that the p value is greater than 0.05 which is not acceptable and does not have significance.There does not exits a linear relationship between the identified variables.
