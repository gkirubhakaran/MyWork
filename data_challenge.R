sales_data <- read.csv("C:/Users/kirubhakaran/Desktop/Assignment Part I.csv")
head(sales_data)
summary(sales_data)
str(sales_data)

sales_data$StartDate <- as.POSIXct(strptime(sales_data$StartDate,"%m/%d/%y")) 
sales_data$EndDate <- as.POSIXct(strptime(sales_data$EndDate,"%m/%d/%y"))


category_sales <- as.data.frame(aggregate((Sales..../1000)~DVSN_name,data = sales_data, FUN = sum))

colnames(category_sales)[2] <- "sales_in_thousands"
category_sales$Percentage_of_overall_sales <- (category_sales$sales_in_thousands/sum(category_sales$sales_in_thousands))*100
category_sales$Percentage_of_overall_sales <- round(category_sales$Percentage_of_overall_sales, digits = 2)

spt <- split(sales_data, sales_data$DVSN_name) 
names(spt)
length(spt)
names(spt)[1] <- "N/A"

a <- data.frame()
for(i in 1:65) {
  
  mu <- mean(spt[[i]]$Sales....)
  sigma <- sd(spt[[i]]$Sales....)
  x <- cbind(mu,sigma)
  a <- rbind(a,x)
}

a$division <- names(spt)
str(a)

spt <- split(sales_2008, sales_2008$DVSN_name) 
names(spt)
length(spt)
names(spt)[1] <- "N/A"

spt[54]
spt[[1]]$deviation <- round((spt[[1]]$Sales...- mean(spt[[1]]$Sales....))/mean(spt[[1]]$Sales....),digits =2)

spt[[63]][spt[[63]]$deviation>1,]

a <- data.frame()

for(i in 1:65) {
 
  spt[[i]]$deviation <- round((spt[[i]]$Sales...- mean(spt[[i]]$Sales....))/mean(spt[[i]]$Sales....),2)
  temp <- spt[[i]][spt[[i]]$deviation>1,]
  a <- rbind(a,temp)
  
}

str(a)
b <- a[week(a$StartDate)>44 & a$Sales....>1000,]
b <- b[complete.cases(b)==T,]
c <- aggregate(deviation~DVSN_name, data = b, FUN = mean)
d <- aggregate(Sales....~DVSN_name, data = b, FUN = mean)

names(spt)

stats <- data.frame()
names(spt)
mean(spt[[2]]$Sales....)
mean(spt[[3]]$Sales....)
mean(spt[[29]]$Sales....)
mean(spt[[36]]$Sales....)
mean(spt[[63]]$Sales....)

layout(matrix(c(1:64), 8, 8, byrow = TRUE))
dev.off()
for(i in 2:65) {
myts <- ts((spt[[i]]$Sales....-min(spt[[i]]$Sales....))/(max(spt[[i]]$Sales....)-min(spt[[i]]$Sales....)), start=c(2007, 1), end=c(2008, 12), frequency=52) 
assign(names(spt[i]),myts)
}

names(spt)
ts.plot(ldeaths, mdeaths, fdeaths,
        gpars=list(xlab="year", ylab="deaths", lty=c(1:3)))

top <- sales_2008[sales_2008$DVSN_name %in% c("Prescription.Drugs",
                            "Home Electronics",
                            "Consumables",
                            "Edible",
                          "Health and Beauty"
                            ),]



p <- ggplot(data = top, aes(x = top$StartDate, y = top$Sales....,group = top$DVSN_name, col = top$DVSN_name))+geom_point()+geom_line()+title(main="Time series",xlab="Time", ylab="Sales")

title()


p <- p + theme(
  axis.title.x = element_text(face="bold", size=10),
  axis.title.y = element_text(face="bold", size=10),
  plot.background=element_blank(),
  panel.background=element_blank(),
  axis.line = element_line(size=1))

plot(p)

myts <- ts((spt[[2]]$Sales....-min(spt[[2]]$Sales....))/(max(spt[[2]]$Sales....)-min(spt[[2]]$Sales....)), start=c(2007, 1), end=c(2008, 12), frequency=52) 
plot(myts)


plot(myts)
myts <- ts((Prescription.Drugs$Sales....-min(Prescription.Drugs$Sales....))/(max(Prescription.Drugs$Sales....)-min(Prescription.Drugs$Sales....)), start=c(2008, 1), end=c(2008, 12), frequency=52) 
myts <- ts(sales_2008$Sales...., start=c(2008, 1), end=c(2008, 12), frequency=52)

measures(myts)

install.packages("fracdiff")
library(fracdiff)
install.packages("tseries")
library(tseries)

myts <- ts((spt[[9]]$Sales....-min(spt[[9]]$Sales....))/(max(spt[[9]]$Sales....)-min(spt[[9]]$Sales....)), start=c(2007, 1), end=c(2008, 12), frequency=52) 
temp <- data.frame(t(measures(myts)))
temp$category <- names(spt[i])
stats <- rbind(stats,temp)

str(spt)
new_spt <- list()
b <- data.frame()
for(i in 1:65){
  a <- nrow(spt[[i]]) 
  if(a==52)
  {b <- rbind(b,spt[[i]])}
}

table(b$DVSN_name)


n <- 10
s <- sample(1:50, n)
idx <- c(s, 52+s,104+s,156+s,208+s,260+s,312+s,364+s,416+s,
         468+s,520+s,572+s,624+s,676+s,728+s,780+s,832+s,884+s,
         936+s,988+s,1040+s,1092+s,1144+s,1196+s,1248+s,1300+s,
         1352+s,
         1404+s,
         1456+s,
         1508+s,
         1560+s,
         1612+s,
         1664+s,
         1716+s,
         1768+s,
         1820+s,
         1872+s,
         1924+s,
         1976+s,
         2028+s,
         2080+s,
         2132+s,
         2184+s,
         2236+s
)
sample2 <- b[idx,c("Sales....")]
observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n))
# compute DTW distances
install.packages("dtw")
library(dtw)
distMatrix <- dist(sample2, method="DTW")
# hierarchical clustering
hc <- hclust(distMatrix, method="average")
plot(hc)
cutree(hc, k = 4)

final <- c(1,1,1,1,3,1,1,1,3,2,1,1,3,4,1,1,1,2,1,2,1,1,2,1,1,1,2,1,1,1,4,1,1,1,1,1,1,1,2,1,1,1,1,3)
length(final)

d <- vector()
for(i in 1:44)
{
  c <- rep(final[i],52)
  d <- c(d,c)
}
length(d)
b$clust <- d
------------------------------------------------------------------------------------------
  
# year wise

sales_data <- sales_data[order(sales_data$StartDate),] 
sales_2007 <- sales_data[sales_data$Year==2007,]
sales_2008 <- sales_data[sales_data$Year==2008,]

sales_2007$wk <- cut(sales_2007$StartDate,breaks='week', labels = 1:52)
sales_2008$wk <- cut(sales_2008$StartDate,breaks='week', labels = 1:52)

category_sales2007 <- as.data.frame(aggregate((Sales..../1000)~DVSN_name,data = sales_2007, FUN = sum))

colnames(category_sales2007)[2] <- "sales_in_thousands"
category_sales2007$Percentage_of_overall_sales <- (category_sales2007$sales_in_thousands/sum(category_sales2007$sales_in_thousands))*100
category_sales2007$Percentage_of_overall_sales <- round(category_sales2007$Percentage_of_overall_sales, digits = 2)

category_sales2008 <- as.data.frame(aggregate((Sales..../1000)~DVSN_name,data = sales_2008, FUN = sum))

colnames(category_sales2008)[2] <- "sales_in_thousands"
category_sales2008$Percentage_of_overall_sales <- (category_sales2008$sales_in_thousands/sum(category_sales2008$sales_in_thousands))*100
category_sales2008$Percentage_of_overall_sales <- round(category_sales2008$Percentage_of_overall_sales, digits = 2)

dim(category_sales2008)
  test <- cbind(category_sales2008[category_sales2008$DVSN_name %in% category_sales2007$DVSN_name,],category_sales2007)
  
  colnames(test)[2] <- "sales_2008"
  colnames(test)[5] <- "sales_2007"
  test$diff <- (test$sales_2008-test$sales_2007)/test$sales_2007
  test$diff <- round(test$diff,digits = 2)
  test$dif <- test$sales_2008-test$sales_2007
------------------------------------------------------------------------------------------
