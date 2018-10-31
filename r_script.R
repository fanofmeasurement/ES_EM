
#####左侧窗口#####

(1*2+3^4)/5

print("你好，世界")

for (i in 1:10) {
  print(i^2)
}

a<-c(7.20,5.78,5.31,5.81,5.64,6.45,5.64)
sum(a)
mean(a)
median(a)
sd(a)

#####右上角窗口#####
x<-1:10
y<-log(x)
A<-data.frame(x,y)
B<-list(1,2,list(1,2,list(1,2,3)))
C<-matrix(1:10,2)
f<-function(x){x+1}
rm(list = ls())#删除右上角所有对象

#####右下角窗口#####
set.seed(1)
e<-rnorm(30)
x<-c(1:30)
y<-2+0.75*x+e
plot(x,y,type = 'p')
abline(lm(y~x),col='red')
?set.seed
?rnorm
?abline
?lm
example(abline)
example(lm)

#####读入数据#####
data1<-read.csv('data.csv')#读入csv文件
library(openxlsx)
data2<-read.xlsx('data.xlsx')#读入xlsx文件
library(Hmisc)
data3<-spss.get('data.sav')#读入spss数据文件
library(foreign)
data4<-read.spss('data.sav')#读入spss数据文件
rm(list=c('data1','data2','data3','data4'))

data1<-read.csv('E:/OneDrive/Documents/教育统计/base-operating-of-R/data.csv')

setwd('C:/')
data1<-read.csv('data.csv')
setwd("E:/OneDrive/Documents/教育统计/base-operating-of-R")
data1<-read.csv('data.csv')

#####数据编辑#####

manager <- c(1, 2, 3, 4, 5) 
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09") 
country <- c("US", "US", "UK", "UK", "UK") 
gender <- c("M", "F", "F", "M", "F") 
age <- c(32, 45, 25, 39, 99) 
q1 <- c(5, 3, 3, 3, 2) 
q2 <- c(4, 5, 5, 3, 2) 
q3 <- c(5, 2, 5, 4, 1) 
q4 <- c(5, 5, 5, NA, 2) 
q5 <- c(5, 5, 2, NA, 1) 
leadership <- data.frame(manager, date, country, gender, age, 
                         q1, q2, q3, q4, q5, stringsAsFactors=FALSE) 

#####在数据框中加入新变量####

mydata<-data.frame(x1 = c(109, 129, 120, 241), 
                   x2 = c(130, 96, 124, 230)) 

#method1
mydata$sumx  <-  mydata$x1 + mydata$x2 
mydata$meanx <- (mydata$x1 + mydata$x2)/2 

#method2 
attach(mydata) 
mydata$sumx  <-  x1 + x2 
mydata$meanx <- (x1 + x2)/2 
detach(mydata) 

#method3
mydata <- transform(mydata, 
                    sumx  =  x1 + x2, 
                    meanx = (x1 + x2)/2) 


#####编码#####
leadership$age[leadership$age  == 99]    <- NA 
leadership$agecat[leadership$age  > 75]  <- "Elder" 
leadership$agecat[leadership$age >= 55 &  
                    leadership$age <= 75]  <- "Middle Aged" 
leadership$agecat[leadership$age  < 55]  <- "Young" 


#####重命名#####
names(leadership)
names(leadership)[2] <- "testDate"
names(leadership)[6:10] <- c("item1", "item2", "item3", "item4", "item5") 
leadership


####缺失值####
y <- c(1, 2, 3, NA) 
is.na(y) 
is.na(leadership[,6:10]) 
x <- c(90, 88, NA, 93)
y <- x[1] + x[2] + x[3] + x[4]
y
z <- mean(x)
z
z<-mean(x,na.rm=TRUE)
z
na.omit(x) %>% mean


####类型转换####
mode(leadership$manager)
## [1] "numeric"
typeof(leadership$country)
## [1] "character"
str(leadership$agecat)
##  chr [1:5] "Young" "Young" "Young" "Young" NA
is.character(leadership$manager)
## [1] FALSE
as.character(leadership$manager)
## [1] "1" "2" "3" "4" "5"


####数据排序####
#按年龄升序排序
newdata <- leadership[order(leadership$age),]

#先女性后男性，年龄升序排序
attach(leadership)
newdata <- leadership[order(gender, age),]
detach(leadership)

#先女性后男性，年龄降序排序
attach(leadership)
newdata <-leadership[order(gender, -age),]
detach(leadership)


#####数据集合并#####
dataframeA<-data.frame(ID=c(1,3,4,2,3,4),
                       Country=c('US','JP','GM','CN','JP','GM'),
                       age=c(14,55,33,22,44,34))
dataframeB<-data.frame(ID=c(2,2,3,3,4,1),
                       Country=c('CN','CN','JP','JP','GM','US'),
                       salary=c(3000,8000,10000,20000,9000,50000))
#用merge函数合并
merge(dataframeA, dataframeB, by=c("ID","Country"))
#合并各列
cbind(dataframeA, dataframeB)
#合并各行(只能合并列名相等的行)
rbind(dataframeA[1:3,1:2], dataframeB[1:2,1:2])


#####选取子集####
#选入（保留）变量
#method1
newdata <- leadership[, c(6:10)]
#method2
myvars <- c("item1", "item2", "item3", "item4", "item5")
newdata <-leadership[myvars]
#method3
myvars <- paste("item", 1:5, sep="")
newdata <- leadership[myvars]

#剔除（丢弃）变量
#method1
myvars <- names(leadership) %in% c("item3", "item4")
newdata <- leadership[!myvars]
#method2
newdata <- leadership[c(-8,-9)]
#method3
leadership$item3 <- leadership$item4 <- NULL

#将大于三十岁的男性数据选入newdata
#method1
newdata <- leadership[1:3,]
newdata <- leadership[leadership$gender=="M" &
                        leadership$age > 30,]
#method2
attach(leadership)
## The following objects are masked _by_ .GlobalEnv:
## 
##     age, country, gender, manager
newdata <- leadership[gender=='M' & age > 30,]
detach(leadership)

#挑选某一时间区间内的数据
leadership$testDate <- as.Date(leadership$testDate, "%m/%d/%y")
startdate <- as.Date("2009-01-01")
enddate <- as.Date("2009-10-31")
newdata <- leadership[which(leadership$testDate >= startdate &
                              leadership$testDate <= enddate),]


####数据导出#####
write.xlsx(leadership,'leadership.xlsx')
