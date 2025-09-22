2+2
'hello'
x=10
y<-10
print(y)
z='hello'
sum(2,3,4)
sqrt(25)
rep(66,times = 3)
rep ("hawyeah" , 45)
q<-c('a','b','c')
h<-c(4:5)
i<-seq(6,7)
j<-seq(5,9,0.5)
learn<- c('you','me','r')
learn[2]
learn[3] <- 'Sas'
learn[4]<-'spss'
a=c(1,2,3)
a=a+1
b=c(4,5,6)
a+b
a-b
a*b
a/b
4==5
a== c(1, 99 ,3)
a== c(2, 99 ,4)
m<-c(1,3,NA,7,9)
sum(m)
sum(m,na.rm=TRUE)
mean(m,na.rm=TRUE)
help(sum)
help(mode)
matrix(0,3,4)
sijo<-1:8
s<-matrix(sijo,c(2,4))
s
s[2,4]
s[2,]
s[,3]
names=c("anthony","benedict","colin","eloise","francessca","gregory","hyacinth")
percent=c(85,88,92,95,89,95,96)
lunch=c("biryani","chickenkebab","biryani","chickenkebab","veg pulao","biryani","veg pulao")
joy<-data.frame(names,percent)
joy
joy<- data.frame(names=c("anthony","benedict","colin","eloise","francessca","gregory","hyacinth"),percent=c(85,88,92,95,89,95,96),lunch=c("biryani","chickenkebab","biryani","chickenkebab","veg pulao","biryani","veg pulao"))
joy
joy$names
joy[[1]]
joy[["percent"]]
yummy<-as.factor(lunch)

dim(grades)
dim(cs2m)                                                                                                                                                                                                                                                                                            

length(cs2m$BP)
length(grades$ethnicity)

min(cs2m$BP)
max(cs2m$BP)
range(cs2m$BP)
sum(cs2m$BP)
median(cs2m$BP)
var(cs2m$BP)
sd(cs2m$BP)
str(cs2m)
str(grades)

head(cs2m)
tail(cs2m)
head(grades)
tail(grades)
which(cs2m$BP==100)
which(cs2m$BP==170)
table(cs2m$AnxtyLH)

table(grades$ethnicity)
summary(cs2m)
summary(grades)

install.packages("psych")
library(psych)

hist(cs2m$BP)
hist(cs2m$BP,col = "blue")
hist(grades$gpa,col="black")
boxplot(cs2m$BP)
boxplot(cs2m$BP,col="red")
plot(cs2m$BP)
plot(cs2m$BP,col="purple")
stem(cs2m$BP)
stem(grades$gpa)
boxplot(cs2m$BP,horizontal="TRUE",col="red")
boxplot(cs2m)
boxplot(cs2m$BP,cs2m$Age,col="orange")
plot(cs2m)
cor(cs2m)

dim(HR_comma_sep)
length(HR_comma_sep$average_montly_hours)

min(HR_comma_sep$average_montly_hours)
max(HR_comma_sep$average_montly_hours)
range(HR_comma_sep$average_montly_hours)
sum(HR_comma_sep$average_montly_hours)
median(HR_comma_sep$average_montly_hours)
var(HR_comma_sep$average_montly_hours)
sd(HR_comma_sep$average_montly_hours)
str(HR_comma_sep$average_montly_hours)
mode(HR_comma_sep$average_montly_hours)

head(HR_comma_sep,10)
tail(HR_comma_sep)
which(HR_comma_sep$average_montly_hours==148)
table(HR_comma_sep$Work_accident)
summary(HR_comma_sep)
hist(HR_comma_sep$average_montly_hours)
hist(HR_comma_sep$average_montly_hours,col = "blue")

boxplot(HR_comma_sep$average_montly_hours)
boxplot(HR_comma_sep$average_montly_hours,col="red")
plot(HR_comma_sep$average_montly_hours)
plot(HR_comma_sep$average_montly_hours,col="purple")
stem(HR_comma_sep$average_montly_hours)

boxplot(HR_comma_sep$average_montly_hours,horizontal="TRUE",col="red")
boxplot(HR_comma_sep$average_montly_hours)
boxplot(HR_comma_sep$average_montly_hours,HR_comma_sep$time_spend_company,col="orange")
plot(HR_comma_sep$average_montly_hours)
cor(HR_comma_sep$satisfaction_level,HR_comma_sep$last_evaluation)
boxplot(HR_comma_sep)
plot(HR_comma_sep)

l = c(600, 300, 150, 100, 200)
labels1 = c("housing", "food", "clothes", "entertainment", "others")
pie(l, labels=labels1,main = "expenses",col = rainbow(length(l)))
legend( 'topright',labels1,fill=rainbow(length(l)))
A<-1:6
s<-matrix(A,c(2,3))
s
B<-5:9
x<-matrix(B,c(2,3))
x
c=s+x
d=s-x
e=s*x
f=s/x


j = c (3,7,5,13,20,23,39,23,40,23,40,23,14,12,56,23)
get_mode <- function(j) {
  uniqj <- unique(j)
  uniqj[which.max(tabulate(match(j, uniqj)))]
}
get_mode(j)

temps <- c(35, 42, 38, 25, 28, 36, 40)
names(temps) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
barplot(temps, main = "Maximum Temperature in Celsius", xlab = "Days", ylab = "Temperature", col = "blue")
legend("topright",legend="temperature", fill = "red")

a=8
b=6
c=a+b
c
d=a-b
d
e=a*b
e
f=a/b
f
g=a%%b
g
h=a%/%b
h
j=(a^b)
j

v=c(2,5,5,6,9)
j<-c(8,2,5,14,9)
print(v<j)
print(v>j)
print(v==j)
print(v<=j)
print(v>=j)
print(v!=j)

v<-c(3,1,TRUE,2+3i)
t<-c(4,1,FALSE,2+3i)
print(v&t)
print(v|t)
print(!v)

v1= c (3,7,5,13,20,23,39)
v2= c('red','green','yellow')
print(v1)
print(v2)
class(v1)
class(v2)

sum(v1)
mean(v1)  
sd(v1)
length(v1)
rev(v1)
cumsum(v1)
sort(v1)
order(v1)
head(v1)
max(v1)
min(v1)
which.max(v1)
which.min(v1)
median(v1)
var(v1)
sd(v1)
diff(v1)
unique(c(5,5,10,100,11))
``

v=c(2,5,5,6,9)
j<-c(8,2,5,14,9)
v
j
v+10
j^2
v*j
v/j
sum(v)+sum(j)
sum(v)-sum(j)

names=c("anthony","benedict","colin","eloise","francessca","gregory","hyacinth")
sort(names)
nchar(names)
length(names)

list_1=list(x=c(10,20,30),y=c("a","b","c"), z=c(TRUE,FALSE))
print(list_1$x)
print(list_1$z)

listdata= list(c("jan","feb","mar"),matrix(c(3,9,5,1,-2,8),nrow=2),list("green",12.3))
names(listdata)=c("1st quater","a matrix","a inner list")
print(listdata)
print(listdata[1])
print(listdata$"1st quater")
print(listdata$`a matrix`)
listdata[4]="new element"
listdata
listdata[4]=NULL

m=matrix(1:9,nrow=3,ncol=3,byrow=T)
m

mat1=matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=3)
mat1
mat2=matrix(1:9,nrow=3,ncol=3,byrow=T)
mat2
sum(mat1)
mean(mat1)
sd(mat1)
length(mat1)
x=mat1+10
rowSums(mat1)
colSums(mat1)
t(mat1)
mat1+mat2

regisno=c(1,2,3,4,5,6,7)
names=c("anthony","benedict","colin","eloise","francessca","gregory","hyacinth")
age=c(25,24,23,22,21,20,19)
course=c("bcom","bcomh","bba","bms","bba","bca","bms")
percent=c(85,88,92,95,89,95,96)
details=data.frame(regisno,names,age,course,percent)
details

data=c("east","west","east","north","north","east","west","west","west","east","north")
print(data)
print(is.factor(data))
factor_data=factor(data)
print(factor_data)

gender=c("m","m","m","f","f","m","f")
newdetails=data.frame(gender)
newdetails

bindfunc=cbind(details,newdetails)
bindfunc

newdetails1=data.frame(regisno=8,names="daphne",age=18,course="bcom",percent=100,gender="f")
newdetails1
bindfunc2=rbind(bindfunc,newdetails1)
bindfunc2



x=c(1:4,NA,6:7,NA)
x
mean(x)
mean(x,na.rm=TRUE)
sum(x,na.rm=TRUE)

m=matrix(c(1:10),nrow=5,ncol=6)
m

a_m=apply(m,2,sum)
a_m
a1_m=apply(m,1,sum)
a1_m

a2_m=apply(m,2,mean)
a2_m
a2_m=apply(m,1,mean)
a2_m

movies=c("SPIDERMAN","IRONMAN","THOR","ANTMAN")
movies_lower=lapply(movies,tolower)
movies_lower
str(movies_lower)

dt=cars
dt
smn_cars=sapply(dt,min)
smn_cars

?Titanic
print(Titanic)

data(iris)
tapply(iris$Sepal.Width,iris$Species,median)

x=c(11,13,16,17,18,19,20,25,27,28,29,30,32,33,34)
x
mean(x)
median(x)
quantile(x,c(0.35,0.55))
quantile(x)
range(x)
IQR(x)
barplot(x)

z=c(2,3,3,4,4,4,4,5,6,8,8,8)
z
a=table(z)
a
sort(a,decreasing=TRUE)[1]


data=c(6,2,4,9,1,3,5)
m=c("jan","feb","march","april","may","june","july")
var(data)
sd(data)
barplot(data,names.arg=m,col="red",ylab="numbers",main="frequency",xlab = "months",border="blue")

colors=c("green","black","blue")
months=c("mar","apr","may","jun","jul")
regions=c("east","west","north")
values=matrix(c(2,9,3,11,9,4,8,7,3,12,5,2,8,10,11),nrow=3,ncol=5,byrow=T)
barplot(values,main="revenue",names.arg=months,xlab="months",ylab = "revenue",col=colors,beside=T)
legend('topleft',regions,cex=0.7,fill=colors)

amount_spent=c(211.89,134.40,90.90,68.47,93.72)
category=c("electronics","clothing and acc","dorm furnishing","school supplies","misc")
colours=c("green","blue","red","black","yellow")
barplot(amount_spent,main="college shopping",names.arg=category,xlab="category",ylab="amount spent",col=colours)
legend("topright",category,fill=colours)

histogram=c(19,23,11,5,16,21,32,14,19,27,39)
hist(histogram,xlab="weight",col=rainbow(4),border="violet",xlim =c(0,50),ylim=c(0,5),breaks=8)

hist(mtcars$mpg)
hist(mtcars$disp)

boxplot(mtcars$mpg,data=mtcars,xlab="number of cylinders",ylab="miles per gallon",main="mileage data")

input=mtcars[,c('mpg','cyl','disp')]
boxplot(input,data=mtcars,xlab="number of cylinders",ylab="miles per gallon",main="mileage data",notch=TRUE)

boxplot(mpg~cyl,data=mtcars,xlab="number of cylinders",ylab="miles per gallon",main="mileage data",notch=TRUE,col=colors)

plot(x=mtcars$wt,y=mtcars$mpg,xlab="weight",ylab="mileage",xlim = c(2.5,5),ylim = c(15,30),main="weight vs mileage",pch="a",col="black")

plot(x=women$height,y=women$weight,xlab = "height",ylab = "weight",main="height vs weight")

x=c(21,6,10,53)
places=c("london","new york","singapore","mumbai")
pipercent=c(round(100*x/sum(x),1))
print(pipercent)
pie(x,places,main="city pie chart",col=rainbow(length(x)),pipercent=places)
legend("topright",places,fill=rainbow(length(x)))
?pie

amount = c(600, 300, 150, 100, 200)
labels1= c("housing", "food", "clothes", "entertainment", "others")
pie(amount, labels=labels1,main = "expenses",col = rainbow(length(amount)))
legend( 'topright',labels1,fill=rainbow(length(amount)))

h=c(88,77,91,60,55,76,92,47,88,67,23,59,72,75,83,77,68,82,97,89,81,75,74,39,67,79,83,70,78,91,68,49,56,94,81)
stem(h)
boxplot(h)
install.packages("moments")
library(moments)
skewness(h)
hist(h)
kurtosis(h)

g=c(50,10,10)
skewness(g)

l=c(10,11,21,22,23,25)
skewness(l)
kurtosis(l)

boxplot(l,horizontal=TRUE)

p=c(sample(60:100,20),15,23,150,168)
p
boxplot(p)
lb=quantile(p,0.25)-1.5*IQR(p)
lb
ub=quantile(p,0.75)+1.5*IQR(p)
ub
p=p[p>lb & p<ub]
p
boxplot(p)

x=c(1,3,5,7,8,9)
y=c(2,4,7,9)
z=c(1,2,3,4,7)
union(x,z)
intersect(x,y)
intersect(x,z)
union(union(x,y),z)
intersect(intersect(x,y),z)
intersect(union(x,y),z)
union(intersect(y,z),intersect(x,y))
setdiff(x,z)

A=c(2,4,6,8)
B=c(3,5,7,9)
A+B
A-B
A*B
A/B

V=c(10,15,22,25,28,23,29,31,36,45,48)
stem(V)

data=c(3,7,5,13,20,23,39,23,40,23,14,12,56,23)
mean(data)
median(data)
mode=table(data)
sort(mode,decreasing=TRUE)[1]
sd(data)
var(data)

universal=c(1,2,3,6,8,10,12,16,17,24,27,31)
A=c(1,6,8,10)
B=c(3,6,10,17,24)
C=c(3,6,12,17,27,31)
union(union(A,B),C)
intersect(B,C)
setdiff(A,C)
setdiff(universal,A)

emp_id=c(1,2,3,4,5)
emp_name=c("Rick","Dan","Michelle","Ryan","Gary")
salary=c(623.30,515.20,611,729,843.25)
dept=c("IT","Operations","IT","HR","Finance")
emp_det=data.frame(emp_id,emp_name,salary,dept)
emp_det

age=c(23,22,24,25,26)
new_col=data.frame(age)
newemp_det=cbind(emp_det,new_col)
newemp_det

new_row=data.frame(emp_id=c(6,7,8),emp_name=c("John","Peter","Jim"),
                   salary=c(400.23,852,789.25),dept=c("IT","IT","HR"),age=c(27,26,25))
new_row
newemp_det2=rbind(newemp_det,new_row)
newemp_det2

genre=c("R&B","Alternative","Rap","Country","Soundtrack","Metal","Classical","Latin")
albums_sold=c(146.4,102.6,73.7,64.5,56.4,26.6,14.8,14.5)
hist(albums_sold,names.arg=genre,title="top music genre",xlab = "genre",
     ylab = "albums sold",col="red")

data(iris)
head(iris)
tail(iris)
min(iris$Sepal.Length)
sum(iris$Petal.Length)
?rbinom
rbinom(25,19,0.65)
dbinom(19,25,0.65)
pbinom(0.35,25,0.65)

?ppois
dpois(5,2.3)

coin_flip=sample(c("heads","tails"),1,replace=TRUE)
coin_flip

die_roll=sample(c(1:6),1,replace =TRUE)
print(die_roll)

gfg=rbinom(500,size = 90,prob=0.7)
gfg
mean(gfg)

plot(gfg)

install.packages("visualize")
library(visualize)
?visualize.binom
visualize.binom(5,10,0.5,section = "upper")
visualize.binom(5,10,0.5,section = "lower")
visualize.binom(5,10,0.5)

visualize.norm(2)
pnorm(c(-2,2))
visualize.norm(c(-2,2),section ="tails")
visualize.norm(c(-2,2),section = "bounded")

r=runif(10000)
hist(r)
meanr=mean(r)
meanr
sdr=sd(r)
sdr

sample(r,4)
mean_sample_4=c(sample(r,4))
for(i in 1:1000){mean_sample_4=c(mean_sample_4,mean(sample(r,4)))}
hist(mean_sample_4)

mean_mean_sample_4=mean(mean_sample_4)
mean_mean_sample_4
sd_sd_sample_4=sd(mean_sample_4)
sd_sd_sample_4

sample(r,9)
mean_sample_9=c(sample(r,9))
for(i in 1:1000){mean_sample_9=c(mean_sample_9,mean(sample(r,9)))}
hist(mean_sample_9)

mean_mean_sample_9=mean(mean_sample_9)
mean_mean_sample_9
sd_sd_sample_9=sd(mean_sample_9)
sd_sd_sample_9

sample(r,100)
mean_sample_100=c(sample(r,100))
for(i in 1:1000){mean_sample_100=c(mean_sample_100,mean(sample(r,100)))}
hist(mean_sample_100)

mean_mean_sample_100=mean(mean_sample_100)
mean_mean_sample_100
sd_sd_sample_100=sd(mean_sample_100)
sd_sd_sample_100

par(mfrow=c(2,2))
hist(r,main = "1 sample")
mtext(sdr,side=3)
mtext(meanr,side=4)

hist(mean_sample_4,main = "4 sample")
mtext(sd_sd_sample_4,side=3)
mtext(mean_mean_sample_4,side=4)

hist(mean_sample_9,main = "9 sample")
mtext(sd_sd_sample_9,side=3)
mtext(mean_mean_sample_9,side=4)

hist(mean_sample_100,main = "100 sample")
mtext(sd_sd_sample_100,side=3)
mtext(mean_mean_sample_100,side=4)

x=c(3,7,11,0,7,0,4,5,6,2)
t.test(x,mu=3,alternative = "greater")
t.test(x,mu=3,alternative = "less")

vol=c(151,153,152,152)
t.test(x=vol,mu=150,conf.level =0.95)

library(visualize)
sugar_cookie=rnorm(30,mean = 9.99,sd=0.04)
head(sugar_cookie)
visualize.t(stat=sugar_cookie,section = "tails")

batch2009=c(567,759,1029,400,998,936)
batch2015=c(820,960,700,545,769,1001)
t.test(batch2009,batch2015,var.equal=FALSE)
t.test(batch2009,batch2015,var.equal = TRUE)

species_A=c(15.2,16.5,17.8,14.9,16.1,15.7,16.8,17.3,15,5,16.2)
species_B=c(14.5,15.8,16.2,13.9,14.6,14.9,15.6,16.0,14.2,15.1)
t.test(species_A,species_B,var.equal = FALSE)
t.test(species_A,species_B,var.equal = TRUE)

#ANOVA#
size = c(3,4,5,6,4,5,6,7,7,8,9,10)
pop = c("A","A","A","A","B","B","B","B","C","C","C","C")

aov.model = aov(size ~ pop)
summary(aov.model)

print(mtcars)
aov.model = aov(mpg ~ hp)
summary(aov.model)

#CHI-SQ#
data("mtcars")
table(mtcars$carb,mtcars$cyl)
chisq.test(mtcars$carb,mtcars$cyl)

install.packages("visualize")

#Assignment#
#1

library(visualize)
values = seq(-143,143,by=0.23)
visualize.norm(stat=values,mu=4.3,sd=5.7,section="tails")
#2

coinflip1=sample(c("heads","tails"),51,replace = TRUE)
coinflip1
prob1=pbinom(26,51,0.5)
prob1
prob2=qbinom(0.25,51,0.5)
prob2

#3
hit=rnorm(80,mean = 0,sd=1)
hit
hist(hit,col=rainbow(length(hit)),xlab="values")

#4
r=runif(10000)
hist(r)
meanr=mean(r)
meanr
sdr=sd(r)
sdr

sample(r,4)
mean_sample_4=c(sample(r,4))
for(i in 1:1000){mean_sample_4=c(mean_sample_4,mean(sample(r,4)))}
hist(mean_sample_4)

mean_mean_sample_4=mean(mean_sample_4)
mean_mean_sample_4
sd_sd_sample_4=sd(mean_sample_4)
sd_sd_sample_4

sample(r,9)
mean_sample_9=c(sample(r,9))
for(i in 1:1000){mean_sample_9=c(mean_sample_9,mean(sample(r,9)))}
hist(mean_sample_9)

mean_mean_sample_9=mean(mean_sample_9)
mean_mean_sample_9
sd_sd_sample_9=sd(mean_sample_9)
sd_sd_sample_9

sample(r,100)
mean_sample_100=c(sample(r,100))
for(i in 1:1000){mean_sample_100=c(mean_sample_100,mean(sample(r,100)))}
hist(mean_sample_100)

mean_mean_sample_100=mean(mean_sample_100)
mean_mean_sample_100
sd_sd_sample_100=sd(mean_sample_100)
sd_sd_sample_100

par(mfrow=c(2,2))
hist(r,main = "1 sample")
mtext(sdr,side=3)
mtext(meanr,side=4)

hist(mean_sample_4,main = "4 sample")
mtext(sd_sd_sample_4,side=3)
mtext(mean_mean_sample_4,side=4)

hist(mean_sample_9,main = "9 sample")
mtext(sd_sd_sample_9,side=3)
mtext(mean_mean_sample_9,side=4)

hist(mean_sample_100,main = "100 sample")
mtext(sd_sd_sample_100,side=3)
mtext(mean_mean_sample_100,side=4)

#5
volume=c(151,153,152,152)
t.test(x=volume,mu=150,conf.level=0.95)

#6
data("EuStockMarkets")
SMIindex=EuStockMarkets[,"SMI"]
CACindex=EuStockMarkets[,"CAC"]
t.test(SMIindex,CACindex,var.equal=FALSE)

#7
size = c(3,4,5,6,4,5,6,7,7,8,9,10)
pop = c("A","A","A","A","B","B","B","B","C","C","C","C")
aov.model = aov(size ~ pop)
summary(aov.model)

#8
Before=c(9,8,1,3,2)
After=c(16,11,15,12,9)
drugtest=data.frame(Before,After)
t.test(drugtest$Before,drugtest$After)

#9
data("mtcars")
table(mtcars$carb,mtcars$cyl)
chisq.test(mtcars$carb,mtcars$cyl)

#10
data("ToothGrowth")
boxplot(ToothGrowth$len ~ ToothGrowth$supp, main="Tooth Length seperated by Delivery Method",
        xlab = "Delivery Method",ylab = "Tooth Length")
oj = subset(ToothGrowth,supp = "OJ")
vc = subset(ToothGrowth,supp = "VC")
t.test(oj$len,vc$len)

#correlation#
x= c(4,6,7,11,14,17,21)
y= c(18,12,13,8,7,7,4)
cor.test(x,y)

data("trees")
cor.test(trees$Height,trees$Volume)
plot(trees$Height,trees$Volume)

install.packages("MLmetrics")
library(MLmetrics)

x=c(65,67,71,71,66,75,67,70,71,69,69)
y=c(175,133,185,163,126,198,153,163,159,151,159)
mydata = data.frame(x,y)

cor_analysis= cor(mydata)
print(cor_analysis)

index= sample(2,nrow(mydata),replace = TRUE,prob = c(0.85,0.15))
index
table(index)

Training = mydata[index==1,]
Testing = mydata[index==2,]

RM = lm(x~y,data = Training)

summary(RM)

y_predict = predict(RM,Testing)
Testing$y_predict = y_predict
View(Testing)

Error = MAPE(Testing$y_predict, Testing$y)
Error

Accuracy = 1 - Error
Accuracy

#Durbin Watson Test#
install.packages("lmtest")
library(lmtest)
install.packages("dplyr")
library(dplyr)
install.packages("car")
library(car)
dwtest(RM)
vif(RM)

# Set the seed to 123
set.seed(123)

# Generate three random numbers
random_numbers_1 <- runif(3)

# Reset the seed to 123
set.seed(123)

# Generate the same three random numbers
random_numbers_2 <- runif(3)

# random_numbers_1 and random_numbers_2 will be the same
