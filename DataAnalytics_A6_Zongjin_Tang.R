rm(list=ls())
setwd("E:RPI\\da")
#load data set
library(openxlsx)
df<- read.xlsx("df.xlsx", sheet = 1)
df1<- read.xlsx("df.xlsx", sheet = 1,startRow = 2)
#Title combination
head(df)
head(df1)
header1 <- colnames(df)
header2 <- colnames(df1)
header2
header2 <- gsub('^X.|^X..',' ',header2)
names(df1) <- paste0(header1,header2)
#Column Dete
dim(df1)
summary(df1)
str(df1) 
unique(df1$sample_depth_mmeters)
df1 <- df1[,-c(4,6:10,26)]
head(df1)
#Missing Value
sum(is.na(df1))
aaa <- which(colSums(is.na(df1)) > 0)
a <- sort(colSums(sapply(df1[aaa], is.na)), decreasing = TRUE)
a
column <- c('density_sigmatkg/m3','conductivity_smSiemens/m','pressure_dbdecibars','salinity_ctdPSU')
d<-  c(1492,1024,1024,332)
a <- data.frame(column,d)
library(ggplot2)
ggplot(data=a,mapping=aes(x=column,y=d,fill=column,group=factor(1)))+
  geom_bar(stat="identity")+
  geom_text(aes(label = d, vjust = 1.2, hjust = 0.5), show.legend = TRUE)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))
df1 <- df1[,-c(12,15,16)]
df1$salinity_ctdPSU
summary(df1$salinity_ctdPSU)
ggplot(df1,aes(x=df1$salinity_ctdPSU))+geom_histogram(binwidth = 0.5,color='black',fill='white')+
  xlim(20,50)
df1$salinity_ctdPSU[is.na(df1$salinity_ctdPSU)] <- 35.10
ggplot(df1,aes(x=df1$salinity_ctdPSU))+geom_histogram(binwidth = 0.5,color='black',fill='white')+
  xlim(20,50)
#Correlation
summary(df1)
as.numeric(df1)
colnames(df1)
Corrdf1 <- df1[,c(2:3,6:13)]
library(reshape)
library(ggplot2)
cormat <- round(cor(Corrdf1), 2)
cormat_melt <- melt(cormat)
upper_tri <- ggplot(cormat_melt, aes(x=X1, y=X2))
ggplot(data=cormat_melt, aes(x=X1, y=X2, fill=value))+
  geom_tile(color="white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,space = "Lab", limit=c(-1, 1),  name="Person\nCorrelation")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1))+
  coord_fixed()
#model 1
head(df1)
unique(df1$sample_depth_mmeters)
df1$deep[df1$sample_depth_mmeters==0]<- 0
df1$deep[df1$sample_depth_mmeters==15] <- 1
reg1 <-df1[,c(2:3,6:13,17)]
reg1 <- reg1[,-1:-2]
boxplot(reg1$`dicµmol/kg`,main = 'DIC')
boxplot(reg1$`taµEq/kg`,main = 'TA')
boxplot(reg1$`ph `,main = 'PH')
boxplot(reg1$pco2µatm,main = 'Pco2')
boxplot(reg1$`aragonite_sat `,main = 'aragonite')
boxplot(reg1$salinity_bottlePSU,main = 'salinity_bot')
boxplot(reg1$salinity_ctdPSU,main = 'salinity_ctd')
boxplot(reg1$temperature_cdegree.C,main = 'temperature')

reg1=reg1[-which(reg1$salinity_ctdPSU ==999),]
reg1<-reg1[-which(reg1$salinity_ctdPSU== -999),]
boxplot(reg1$salinity_ctdPSU,main = 'salinity_ctd')
reg1$target <- reg1$`dicµmol/kg`
reg1 <-reg1[,-1]
#train & test
set.seed(1)
sub<-sample(1:nrow(reg1),round(nrow(reg1)*0.8))
length(sub)
data_train<-reg1[sub,]#get 0.8 train data set
data_test<-reg1[-sub,]#get 0.2 test data set
dim(data_train)
dim(data_test)
colnames(data_train)
ln <- lm(target~.,data = data_train)
par(mfrow = c(2,2))
summary(ln)
plot(ln)
ln_pre<-predict(ln,data_test)
ggplot(data_train,aes(x=ln_pre,y=target))+
  geom_point(size=1 , color = "steelblue")+
  geom_smooth(method = "loess", color="darkred")+
  labs(x="Predicted", y="Actual")
plot(ln_pre)

RMSE <- function(x,y){
  a <- round(sqrt(sum((log(x)-log(y))^2)/length(y)),5)
  return(a)
}
RMSE(data_test$target, ln_pre)

ggplot(data_test,aes(x=ln_pre,y=target))+
  geom_point(size=1 , color = "steelblue")+
  geom_smooth(method = "loess", color="darkred")+
  labs(x="Predicted", y="Actual")+
  theme(plot.margin = unit(c(2,2,2,2), "cm"))


#Model 2
header1 <- colnames(df)
header2 <- colnames(df1)
header2 <- gsub('^X.|^X..',' ',header2)
names(df1) <- paste0(header1,header2)
df1 <- df1[,-c(4,6:10,26)]
df1 <- df1[,-c(12,15,16)]
df1$salinity_ctdPSU[is.na(df1$salinity_ctdPSU)] <- 35.10
library(lubridate)
df1$td <- as.Date(df1$timeUTC)
df1$year <- year(df1$td)
df1$month <- month(df1$td)
reg2 <- df1
reg2$target <- reg2$`dic��mol/kg`
reg2 <- reg2[,-c(1,17)]
reg2 <- reg2[,-16]
reg2$season[reg2$month==12|reg2$month==1|reg2$month==2] <- 'winter'
reg2$season[reg2$month <= 5 & reg2$month >= 3] <- 'spring'
reg2$season[reg2$month <= 8 & reg2$month >= 6] <- 'summer'
reg2$season[reg2$month <= 11 & reg2$month >= 9] <- 'autumn'
library(ggplot2)
ggplot(data=reg2,aes(x=season,fill = season))+
  geom_bar(stat="count")+
  labs(x="Season", y="Times")
colnames(reg2)
ggplot(data=reg2,aes(x=latitudedegrees_north,y = longitudedegrees_east))+
  geom_point()
reg2<-reg2[-which(reg2$salinity_ctdPSU== -999),]
reg3 <- reg2
reg2 <- reg3
reg2 <- reg2[-which(reg2$latitudedegrees_north == -999),]
ggplot(data=reg2,aes(x=latitudedegrees_north,y = longitudedegrees_east))+
  geom_point()
fivenum(reg2$latitudedegrees_north)
fivenum(reg2$longitudedegrees_east)
install.packages('ggmap')
unique(reg2$`region `)
reg2 <- reg2[,-1:-2]
reg2$depth[reg2$sample_depth_mmeters == 0] <- 'shallow'
reg2$depth[reg2$sample_depth_mmeters == 15] <- 'deep'
reg2 <- reg2[,-2]
reg2 <- reg2[,-10]
library(ggplot2)
ggplot(data = reg2,aes(x=target,fill=target))+
  geom_histogram(bins = 100,fill="magenta")+
  theme(panel.background = element_rect(fill = 'green', colour = 'black'))+
  xlim(1600,2400)
#find the bond of target
aaa<- reg2$target
ggplot(data=reg2,aes(x=target))+geom_boxplot()
fivenum(aaa)
summary(aaa)
quantile(aaa,0.4)
quantile(aaa,0.8)
#classification model
reg2$Target[reg2$target <= 2000] <- 'low'
reg2$Target[reg2$target > 2000 & reg2$target < 2070] <- 'medium'
reg2$Target[reg2$target >= 2070] <- 'high'
reg2 <- reg2[,-13]
reg2$region <- reg2$`region `
reg2 <- reg2[,-1]
reg2 <- reg2[,-1]
#train & test
set.seed(3)
sub<-sample(1:nrow(reg2),round(nrow(reg2)*2/3))
length(sub)
data_train<-reg2[sub,]#get 2/3 train data set
data_test<-reg2[-sub,]#get 1/3 test data set
dim(data_train)
install.packages('randomForest')
install.packages('C50')
library('randomForest')
library('rpart')
library('rpart.plot')
model2 <- rpart(Target~.,data = reg2,method = 'class')
rpart.plot(model2)
reg3 <- reg2[,-1]
model3 <- rpart(Target~.,data = reg3)
rpart.plot(model3)
