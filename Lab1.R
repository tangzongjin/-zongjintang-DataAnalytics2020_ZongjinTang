install.packages('MASS')
library(MASS)
attach(Boston)
head(Boston)
?Boston
library(ISLR)
install.packages('ISLR')
data(auto)
data('auto')
rm(list=ls())
setwd("E:\\RPI\\da")
EPI_data <- read.csv("EPI_data.csv")
head(EPI_data)
attach(EPI_data)
fix(EPI_data)
EPI_data
tf <- is.na(EPI_data)
E <- EPI_data[!tf]
E
tf
EPI_data$EPI
summary(EPI)
fivenum(EPI,na.rm = TRUE)
stem(EPI)
hist(EPI,main = 'Hist of Zongjin' )
hist(EPI,seq(30.,95.,1.0))
line(density(EPI,na.rm = TRUE,bw = 1.))
line(density(EPI,na.rm = TRUE,bw = 'SJ'))
rug(EPI)
