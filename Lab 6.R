install.packages('titanic')
library(titanic)
head(titanic_train)
train <- titanic_train
test <- titanic_test

install.packages('randomForest')
require(randomForest)
fitKF <- randomForest(Survived~.,   data=train)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

# other data....
data(imports85)