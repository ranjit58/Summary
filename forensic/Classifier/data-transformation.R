setwd("~/Research16/transplant/transplant93/SVM2")

###### load ML libraries ######
library(caret)
#names(getModelInfo())
library(e1071)
################################



set.seed(100)
train_snp<-read.table(file="train_merged_file_filtered.txt",header=T,sep="\t",na.strings = "NA")
test_snp<-read.table(file="test_merged_file_filtered_noout.txt",header=T,sep="\t",na.strings = "NA")

alldata = rbind(train_snp,test_snp)
alldata$Outcome = factor(alldata$Outcome)

dim(alldata)
colnames(alldata)

selectBug = "Bacteroides_vulgatus_PC510"
#selectBug = "Bacteroides_ovatus_SD_CMC_3f"

selectSet = na.omit(alldata[,c(selectBug,"Outcome")])

#attach(selectSet)
#max_value_0 = max(selectSet[ Outcome == 0,c(selectBug)])
#min_value_1 = min(selectSet[ Outcome == 1,c(selectBug)])
#table(selectSet$Outcome)

##### Statistical tests ######
#wilcox.test(Bacteroides_vulgatus_PC510 ~ Outcome, data=selectSet) 
#wilcox.test(Bacteroides_vulgatus_PC510, mu=40)

###### fit distributions ########################
library(fitdistrplus)
plotdist(Bacteroides_vulgatus_PC510, histo = TRUE, demp = TRUE)
descdist(Bacteroides_vulgatus_PC510,boot=1000)
fw <- fitdist(Bacteroides_vulgatus_PC510, "weibull")
summary(fw)
R> fg <- fitdist(groundbeef$serving, "gamma")
R> fln <- fitdist(groundbeef$serving, "lnorm")
R> par(mfrow = c(2, 2))
R> plot.legend <- c("Weibull", "lognormal", "gamma")
4
R> denscomp(list(fw, fln, fg), legendtext = plot.legend)
R> qqcomp(list(fw, fln, fg), legendtext = plot.legend)
R> cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
R> ppcomp(list(fw, fln, fg), legendtext = plot.legend)


#### Testing data transformation########
#hist(l)
selectBug_data = selectSet[selectSet$Outcome == 0,selectBug]
#hist(selectBug_data^2,breaks=seq(0,100,by=1),xlim=c(0,100))
hist(selectBug_data)
plot(density(selectBug_data),,main="Density estimate of data")
plot(ecdf(selectBug_data),,main="Empirical cumulative distribution function")
qqnorm(selectBug_data)
abline(0,1)
qqline(selectBug_data, col = 2)

# use box-cox transformation to find what power to raise the feature.
hist(selectBug_data^2) # raise x to power of 2
hist(selectBug_data^3) # raise x to power of 3
hist(1/selectBug_data)
hist(1/sqrt(selectBug_data))
hist(selectBug_data^(1/9))
hist(selectBug_data^(1/sqrt(80)))
hist(exp(selectBug_data))
hist(sin(selectBug_data))
hist(asin(selectBug_data))
hist(asin(selectBug_data/100))


hist(sqrt(selectBug_data))
hist(log(selectBug_data))
hist(log10(selectBug_data))

#hist(log2(selectBug_data))
#curve(dnorm(x, mean=mean(selectBug_data), sd=sd(selectBug_data)), add=TRUE, col="darkblue", lwd=2)


#Normality test : The problem is that when the sample size is small, even big departures from normality are not detected, and when your sample size is large, even the smallest deviation from normality will lead to a rejected null.
#Shapiro-Wilk Normality Test : shapiro.test() function performs normality test of a data set with hypothesis that it's normally distributed.
#shapiro.test(rnorm(100, mean = 5, sd = 3))
x <- rnorm(1000, mean=0)
hist(x)
shapiro.test(x) # p-value needs to be > 0.05 for data to be normal

hist(rnorm(100, mean=0))
qqnorm(rnorm(100, mean=0))

data = selectBug_data^(1/9)

qqnorm(data)
qqline(data, col = 2)
hist(data,breaks=100)
shapiro.test(data)  

# Anderson darling test
library(nortest)
ad.test(rnorm(100000, mean = 10, sd = 2)) # p-value should be greater than 0.05 for data to be normal
ad.test(data)


#  BoxCox transformation : can work for both predictor and  response variable (based on predictors) used with a model
#The Lambda value indicates the power to which all data should be raised. In order to do this, the Box-Cox power transformation searches from Lambda = -5 to Lamba = +5 until the best value is found.
#Note that for Lambda = 0 , the logarithm of Y. 
#the Box-Cox Power transformation only works if all the data is positive and greater than 0. This, however, can usually be achieved easily by adding a constant (c) to all data such that it all becomes positive before it is transformed. 

# make sure feature is positive
# after predicting the correct lambda you can directly use power to raise values of x. but take care of 0 if you are doign manually.
library(MASS)
#Ex-1  transformation of any predictor/data
x<-rexp(1000) 
hist(x)
#out <- boxcox(lm(tmp~1))
boxcox(x~1)  # find lambda visually
boxcox(x~1, lambda = seq(0.2, 0.3, 0.01)) # find lambda visually, seems 0.25
#find correct lambda from the plot
plot = boxcox(x~1)
lambda = plot$x[which.max(plot$y)]  # gives lambda = 0.26

now you can just power raise values 
y <- x ^ 0.26
qqnorm(y)      # # Normal probability plot for transformed variable 
qqline(y, col = 2)
shapiro.test(y) #  normal and kool

#box cox transformation of a feature using car package
library(car)
x<-rexp(1000)   	# exponential sample with parameter 1 
hist(x)
qqnorm(x)			# Normal probability plot for original variable
qqline(data, col = 2)
shapiro.test(x) # not normal
boxcox(x~1)	            # Illustration of Log-Likelihood profile seems 0.3 good
p<-powerTransform(x) # Estimaton of Box-Cox lambda predict 0.29
y<-bcPower(x,p$lambda)
hist(y)
qqnorm(y)  		# # Normal probability plot for transformed variable 
qqline(y, col = 2)
shapiro.test(y) #  normal

# if response variable needs to be normal - box cox transformation can be done directly on a model using MASS package where goal is to transform outcome variable for the model.
md <-lm(O3~temp+humidity+ibh,data=ozone) # idea is to transform O3
summary(md)
plot(md,which=1)  # check residuals
bc<-boxcox(md,plotit=T)
bc<-boxcox(md,plotit=T,lambda=seq(0,0.8,by=0.1))
# find the best value of lambda
> (lambda<-bc$x[which.max(bc$y)])
[1] 0.2747475

Now incorporte lambda into new model
md_best<-lm(I(O3^lambda)~temp+humidity+ibh,data=ozone)
summary(md_best)
plot(md_best,which=1)

# https://rexplorations.wordpress.com/2015/11/03/johnson-transformation-for-non-normal-data/
# Johnson Transformation The Johnson R package can be used to access certain tried and tested transformation approaches for transformation. 
# The Johnson package contains a number of useful functions, including a normality test (Anderson-Darling, which is comparable in power to Shapiro Wilk), and the Johnson transformation function. 
x <- rweibull(1000,2,5)  #Generating a weibull distribution sample
hist(x, breaks = 20, col = rgb(0.1, 0.1, 0.9, 0.5))  #Plotting and visualizing data
shapiro.test(x)   #Shapiro-Wilk test for normality
qqnorm(x)    	# # Normal probability plot for transformed variable 
qqline(x, col = 2)


library(Johnson)
#Running the Anderson-Darling Normality Test on x
adx <- RE.ADT(x)
adx

#Running the Johnson Transformation on x
x_johnson <- RE.Johnson(x)

#Plotting transformed data
hist(x_johnson$transformed, breaks = 25, col = rgb(0.9, 0.1, 0.1, 0.5))
qqnorm(x_johnson$transformed)
qqline(x_johnson$transformed, col = "red")

#Assessing normality of transformed data
adx_johnson <- RE.ADT(x_johnson$transformed)
adx_johnson






















######################################

# partition the data method 1 manual ########
rows_total = dim(selectSet)[1]
# split data into 75 25 as training and test dataset
count_pos = round(rows_total * 0.75)
count_neg = rows_total - count_pos

index = sample(rows_total,count_pos)
trainSet = selectSet[index,]
testSet = selectSet[-index,]

# method 2 using createDataPartiotion from Caret package
#index = createDataPartition(selectSet$Outcome, p=0.7, list = FALSE, times=1 )
#trainSet = selectSet[index,]
#testSet = selectSet[-index,]


############################################


rownames(trainSet) = NULL
rownames(testSet) = NULL



###### train the algorithm ##########################

# Using Caret package
#cross_val = trainControl(method="cv",number=5)
#cross_val = trainControl(method="repeatedcv",number=5,repeats=10)
#model = train(Outcome ~.,data=trainSet, method = 'glm',family="binomial",trControl = cross_val)
#model = train(Outcome ~.,data=trainSet, method = 'svmLinear',trControl = cross_val,savePred=T, classProb=T)
#model = train(Outcome ~.,data=trainSet, method = 'svmLinear2',trControl = cross_val,savePred=T, classProb=T)
#model = train(Outcome ~.,data=trainSet, method = 'rf',trControl = cross_val)
#model = train(Outcome ~.,data=trainSet, method = 'nb',trControl = cross_val)
#model = train(Outcome ~.,data=trainSet, method = 'bayesglm',trControl = cross_val)
#model = train(Outcome ~.,data=trainSet, method = 'lda',trControl = cross_val)


# Using SVM from e1071
#model=svm(Outcome ~.,data=trainSet, kernel="linear", cost=1, gamma=0.5)
#svm_tune <- tune(svm,Outcome ~.,data=trainSet, kernel="linear", ranges=list(cost=10^(-1:3), gamma=c(.1,.2,.3,.4,.5,1,2,3,4,5)))
model=svm(Outcome ~.,data=trainSet, kernel="linear", cost=100, gamma=0.1,probability=T,cross=5)
#model=svm(Outcome ~.,data=trainSet, kernel="linear", cost=100, gamma=0.1,probability=T,cross=5,class.weights = c('0'=100, '1' = 1))

###############################

model
#only runs with model created by caret package
#confusionMatrix(model)



#### Testing ################

# testing training data itself
predictions <- predict(model,decision.values = T,probability=T)
cm = confusionMatrix(predictions,trainSet[,c("Outcome")])
#cm$overall
#cm$overall['Accuracy']
#cm$byClass
#cm$byClass['Sensitivity']
cm_overall = as.data.frame(t(cm$overall))
cm_byClass = as.data.frame(t(cm$byClass))
stats = cbind(selectBug,cm_overall,cm_byClass,max_value_0,min_value_1)


# testing on test dataset
predictions <- predict(model, newdata = testSet,decision.values = T,probability=T)
confusionMatrix(predictions,testSet[,c("Outcome")])

#table (Prediction = predictions, Truth = testSet[,c("Outcome")])


# tesing on new linear dataset from 80 to 100
testNew = as.data.frame(seq(80,100,by=0.5))
colnames(testNew) = selectBug

predictions <- predict(model, newdata = testNew,decision.values = T,probability=T)

cbind(testNew,predictions,attr(predictions, "probabilities"))
#cbind(testNew,predictions)



for (selectBug in colnames(alldata[,3:ncol(alldata)-1])){
  print(paste("The bug is", selectBug))
}


