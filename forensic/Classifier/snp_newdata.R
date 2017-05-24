setwd("~/Research16/transplant/transplant93/SVM2")
###### load ML libraries ######
library(caret)
#names(getModelInfo())
library(e1071)

################################



set.seed(100)
#train_snp<-read.table(file="train_merged_file_filtered.txt",header=T,sep="\t",na.strings = "NA")
#test_snp<-read.table(file="test_merged_file_filtered_noout.txt",header=T,sep="\t",na.strings = "NA")

#alldata = rbind(train_snp,test_snp)
#alldata$Outcome = factor(alldata$Outcome)

alldata = read.table(file="hmp_new.txt",header=F,sep="\t",na.strings = "NA")
colnames(alldata) = c("Microbe","Similarity","Outcome")
#dim(alldata)
#colnames(alldata)
#unique(alldata[,1])

selectBug = "Acidaminococcus_sp_D21"
#selectBug = "Bacteroides_ovatus_SD_CMC_3f"

selectSet = subset(alldata, Microbe == selectBug, select = c(Similarity,Outcome))
selectSet$Outcome = factor(selectSet$Outcome)
rownames(selectSet) = NULL

attach(selectSet)
max_value_0 = max(selectSet[ Outcome == 0,c("Similarity")])
min_value_1 = min(selectSet[ Outcome == 1,c("Similarity")])
table(selectSet$Outcome)

##### Statistical tests ######
#wilcox.test(Bacteroides_vulgatus_PC510 ~ Outcome, data=selectSet) 
#wilcox.test(Bacteroides_vulgatus_PC510, mu=40)
###### fit distributions ########################
#library(fitdistrplus)
#plotdist(Bacteroides_vulgatus_PC510, histo = TRUE, demp = TRUE)
#descdist(Bacteroides_vulgatus_PC510,boot=1000)
#fw <- fitdist(Bacteroides_vulgatus_PC510, "weibull")
#summary(fw)
#R> fg <- fitdist(groundbeef$serving, "gamma")
#R> fln <- fitdist(groundbeef$serving, "lnorm")
#R> par(mfrow = c(2, 2))
#R> plot.legend <- c("Weibull", "lognormal", "gamma")
#4
#R> denscomp(list(fw, fln, fg), legendtext = plot.legend)
#R> qqcomp(list(fw, fln, fg), legendtext = plot.legend)
#R> cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
#R> ppcomp(list(fw, fln, fg), legendtext = plot.legend)


####testing data transformation########
#hist(l)
#selectBug_data = selectSet[Outcome == 0, selectBug]
#hist(selectBug_data^2,breaks=seq(0,100,by=1),xlim=c(0,100))
#hist(selectBug_data)
#plot(density(selectBug_data),,main="Density estimate of data")
#plot(ecdf(selectBug_data),,main="Empirical cumulative distribution function")
#qqnorm(selectBug_data)
#abline(0,1)
#qqline(selectBug_data, col = 2)

#hist(selectBug_data^2) # raise x to power of 2
#hist(selectBug_data^3) # raise x to power of 3
#hist(1/selectBug_data)
##hist(1/sqrt(selectBug_data))
#hist(selectBug_data^(1/9))
#hist(selectBug_data^(1/sqrt(80)))
#hist(exp(selectBug_data))
#hist(sin(selectBug_data))
#hist(asin(selectBug_data))


#hist(sqrt(selectBug_data))
#hist(log(selectBug_data))
#hist(log10(selectBug_data))

#Normality test : The problem is that when the sample size is small, even big departures from normality are not detected, and when your sample size is large, even the smallest deviation from normality will lead to a rejected null.
#Shapiro-Wilk Normality Test : shapiro.test() function performs normality test of a data set with hypothesis that it's normally distributed.
#shapiro.test(rnorm(100, mean = 5, sd = 3))
#x <- rnorm(100, mean=0)
#hist(x)
#shapiro.test(x) # p-value needs to be > 0.05 for data to be normal

#hist(rnorm(100, mean=0))
#qqnorm(rnorm(100, mean=0))

#data = selectBug_data^(1/9)

#qqnorm(data)
#qqline(data, col = 2)
#hist(data,breaks=100)
#shapiro.test(data)  

#library(nortest)
#ad.test(rnorm(100000, mean = 10, sd = 2)) # p-value should be greater than 0.05 for data to be normal
#ad.test(data)

#library(MASS)
#boxcox(College)


#hist(log2(selectBug_data))
#curve(dnorm(x, mean=mean(selectBug_data), sd=sd(selectBug_data)), add=TRUE, col="darkblue", lwd=2)

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
cross_val = trainControl(method="repeatedcv",number=5,repeats=10)
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
model=svm(Outcome ~.,data=trainSet, kernel="linear", cost=10, gamma=0.1,probability=T,cross=5)
#model=svm(Outcome ~.,data=trainSet, kernel="linear", cost=100, gamma=0.1,probability=T,cross=5,class.weights = c('0'=100, '1' = 1))

###############################

model
#only runs with model created by caret package
#confusionMatrix(model)



#### Testing ################

# testing training data itself

predictions <- predict(model,decision.values = T,probability=T)
table(predictions)
cm = confusionMatrix(predictions,trainSet[,c("Outcome")])
table(trainSet[,c("Outcome")])
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
testNew = as.data.frame(seq(80,100,by=0.1))
colnames(testNew) = c("Similarity")

predictions <- predict(model, newdata = testNew,decision.values = T,probability=T)

cbind(testNew,predictions,attr(predictions, "probabilities"))
#cbind(testNew,predictions)

# test on transplant dataset
transplant_data = read.table(file="transplant3.txt",header=F,sep="\t",na.strings = "NA")
colnames(transplant_data) = c("Microbe","Similarity","Outcome")

TestSet2 = subset(transplant_data, Microbe == selectBug , select = c(Similarity))
rownames(TestSet2) = NULL

predictions <- predict(model, newdata = TestSet2,decision.values = T,probability=T)
cbind(TestSet2$Similarity,predictions)
cbind(TestSet2,predictions,attr(predictions, "probabilities"))

TestSet3 = subset(transplant_data, Microbe == selectBug , select = c(Similarity,Outcome))
rownames(TestSet3) = NULL

predictions <- predict(model, newdata = TestSet3,decision.values = T,probability=T)
cbind(TestSet3$Similarity,predictions)
cbind(TestSet3,predictions,attr(predictions, "probabilities"))
confusionMatrix(predictions,TestSet3[,c("Outcome")])
#cbind(testNew,predictions)


#for (selectBug in colnames(alldata[,3:ncol(alldata)-1])){
#  print(paste("The bug is", selectBug))
#}


