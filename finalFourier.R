setwd("C:/Users/harrison/Documents/IA650_DataMining/Project/")
library(ggplot2)
fourier <- read.csv("fourier.csv",header = T)
fourierModel <- glm(factor ~., family=binomial(link='logit'),data=fourier)
summary(fourierModel)
anova(fourierModel, test = 'Chisq')

fourierCount <- glm(factor ~Count, family=binomial(link='logit'),data=fourier)
summary(fourierCount)

library(ROCR)
library(lmtest)
library(mlr)
p <- predict(fourierCount, newdata=fourier, type="response")
pr <- prediction(p, fourier$factor)
pr
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

lrtest(fourierCount)


rm(auc)
detach(package:ROCR, unload = TRUE)

fourier$factor <- ifelse(fourier$factor == 1, "planet", "no planet")


#feeding the naive bayes learner the entire dataset, it's no better than a coinflip
task = makeClassifTask(data = fourier, target = "factor")
n = getTaskSize(task)
train.set = sample(n, size = round(2/3 * n))
test.set = setdiff(seq_len(n), train.set)

FOURnv = makeLearner("classif.naiveBayes",predict.type = "prob")
FOURmod1 = train(FOURnv, task, subset = train.set)
FOURpred1 = predict(FOURmod1, task = task, subset = test.set)
performance(FOURpred1,measures = auc)
FOURdf1 <-generateThreshVsPerfData(FOURpred1, measures = list(fpr, tpr, mmce))
FOURdf1
plotROCCurves(FOURdf1)

FOURRF = makeLearner("classif.randomForestSRC", predict.type = "prob")
FOURmodelRF = train(FOURRF, task, subset = train.set)
FOURpredRF = predict(FOURmodelRF, task = task, subset = test.set)
performance(FOURpredRF,measures = auc)
FOURdfRF <-generateThreshVsPerfData(FOURpredRF, measures = list(fpr, tpr, mmce))
FOURdfRF
plotROCCurves(FOURdfRF)

FOURlrn1 = makeLearner("classif.lda", predict.type = "prob")
FOURmodlrn1 = train(FOURlrn1, task, subset = train.set)
FOURpredlrn1 = predict(FOURmodlrn1, task = task, subset = test.set)
performance(FOURpredlrn1,measures = auc)
FOURdflrn <- generateThreshVsPerfData(FOURpredlrn1, measures = list(fpr, tpr, mmce))
FOURdflrn
plotROCCurves(FOURdflrn)
