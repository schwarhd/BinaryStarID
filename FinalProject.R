setwd("C:/Users/harrison/Documents/IA650_DataMining/Project/")
library(ggplot2)
data <- read.csv("trainingSet.csv")
str(data)
# PCA

#THE TYPES OF STARS
star.identity <- data[,1]
#THE FLUX
star.flux <- data[,2:3198]

### WARNING THIS TAKES ABOUT 2 MINUTES
star.pca <- prcomp(star.flux, center = TRUE, scale. = TRUE)
### WARNING THIS TAKES ABOUT 2 MINUTES

plot(star.pca, type = "lines")

#CANT LOG THE DATA - SOME IS NEGATIVE 

#TRANSPOSING THE DATA TABLE
library(reshape2)
flux.star <- t(star.flux)
exo1 <- ts(flux.star[,1])
plot.ts(exo1)
exo15 <- ts(flux.star[,15])
plot.ts(exo15)

#VISUALIZE ALL THE BINARY SYSTEMS
#VISUALIZE THE SERIES 1 - 10
exo10 <- ts(flux.star[,1:10])
plot.ts(exo10)
#VISUALIZE THE SERIES 11 - 20
exo20 <- ts(flux.star[,11:20])
plot.ts(exo20)
#VISUALIZE THE SERIES 21 - 30
exo30 <- ts(flux.star[,21:30])
plot.ts(exo30)
#VISUALIZE THE SERIES 31 - 40
exo37 <- ts(flux.star[,31:40])
plot.ts(exo37)

#VISUALIZE SOME NON-BINARY SYSTEMS
non47 <- ts(flux.star[,38:47])
plot.ts(non47)
non1000 <- ts(flux.star[,991:1000])
plot.ts(non1000)
non2000 <- ts(flux.star[,1991:2000])
plot.ts(non2000)

# LOOK AT JUST ONE STAR
exo15 <- ts(flux.star[,15])
plot.ts(exo15)

# WRITE JUST ONE THREE STARS TO A DATA FILE
write.csv(flux.star[,1:1000],file="exo1000.csv")
write.csv("exo3.csv")

# SMOOTHING ONE CURVE
#install.packages("smoother")
library(smoother)
smooth15 <- smth.gaussian(x = exo1, window = 40, alpha = 0.0005, tails = TRUE)
plot.ts(exo15)
plot.ts(smooth15)
library(psych)
# WRITING SUMMARY DATA FOR THE ENTIRE DATA SET
desc1 <- describe(flux.star[,1:2000])
desc2 <- describe(flux.star[,2001:4000])
desc3 <- describe(flux.star[,4001:5087])
desc3

total <- rbind(desc1,desc2)
total.data <- rbind(total, desc3)
# PULL OFF COLUMNS 1 and 2

vars <- total.data[,3:11]
vars.id <- cbind(star.identity,vars)
vars.id$star.identity <- vars.id$star.identity - 1
# VARS ID HAS SUM STATS BUT NO FOURIER
vars.id
model <- glm(star.identity ~., family=binomial(link='logit'),data=vars.id)
# HERE WE GET ERROR WARNING Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
library(lmtest)
lrtest(model)

summary(model)
library(survey)

anova(model, test = "Chisq")

# MEDIAN PERFECTLY SEPARATES THE DATA
# REMOVE MEDIAN
vars.nmid <- vars.id[, -c(4)]
vars.nmid

model1 <- glm(star.identity ~., family=binomial(link='logit'),data=vars.nmid)
summary(model1)

# SAME THING AGAIN THIS TIME WITH SKEW

vars.nsid <- vars.nmid[, -c(9)]
vars.nsid

model2 <- glm(star.identity ~., family=binomial(link='logit'),data=vars.nsid)
summary(model2)


model3 <- glm(star.identity ~ mean+sd+mad+range+trimmed+mad, family=binomial(link='logit'),data=vars.nsid)
summary(model3)

anova(model3,test = "Chisq")

fitted.results <- predict(model,newdata=vars.nsid,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != vars.nsid$star.identity)
print(paste('Accuracy',1-misClasificError))

# SO INCLUSION OF RANGE MAKES THIS STUPID

model4 <- glm(star.identity ~ mean+sd+mad, family=binomial(link='logit'),data=vars.nsid)
summary(model4)

fitted.results <- predict(model4,newdata=vars.nsid,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != vars.nsid$star.identity)
print(paste('Accuracy',1-misClasificError))

model5 <- glm(star.identity ~ mean, family=binomial(link='logit'),data=vars.nsid)
summary(model5)
# READING IN THE TEST DATA

test <- read.csv("testSet.csv")
test.identity <- test[,1]
#THE FLUX
test.flux <- test[,2:3198]
flux.test <- t(test.flux)
test.desc <- describe(flux.test)
test.id <- cbind(test.identity,test.desc)
test.id$test.identity <- test.id$test.identity - 1

fitted.results <- predict(model5,newdata=test.id,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test.id$test.identity)
print(paste('Accuracy',1-misClasificError))

table(vars.nsid$star.identity,predict(model5,type='response')>=0.5)

library(pscl)
pR2(model5)

library(ROCR)
p <- predict(model, newdata=test.id, type="response")
pr <- prediction(p, test.id$test.identity)
pr
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

rm(auc)
detach(package:ROCR, unload = TRUE)


library(mlr)
library(spectral)
vars.id$factor <- ifelse(vars.id$star.identity == 1, "planet", "no planet")
vars.factor <- vars.id[,c(2:11)]
task = makeClassifTask(data = vars.factor, target = "factor")
lrn = makeLearner("classif.lda", predict.type = "prob")
n = nrow(vars.factor)
train.set = sample(n, size = n)
modellrn = train(lrn, task, subset = train.set)

test.id$factor <- ifelse(test.id$test.identity == 1, "planet", "no planet")
test.factor <- test.id[,c(4:15)]

nt = nrow(test.factor)
test.set = sample(nt, size = nt)
predlrn = predict(modellrn, task = task, subset = test.set)

performance(predlrn)

df = generateThreshVsPerfData(predlrn, measures = list(fpr, tpr, mmce))
plotROCCurves(df)

RF = makeLearner("classif.randomForestSRC", predict.type = "prob")
modelRF = train(RF, task, subset = train.set)
predRF = predict(modelRF, task = task, subset = test.set)

performance(predRF, measures = list(mmce, acc))

lrn = makeLearner("classif.lda", predict.type = "prob")
mod = train(RF, task = task)
pred = predict(mod, task = task)

df1 <-  generateThreshVsPerfData(predRF, measures = list(fpr, tpr, mmce))
plotROCCurves(df1)
plotThreshVsPerf(df)
plotThreshVsPerf(df1)
performance(predRF, auc)

    n = getTaskSize(task)
train.set = sample(n, size = round(2/3 * n))
test.set = setdiff(seq_len(n), train.set)

lrn1 = makeLearner("classif.lda", predict.type = "prob")
mod1 = train(lrn1, task, subset = train.set)
pred1 = predict(mod1, task = task, subset = test.set)
performance(pred1,measures = auc)

nv = makeLearner("classif.naiveBayes",predict.type = "prob")
mod1 = train(nv, task, subset = train.set)
pred1 = predict(mod1, task = task, subset = test.set)
pred1

df1 <-generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
df1
df1 <- performance(pred1, measures = list(acc, fpr, tpr, mmce,auc))
df1
plotROCCurves(df1)

#feeding the naive bayes learner the entire dataset, it's no better than a coinflip
data$factor <- ifelse(data$LABEL == 2, "planet", "no planet")
ndata <- data[,2:3199]
task = makeClassifTask(data = ndata, target = "factor")
n = getTaskSize(task)
train.set = sample(n, size = round(2/3 * n))
test.set = setdiff(seq_len(n), train.set)

nv = makeLearner("classif.naiveBayes",predict.type = "prob")
mod1 = train(nv, task, subset = train.set)
pred1 = predict(mod1, task = task, subset = test.set)
performance(pred1,measures = auc)
df1 <-generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
df1
plotROCCurves(df1)

RF = makeLearner("classif.randomForestSRC", predict.type = "prob")
modelRF = train(RF, task, subset = train.set)
predRF = predict(modelRF, task = task, subset = test.set)
performance(predRF,measures = list(auc,fpr, tpr, mmce))
dfRF <-generateThreshVsPerfData(predRF, measures = list(fpr, tpr, mmce))
dfRF
plotROCCurves(dfRF)

lrn1 = makeLearner("classif.lda", predict.type = "prob")
modlrn1 = train(lrn1, task, subset = train.set)
predlrn1 = predict(modlrn1, task = task, subset = test.set)
performance(predlrn1,measures = list(auc,fpr, tpr, mmce))
dflrn <- generateThreshVsPerfData(predlrn1, measures = list(fpr, tpr, mmce))
dflrn
plotROCCurves(dflrn,pretty.names = TRUE)

