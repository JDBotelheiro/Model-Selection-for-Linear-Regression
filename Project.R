library(MASS)
library(caTools)
library(leaps)
library(ggplot2)
library(fmsb)
library(corrplot)
library(GGally)
library(Hmisc)
library(ggrepel)
library(car)
library(glmnet)

#loading data
data_original <- read.table("/Users/Joao/Desktop/AML/Dados21.txt", header = FALSE)
colnames(data_original) <- c("Id","PSA","Cancer_Vol","Weight","Age","Bph","Svi","Capsular_pen","Gl_Score")
data_original <- subset(data_original, select = names(data_original)[-c(1)])
#We gonna use this set for transformation
data <- data_original
summary(data)


#EXPLORATORY DATA ANALYSIS
plot_missing(data)
PSA <- ggplot(data, aes(x = data$PSA)) + geom_histogram()
PSA
age <- ggplot(data, aes(x = data$Age)) + geom_histogram()
age
Weight <- ggplot(data, aes(x = data$Weight)) + geom_histogram()
Weight
Canser_Vol <- ggplot(data, aes(x = data$Cancer_Vol)) + geom_histogram()
Canser_Vol
Bph <- ggplot(data, aes(x = data$Bph)) + geom_histogram()
Bph
Svi <- ggplot(data, aes(x = data$Svi)) + geom_histogram()
Svi
Capsular_pen <- ggplot(data, aes(x = data$Capsular_pen)) + geom_histogram()
Capsular_pen



#we find a unnatural value in weights so we decide to change by the median value of the data set:
mean.data <- c(data$Weight[0:31],data$Weight[33:97])
mean_weight <- mean(mean.data)
#mean of weight = 41.2742 
data$Weight[32] <- mean_weight
#new wight varaible
Weight <- ggplot(data, aes(x = data$Weight)) + geom_histogram()
Weight

#How can age & weight influence the Gl_score?
ggplot(data, aes(x=data$Age, y=data$Weight))+ geom_point(aes(color = data$Gl_Score))

#correlation matrix
o.cor <- rcorr(as.matrix(data))
o.cor
ggcorr(data[, 1:8], geom = "blank", label = TRUE, hjust = 0.75) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 0)) + guides(color = FALSE, alpha = FALSE)

#Variable response 
ggplot(data, aes(x = data$Cancer_Vol, y = data$PSA))+ geom_point()
ggplot(data, aes(x = data$Cancer_Vol, y = sqrt(data$PSA)))+ geom_point()
ggplot(data, aes(x = data$Cancer_Vol, y = 1/(data$PSA)))+ geom_point()

#General plots of the variabels
p <- ggpairs(data, upper = list(continuous = "points"), lower = list(continuous = "cor"))
print(p)
#Applaying log to PSA
data$PSA = log(data$PSA)

#Cancer_vol
ggplot(data, aes(x = log10(data$Cancer_Vol), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm", color = "red")
ggplot(data, aes(x = 1/(data$Cancer_Vol), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm", color = "red")
ggplot(data, aes(x = sqrt(data$Cancer_Vol), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm", color = "red")
data$Cancer_vol <-sqrt(data$Cancer_Vol)

#Weight
ggplot(data, aes(x = log10(data$Weight), y = log10(data$PSA)))+ geom_point()+ geom_smooth(method = "lm", color = "red")
ggplot(data, aes(x = 1/(data$Weight), y = log10(data$PSA)))+ geom_point()+ geom_smooth(method = "lm", color = "red")
ggplot(data, aes(x = sqrt(data$Weight), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm", color = "red")
data$Weight <- log(data$Weight)

#Age
ggplot(data, aes(x = log10(data$Age), y = log10(data$PSA))) + geom_point()+ geom_smooth(method = "lm",color = "red")
ggplot(data, aes(x = 1/(data$Age), y = log10(data$PSA))) + geom_point() + geom_smooth(method = "lm",color = "red")
ggplot(data, aes(x = sqrt(data$Age), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm",color = "red")

#Bph
ggplot(data, aes(x = log10(data$Bph), y = log10(data$PSA))) + geom_point()+ geom_smooth(method = "lm",color = "red")
ggplot(data, aes(x = 1/(data$Bph), y = log10(data$PSA))) + geom_point() + geom_smooth(method = "lm",color = "red")
ggplot(data, aes(x = sqrt(data$Bph), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm",color = "red")

#Capsular_pen
ggplot(data, aes(x = log10(data$Capsular_pen), y = log10(data$PSA))) + geom_point()+ geom_smooth(method = "lm",color = "red")
ggplot(data, aes(x = 1/(data$Capsular_pen), y = log10(data$PSA))) + geom_point() + geom_smooth(method = "lm",color = "red")
ggplot(data, aes(x = sqrt(data$Capsular_pen), y = log10(data$PSA)))+ geom_point() + geom_smooth(method = "lm",color = "red")


#Lm(256 modelos possiveis) - fiting the best model
full.model <- lm(PSA ~ Cancer_Vol + Weight + Age + Bph + Svi + Capsular_pen + Gl_Score, data = data)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)

#step = step(full.model)
summary(step.model)
# we get formula=PSA ~ Cancer_Vol + Weight + Bph + Svi + Gl_Score


#Graphs
par(mfrow=c(2,3))
plot(step.model, which = c(1,4,6))
plot(data$Capsular_pen, step.model$residuals, main="Residuals vs ts")
#Horizontal line at zero
abline(h = 0, col = "gray75")
hist(step.model$residuals)
#Horizontal line at zero
abline(h = 0, col = "gray75")
#Normality of Residuals
qqPlot(step.model$residuals, las = 1, main="QQ Plot", id = TRUE, col.lines = "Red")
#Histogram of residuals
hist(step.model$residuals)

#Try to find the values outside qqplot line
order(step.model$residuals) 

#Train & Test
set.seed(123)
sample = sample.split(data,SplitRatio = 0.70)
train1 = subset(data,sample == TRUE)
test1 = subset(data, sample == FALSE)

model = lm(formula = PSA ~ Cancer_Vol + Weight + Bph + Svi + Gl_Score, data = train1)
result = predict(model, test1)
results = data.frame(actual = exp(test1$PSA), prediction = exp(result))

#Prediction accuracy and error rates
correlation_accuracy = cor(results)  
head(results)

#accuracy = mean(apply(results, 1, min) / apply(results, 1, max)) 
#accuracy


measures <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}
measures(test1$PSA,result)



####Dirty Analysis####
#trying to get a better result
data2 <- data[-c(1, 4, 39), ] 

#applying again the linear model
set.seed(123)
sample2 = sample.split(data2,SplitRatio = 0.70)
train2 = subset(data2,sample2 == TRUE)
test2 = subset(data2, sample2 == FALSE)

model2 = lm(formula = PSA ~ Cancer_Vol + Weight + Bph + Svi + Gl_Score, data = train2)
result2 = predict(model2, test2)
results2 = data.frame(actual = exp(test2$PSA), prediction = exp(result2))

#Measures
correlation_accuracy = cor(results2)  
head(results2)

measures <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}
measures(test2$PSA,result2)

#trying to get a better result, another try
data3 <- data[-c(4, 39), ] 

#applying again the linear model
set.seed(123)
sample3 = sample.split(data3,SplitRatio = 0.70)
train3 = subset(data3,sample3 == TRUE)
test3 = subset(data3, sample3 == FALSE)

model3 = lm(formula = PSA ~ Cancer_Vol + Weight + Bph + Svi + Gl_Score, data = train3)
result3 = predict(model3, test3)
results3 = data.frame(actual = exp(test3$PSA), prediction = exp(result3))

#Measures
correlation_accuracy = cor(results3)  
head(results3)

measures <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}
measures(test3$PSA,result3)


#Without any transformation
#applying again the linear model
set.seed(123)
sample4 = sample.split(data_original,SplitRatio = 0.70)
train4 = subset(data_original,sample4 == TRUE)
test4 = subset(data_original, sample4 == FALSE)

model4 = lm(formula = PSA ~ Cancer_Vol + Weight + Bph + Svi + Gl_Score, data = train4)
result4 = predict(model4, test4)
results4 = data.frame(actual = exp(test4$PSA), prediction = exp(result4))

#Measures
correlation_accuracy = cor(results4)  
head(results4)

measures <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}
measures(test4$PSA,result4)





#Tryng this time with BIC
step.model <- stepAIC(full.model,k=log(nrow(data)))

#step = step(full.model)
summary(step.model)
# we get formula=PSA ~ Cancer_Vol + Weight + Bph + Svi + Gl_Score


#Graphs
#par(mfrow=c(2,3))
plot(step.model, which = c(1,4,6))
plot(data$Capsular_pen, step.model$residuals, main="Residuals vs ts")
#Horizontal line at zero
abline(h = 0, col = "gray75")
plot(data$Bph, step.model$residuals, main="Residuals vs cod")
#Horizontal line at zero
abline(h = 0, col = "gray75")
#Normality of Residuals
qqPlot(step.model$residuals, las = 1, main="QQ Plot", id = TRUE, col.lines = "Red")
#Histogram of residuals
hist(step.model$residuals)


