#installing the packages required

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)
library(rpart)
library(rpart.plot)

df = read.csv('insurance.csv',stringsAsFactors = TRUE) #variables sex, smoker and region are in 'chr' type, factoring it because they are categorical
View(df)
summary(df)

#cleaning data

any(is.na(df))
str(df)

#EDA

plot1 <- ggplot(df, aes(charges)) +geom_histogram(col="#009966", fill="#66FFCC") + labs(title = "Insurance charges summary", x="charges", y="frequency")
print(plot1)
plot2 <-  ggplot(df) + geom_histogram(mapping=aes(charges, fill=smoker))
print(plot2)
plot3 <-  ggplot(df) + geom_histogram(mapping=aes(charges, fill=sex))
print(plot3)
plot4 <-  ggplot(df) + geom_boxplot(mapping=aes(y=bmi, col=region))
print(plot4)
plot5 <- ggplot(df,aes(x=age,y=charges)) +geom_point(size=2,shape=18,color="#FF3366") + geom_smooth(method=lm)
print(plot5)
plot6 <- ggplot(df,aes(x=bmi,y=charges)) +geom_point(size=2,shape=18,color="#FF9900") + geom_smooth(method=lm)
print(plot6)

summary(df$charges)
table(df$region)
table(df$sex)
table(df$smoker)

# correlation analysis #multiple regression

cor(df$age, df$charges)
cor(df$bmi, df$charges)
cor(df$children,df$charges)

#correlation matrix

cor.matrix <- cor(df[c("age","bmi", "children", "charges")])
print(cor.matrix)

numeric.columns <-  sapply(df,is.numeric)
cor.data <- cor(df[,numeric.columns])
print(corrplot(cor.data,method='color')) #visualising correlation, every feature should be perfectly co-related with itself
corrgram(df,order=TRUE,lower.panel = panel.shade, upper.panel = panel.pie, text.panel=panel.txt)

#train-test split

library(caTools)
set.seed(101)
sample <- sample.split(df$charges,SplitRatio=0.8)
sample.train <- subset(df,sample==TRUE)
sample.test <- subset(df,sample==FALSE)
nrow(sample.train)
nrow(sample.test)

linear.model <- lm(charges~., data=sample.train)
summary(linear.model)

fit.model <-  rpart(charges~., data = sample.train, method = "anova")
prediction <-  predict(fit.model,sample.test)
fit.model

prp(fit.model)
rpart.plot(fit.model)
printcp(fit.model)

# improving the model

df$age.new <- df$age^1.5
df$bmi.new <- ifelse(df$bmi >= 30, 1, 0)
linear.model2 <- lm(charges ~ age + age.new + children + bmi + sex + bmi.new*smoker + region, data =df)

summary(linear.model2)


