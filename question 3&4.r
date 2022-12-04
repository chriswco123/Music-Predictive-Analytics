install.packages(
  "ggplot2",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com"))
remove.packages("rlang")
install.packages("rlang")
install.packages("ggpubr")
install.packages("factoextra")
remove.packages("leaps")
install.packages("leaps")
install.packages("MLmetrics")


data <- read.csv("../桌面/GradClasses/csp571/project/tracks.csv")
sapply(data, function(x) sum(is.na(x)))
summary(data)
data$release_date <- substr(data$release_date, 0, 4)
colnames(data)[colnames(data) == "release_date"] = "release_year"
data$release_year <- as.numeric(data$release_year)
str(data)
nData <- subset(data, select=-c(id,name,artists,id_artists))
summary(nData)
k.max <- 20
set.seed(12)
elbow <- sapply(1:k.max, function(k) {kmeans(nData, k,nstart=5, iter.max=15, algorithm="Lloyd") $tot.withinss})
plot(1:k.max, elbow,
     type="b", frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
library(rlang)
library(ggplot2)
library(ggpubr)
library(factoextra)
kmc = kmeans(nData,16,iter.max=20)
fviz_cluster(kmc, nData,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
kmc




library(dplyr)
newData <- nData
newData <- newData %>%
  mutate(popular=case_when(
    popularity > 41 ~ 1,
    popularity <= 41 ~ 0
  ))
newData <- subset(newData, select=-c(popularity))

set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(newData), replace=TRUE, prob=c(0.8,0.2))
train <- newData[sample,]
test <- newData[!sample,]
x_train <- subset(train, select=-c(popular))
y_train <- as.factor(train$popular)
x_test <- subset(test, select = -c(popular))
y_test <- as.factor(test$popular)

library(corrplot)
corrplot(cor(newData), order = "hclust", 
         tl.col = "black", tl.srt = 45)
library(leaps)
library(lattice)
library(ggplot2)
library(caret)

train_control <- trainControl(method="cv", number=10)
glmModel <- train(as.factor(popular)~., data=train, trControl=train_control, method="glm")
print(glmModel)
glmPredict <- predict(glmModel,x_test)
library(MLmetrics)
MLmetrics::Accuracy(glmPredict,y_test)
#80.2 acc


lbTune <- expand.grid(nIter=c(10,15))
lbGrid <- train(as.factor(popular)~., data=train, trControl=train_control, method='LogitBoost', tuneGrid=lbTune)
print(lbGrid)
library(caTools)
lbModel <- LogitBoost(x_train, y_train, nIter=10)
lbPred <- predict(lbModel,x_test)
cm <- confusionMatrix(lbPred,y_test) 
print(cm)
#86.75 acc
importance <- varImp(lbModel, scale=FALSE)
plot(importance)


treeGrid <- train(as.factor(popular)~., data=train, trControl=train_control, method='treebag')
print(treeGrid)
treePred <- predict(treeGrid,x_test)
confusionMatrix(treePred,y_test)
#81.71
