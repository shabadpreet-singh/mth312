data <- read.csv("wdbc.data", header = FALSE)
head(data)

# removing the first column 
data <- data[,-1]

y <- data[,1]
X <- data[,-1]

# standardization
x <- scale(X)

p <- dim(x)[2]
n <- dim(x)[1]
for(i in 1:n)
{
  if(y[i]=='M')
    y[i] = 1
  else
    y[i] = 0
}
y <- as.numeric(y)

# depth based
library(ddalpha)

mn <- numeric(length = 100)
for( i in 1:100){
idx <- sample(1:n,0.2*n) # 80:20
x_train <- x[-idx,]
y_train <- y[-idx]

x_test <- x[idx,]
y_test <- y[idx]

idx1 <- which(y_train==1, arr.ind=T)
x1 <- x_train[idx1,]
x2 <- x_train[-idx1,]

dist1 <- depth.halfspace(x_test, x1)
dist2 <- depth.halfspace(x_test, x2)
y_pred <- as.numeric(dist1>dist2)

mn[i] <- mean(y_pred == y_test)}
mean(mn) # 0.8086726
sd(mn) # 0.04144669
1-0.8086726

mn <- numeric(length = 100)
for( i in 1:100){
  idx <- sample(1:n,0.3*n) # 70:30
  x_train <- x[-idx,]
  y_train <- y[-idx]
  
  x_test <- x[idx,]
  y_test <- y[idx]
  
  
  idx1 <- which(y_train==1, arr.ind=T)
  x1 <- x_train[idx1,]
  x2 <- x_train[-idx1,]
  
  dist1 <- depth.halfspace(x_test, x1)
  dist2 <- depth.halfspace(x_test, x2)
  y_pred <- as.numeric(dist1>dist2)
  
  mn[i] <- mean(y_pred == y_test)}
mean(mn) # 0.7868824
sd(mn) # 0.0294041
1- 0.7868824
  
# SVM

library(e1071)
data$V2 <- as.numeric(as.factor(data$V2))

mn <- numeric(length = 100)
for( i in 1:100){
idx <- sample(1:n,0.2*n) # 80:20
dat_train <- data[-idx,]
dat_test <- data[idx,]
x_test <- dat_test[,-1]
y_test <- dat_test[,1]

model <- svm(V2 ~ ., data = dat_train, kernel = 'linear', scale = T, type = "C-classification")
y_pred2 <- predict(model, x_test )
mn[i] <- mean(y_test == y_pred2)}

1-mean(mn) # 0.970354
sd(mn) # 0.01547555

mn <- numeric(length = 100)
for( i in 1:100){
  idx <- sample(1:n,0.3*n) # 70:30
  dat_train <- data[-idx,]
  dat_test <- data[idx,]
  x_test <- dat_test[,-1]
  y_test <- dat_test[,1]
  
  model <- svm(V2 ~ ., data = dat_train, kernel = 'linear', scale = T, type = "C-classification")
  y_pred2 <- predict(model, x_test )
  mn[i] <- mean(y_test == y_pred2)}

1-mean(mn) # 0.9703529
sd(mn) # 0.01152216


# knn

library(caret)
cross_val <- trainControl(method = "repeatedcv",number=12,repeats=3)
metric <- "Accuracy"

##dataset 1
grid <- expand.grid(.k=seq(1,20,by=1))

cf <- numeric(length = 100)
for(i in 1:100){
model.knn <- train(V2~., data=dat_train, method="knn", 
                   metric=metric, tuneGrid=grid, trControl=cross_val)
knn.k2 <- model.knn$bestTune 
print(model.knn)
plot(model.knn, main = "Accuracy vs. k for KNN dataset-1")
y_pred.knn_1 <- predict(model.knn, x_test)
cf[i] <- mean(y_pred.knn_1== y_test)}
mean(cf)
sd(cf)


#4. KDE Based Classifier
n <- dim(data)[1]
idx <- sample(1:n,0.2*n) # 80:20
X_train <- data[-idx,-1]
X_test <- data[idx,-1]
Y_train <- data[-idx,1]
Y_test <- data[idx,1]

B_train = X_train[Y_train == 'B',]
M_train = X_train[Y_train == 'M',]

bc_data <- data

# KDE Based Classifier

library(ks)
library(mvtnorm)

bc_data_pca =  cbind(bc_data[,1], prcomp(bc_data[,-1], scale = TRUE,
                                         center = TRUE, retx = T)$x[,1:6])

n <- dim(data)[1]
accuracy_KDE <- numeric(length = 100)

for(j in 1:100){
train_indices <- sample(1:n,0.8*n)
train_data_pca = bc_data_pca[train_indices, ]
test_data_pca = bc_data_pca[-train_indices, ]

Y_train_pca = train_data_pca[,1]
X_train_pca = train_data_pca[,-1]

Y_test_pca = test_data_pca[,1]
X_test_pca = test_data_pca[,-1]

B_train_pca = as.numeric(X_train_pca[Y_train_pca == 'B',])
M_train_pca = as.numeric(X_train_pca[Y_train_pca == 'M',])

kde_B = matrix(0,nrow = length(Y_test_pca),ncol=6)
kde_M = matrix(0,nrow = length(Y_test_pca),ncol=6)

Y_test_pred <- numeric(length = length(Y_test_pca))
for(i in 1 : length(Y_test_pca)){
  kde_B[i,] = kde(B_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
  kde_M[i,] = kde(M_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
  p <- ifelse(kde_B[i,]>kde_M[i,], 'B', 'M')
  if(sum(p=='B') >sum(p=='M')){Y_test_pred[i] <- 'B'}
  else{Y_test_pred[i] <- 'M'}
}
accuracy_KDE[j] = mean(Y_test_pca == Y_test_pred)}

1-mean(accuracy_KDE) # 0.7168421
sd(accuracy_KDE) # 0.04042131



for(j in 1:100){
  train_indices <- sample(1:n,0.7*n)
  train_data_pca = bc_data_pca[train_indices, ]
  test_data_pca = bc_data_pca[-train_indices, ]
  
  Y_train_pca = train_data_pca[,1]
  X_train_pca = train_data_pca[,-1]
  
  Y_test_pca = test_data_pca[,1]
  X_test_pca = test_data_pca[,-1]
  
  B_train_pca = as.numeric(X_train_pca[Y_train_pca == 'B',])
  M_train_pca = as.numeric(X_train_pca[Y_train_pca == 'M',])
  
  kde_B = matrix(0,nrow = length(Y_test_pca),ncol=6)
  kde_M = matrix(0,nrow = length(Y_test_pca),ncol=6)
  
  Y_test_pred <- numeric(length = length(Y_test_pca))
  for(i in 1 : length(Y_test_pca)){
    kde_B[i,] = kde(B_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
    kde_M[i,] = kde(M_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
    p <- ifelse(kde_B[i,]>kde_M[i,], 'B', 'M')
    if(sum(p=='B') >sum(p=='M')){Y_test_pred[i] <- 'B'}
    else{Y_test_pred[i] <- 'M'}
  }
  accuracy_KDE[j] = mean(Y_test_pca == Y_test_pred)}

1-mean(accuracy_KDE) # 0.7096491
sd(accuracy_KDE) # 0.02828954
