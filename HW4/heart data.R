data2 <- read.csv("heart.csv")
data2 <- na.omit(data2)
head(data2)
n <- dim(data2)[1]

# svm
mn <- numeric(length = 100)
for( i in 1:100){
  idx <- sample(1:n,0.2*n)
  dat_train <- data2[-idx,]
  dat_test <- data2[idx,]
  x_test <- dat_test[,-14]
  y_test <- dat_test[,14]
  
  model <- svm(target ~ ., data = dat_train, kernel = 'linear', scale = T, type = "C-classification")
  y_pred2 <- predict(model, x_test )
  mn[i] <- mean(y_test == y_pred2)}

1-mean(mn) # 0.839561
sd(mn) # 0.0222289

mn <- numeric(length = 100)
for( i in 1:100){
  idx <- sample(1:n,0.3*n)
  dat_train <- data2[-idx,]
  dat_test <- data2[idx,]
  x_test <- dat_test[,-14]
  y_test <- dat_test[,14]
  
  model <- svm(target ~ ., data = dat_train, kernel = 'linear', scale = T, type = "C-classification")
  y_pred2 <- predict(model, x_test )
  mn[i] <- mean(y_test == y_pred2)}

1-mean(mn) # 0.8385668
sd(mn) #  0.0182997

# depth based

X <- data2[1:13]
x <- scale(X)

p <- dim(x)[2]
n <- dim(x)[1]

y <- data2$target
library(ddalpha)

mn <- numeric(length = 100)
for( i in 1:100){
  idx <- sample(1:n,0.2*n)
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
1-mean(mn) # 0.9655122
sd(mn) # 0.01814971


mn <- numeric(length = 100)
for( i in 1:100){
  idx <- sample(1:n,0.3*n)
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
1-mean(mn) # 0.9552769
sd(mn) # 0.01739003

# knn
cross_val <- trainControl(method = "repeatedcv",number=12,repeats=3)
metric <- "Accuracy"

grid <- expand.grid(.k=seq(1,20,by=1))

cf <- numeric(length = 100)
for(i in 1:100){
model.knn <- train(target~., data=dat_train, method="knn", 
                   metric=metric, tuneGrid=grid, trControl=cross_val)
knn.k2 <- model.knn$bestTune 
print(model.knn)
plot(model.knn, main = "Accuracy vs. k for KNN dataset-1")
y_pred.knn_2 <- predict(model.knn, x_test)
cf <- mean(y_pred.knn_2== y_test)}
mean(cf)
sd(cf)


# kernel 

bc_data <- data2
bc_data_pca =  cbind(bc_data[,14], prcomp(bc_data[,-14], scale = TRUE,
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
  
  B_train_pca = as.numeric(X_train_pca[Y_train_pca == 0,])
  M_train_pca = as.numeric(X_train_pca[Y_train_pca == 1,])
  
  kde_B = matrix(0,nrow = length(Y_test_pca),ncol=6)
  kde_M = matrix(0,nrow = length(Y_test_pca),ncol=6)
  
  Y_test_pred <- numeric(length = length(Y_test_pca))
  for(i in 1 : length(Y_test_pca)){
    kde_B[i,] = kde(B_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
    kde_M[i,] = kde(M_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
    p <- ifelse(kde_B[i,]>kde_M[i,], 0, 1)
    if(sum(p==0) >sum(p==1)){Y_test_pred[i] <- 0}
    else{Y_test_pred[i] <- 1}
  }
  accuracy_KDE[j] = mean(Y_test_pca == Y_test_pred)
  }

1-mean(accuracy_KDE) # 0.5367193
sd(accuracy_KDE) # 0.02440681


accuracy_KDE <- numeric(length = 100)

for(j in 1:100){
  train_indices <- sample(1:n,0.7*n)
  train_data_pca = bc_data_pca[train_indices, ]
  test_data_pca = bc_data_pca[-train_indices, ]
  
  Y_train_pca = train_data_pca[,1]
  X_train_pca = train_data_pca[,-1]
  
  Y_test_pca = test_data_pca[,1]
  X_test_pca = test_data_pca[,-1]
  
  B_train_pca = as.numeric(X_train_pca[Y_train_pca == 0,])
  M_train_pca = as.numeric(X_train_pca[Y_train_pca == 1,])
  
  kde_B = matrix(0,nrow = length(Y_test_pca),ncol=6)
  kde_M = matrix(0,nrow = length(Y_test_pca),ncol=6)
  
  Y_test_pred <- numeric(length = length(Y_test_pca))
  for(i in 1 : length(Y_test_pca)){
    kde_B[i,] = kde(B_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
    kde_M[i,] = kde(M_train_pca, eval.points = as.numeric(X_test_pca[i,]))$estimate
    p <- ifelse(kde_B[i,]>kde_M[i,], 0, 1)
    if(sum(p==0) >sum(p==1)){Y_test_pred[i] <- 0}
    else{Y_test_pred[i] <- 1}
  }
  accuracy_KDE[j] = mean(Y_test_pca == Y_test_pred)
}

1-mean(accuracy_KDE) # 0.5451356
sd(accuracy_KDE) # 0.02693783
