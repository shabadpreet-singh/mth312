# Question 1
library(ddalpha)

# Load the Iris dataset
data(iris)

data1 <- iris[which(iris[,5]=="setosa"),][,-5]
data2 <- iris[which(iris[,5]=="versicolor"),][,-5]
data3 <- iris[which(iris[,5]=="virginica"),][,-5]


# Considering Setosa and Versicolor
x1 <- depth.halfspace(rbind(data1,data2), data1)
y1 <- depth.halfspace(rbind(data1,data2), data2)
plot(x1,y1,cex=1.5,pch=19,col="red",xlab="Setosa",ylab="Versicolor")
abline(0,1,lwd=2)


# Considering Virginica and Versicolor
x2 <- depth.halfspace(rbind(data2,data3), data2)
y2 <- depth.halfspace(rbind(data2,data3), data3)
plot(x2,y2,cex=1.5,pch=19,col="blue",ylab="Virginica",xlab="Versicolor")
abline(0,1,lwd=2)


# Considering Setosa and Virginica
x3 <- depth.halfspace(rbind(data1,data3), data1)
y3 <- depth.halfspace(rbind(data1,data3), data3)
plot(x3,y3,cex=1.5,pch=19,col="green",xlab="Setosa",ylab="Virginica")
abline(0,1,lwd=2)



# Question 2

library(ggplot2)
library(ks)
library(MASS)
library(fastmatrix)
library(rsdepth)

gpa_study_hours <- read.csv("gpa_study_hours_data.csv")
d <- gpa_study_hours
x <- gpa_study_hours$gpa
y <- gpa_study_hours$study_hours

kd <- ks::kde(d, compute.cont = TRUE)

# Define contour levels
cont_levels <- seq(90, 10, by = -10)

# Create a list to store contour data frames
contour_list <- vector("list", length = length(cont_levels))

# Generate contour lines for each level and store in the list
for (i in seq_along(cont_levels)) {
  level <- cont_levels[i]
  contour_list[[i]] <- data.frame(with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                                        z = estimate, levels = cont[paste0(level, "%")])[[1]]),
                                  level = level)
}

# Combine all contour data frames into a single data frame
contour_data <- do.call(rbind, contour_list)

# Plot the data using ggplot
ggplot(data = d, aes(x, y))  + geom_point()+
  geom_path(aes(x, y, group = as.factor(level), col = as.factor(level.1)), data = contour_data, linewidth=1) +
  theme_bw() +
  scale_color_discrete(name = "Contour Level", breaks = cont_levels)+
  theme(legend.position = "none")

#correlation
cor(d[,1],d[,2])

#boxplots for outliers
boxplot(d[,1],col="orange")
boxplot(d[,2],col="purple")

#finding the mode numerically
kde <- kde2d(d$gpa, d$study_hours, n = 193)
mode_index <- which(kde$z == max(kde$z), arr.ind = TRUE)
mode_values <- c(kde$x[mode_index[1, 1]], kde$y[mode_index[1, 2]])
mode_values

#finding the skewness and kurtosis numerically
skewness(d)      
kurtosis(d)

#finding median
rsmed(as.matrix(d))



