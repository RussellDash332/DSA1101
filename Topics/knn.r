# Install packages
install.packages("class")
library("class")

# List the x-factors to be trained, placeholder value below
x1 = c(25,35,45,10,35,52,23,40,90,48,33)
x2 = c(4,6,8,2,12,1.8,9.5,6.2,10,22,15)*10000

# List the x-factors to be tested, placeholder value below
test.x1 = c(48,17,11,22,33,44,55,66,77,88)
test.x2 = c(14.2,0,11,12,13,14,15,16,17,0)*10000

# Combine all the x-factors, no need to modify
train.x = cbind(x1,x2)
test.x = cbind(test.x1,test.x2)

# List the classified values
train.y = c(0,0,0,0,0,0,1,1,1,1,1)
test.y = c(1,0,1,0,1,0,1,0,1,1)

# Visualization of (x1,x2) with classified y
# Constraints : all values of the test x's and y's must be in between the train x's and y's extreme values
# Change xlab and ylab if necessary
plot(x=x1,y=x2,xlab="Age (years)",ylab="Loan (SGD)",main="KNN visualization (R/G = 1, K/B = 0)",col=ifelse(train.y == 1,'red','black'), pch = 19, cex=2)
##points(test.x1,test.x2,col='blue',pch = 3)
points(test.x1,test.x2,col=ifelse(test.y == 0,'blue','green'),pch=17,cex=1.5)

# Apply the knn() function, for now we set k = 5
knn.pred = knn(train.x,test.x,train.y,k = 5)
confusion.matrix = table(knn.pred,test.y)
confusion.matrix

# Some diagnostics here, no need to modify
accuracy <- function(matrix){
	return (sum(diag(matrix))/sum(matrix));
}

precision <- function(matrix){
	return (matrix[2,2]/sum(matrix[2,]));
}

tpr <- function(matrix){
	return (matrix[2,2]/sum(matrix[,2]));
}

fnr <- function(matrix){
	return (matrix[1,2]/sum(matrix[,2]));
}

fpr <- function(matrix){
	return (matrix[2,1]/sum(matrix[,1]));
}

# Summarizes all diagnostics
diagnostics <- function(matrix){
	Diagnostics = c("Accuracy","Precision","TPR","FPR (T1 err)","FNR (T2 err)");
	Rate = c(accuracy(matrix),precision(matrix),tpr(matrix),fpr(matrix),fnr(matrix));
	Percentage = round(Rate*100,2)
	return (data.frame(Diagnostics,Rate,Percentage));
}

# What you need!
diagnostics(confusion.matrix)

#   > confusion.matrix
#           test.y
#   knn.pred 0 1
#          0 1 1
#          1 3 5