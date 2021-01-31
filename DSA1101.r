# General tip : try to load all the packages and functions then try each of them one by one before the real usage.

# Install all the packages first
install.packages('class')
install.packages('e1071')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('ggplot2')
install.packages('magrittr')
install.packages('arules')
install.packages('arulesViz')
install.packages('ROCR')
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(magrittr)
library(arules)
library(arulesViz)
library(ROCR)

#######################################################################
##################
#    Functions   #
##################

# Gets the basic statistical values of a vector x
getstats <- function(x){
	stat = c(mean(x),median(x),var(x),sd(x),max(x),min(x),length(x))
	name = c('Mean','Median','Var','SD','Max','Min','Length')
	return (data.frame(name,stat))
}

# Gets the basic statistical values of two vectors x and y
getstats2 <- function(x,y){
	stat = c(cor(x,y),cov(x,y))
	name = c('Cor','Cov')
	return (data.frame(name,stat))
}

# Linear regression
linreg <- function(x,y){
	beta_1 <- (sum(x*y)-mean(y)*sum(x))/(sum(x^2)-mean(x)*sum(x));
	beta_0 <- mean(y)-beta_1*mean(x);
	return(c(beta_0,beta_1));
}

# Entropy of a purity
entropy <- function(p){
	if (p==0 | p==1){
		return (0)
	} else {
		return (-(log2(p)*p+log2(1-p)*(1-p)))
	}
}

# Gini index of a purity
gini <- function(p){
	return (2*p*(1-p))
}

# Logistic function
logistic <- function(z){
	exp(z)/(1+exp(z))
}

# Likelihood function given probability, number of 1's and 0's
likelihood <- function(p,n1,n0){
	p^n1*(1-p)^(n0)
}

# Accuracy of a confusion matrix
accuracy <- function(matrix){
	return (sum(diag(matrix))/sum(matrix));
}

# Precision of a confusion matrix
precision <- function(matrix){
	return (matrix[2,2]/sum(matrix[2,]));
}

# TPR of a confusion matrix
tpr <- function(matrix){
	return (matrix[2,2]/sum(matrix[,2]));
}

# FNR of a confusion matrix
fnr <- function(matrix){
	return (matrix[1,2]/sum(matrix[,2]));
}

# FPR of a confusion matrix
fpr <- function(matrix){
	return (matrix[2,1]/sum(matrix[,1]));
}

# The whole diagnostics of a confusion matrix
diagnostics <- function(matrix){
	Diagnostics = c("Accuracy","Precision","TPR","FPR (T1 err)","FNR (T2 err)");
	Rate = c(accuracy(matrix),precision(matrix),tpr(matrix),fpr(matrix),fnr(matrix));
	Percentage = round(Rate*100,2)
	return (data.frame(Diagnostics,Rate,Percentage));
}

# The whole diagnostics given TP,FN,FP,TN in that order
diagnosticsmanual <- function(TP,FN,FP,TN){
	Diagnostics = c("Accuracy","Precision","TPR","FPR (T1 err)","FNR (T2 err)");
	Rate = c((TP+TN)/(TP+FN+FP+TN),TP/(TP+FP),TP/(TP+FN),FP/(FP+TN),FN/(TP+FN));
	Percentage = round(Rate*100,2)
	return (data.frame(Diagnostics,Rate,Percentage));
}

# Sum of Euclidean distances of a point to a list of points
euclidean <- function(centroid, p_list){
	ss = 0
	for (i in 1:length(centroid)){
		ss = ss + sum((centroid[i]-p_list[,i])^2)
	}
	return (ss)
}

# Does the whole thing related to KNN
displayknn <- function(train.x,test.x,train.y,test.y,K){
	knn.pred = knn(train.x,test.x,train.y,k=K)
	confusion.matrix=table(knn.pred, test.y)
	predicted = as.factor(c(0,0,1,1))
	actual = as.factor(c(0,1,0,1))
	cm = table(predicted,actual)
	cm[1,1]='TN'
	cm[1,2]='FN'
	cm[2,1]='FP'
	cm[2,2]='TP'
	print(cm)
	print(confusion.matrix)
	print(diagnostics(confusion.matrix))
}

# For Naive Bayes function
probability <- function(table,y){
	prob = 1;
	for (i in 1:dim(table)[2]){
		prob <- prob*table[y,i]
	};
	return (prob);
}

# Does the whole thing related to Naive Bayes
naivebayes <- function(table){
	Class <- rownames(table);
	Probability <- probability(table,Class);
	return (data.frame(Probability));
}

# Does the whole thing related to Naive Bayes V2
nb <- function(tabulation){
	colnames(tabulation) <- Characteristics
	conditional = (tabulation/tabulation[,dim(tabulation)[2]])[,-dim(tabulation)[2]]
	Is.Y = tabulation[,dim(tabulation)[2]]
	conditional = cbind(conditional,Is.Y/sum(Is.Y))
	colnames(conditional) <- c(Characteristics[-length(Characteristics)],"Is Y")
	Probability <- naivebayes(conditional)[,1]
	LogProb <- log(Probability)
	return (cbind(conditional,Probability,LogProb))
}

# Does the whole thing related to Decision Tree
dt <- function(Yes,No){
	FYes = Yes
	FNo = No
	Total = Yes+No
	Yes = Yes/Total
	No = No/Total
	Total = Total/sum(Total)
	data = as.data.frame(rbind(Total,Yes,No))
	purity = as.data.frame(rbind(FYes,FNo))
	purity = cbind(purity,rowSums(purity))

	names(data) = c('split1','split2')
	names(purity) = c(names(data),'Total')
	rownames(purity) = c('Yes','No')
	
	#print('Data')
	#print(data)
	#print('')
	print('Contigency Table')
	print(purity)
	print('')
	fle = entropy(purity['Yes','Total']/sum(purity[,'Total']))
	print('Base Entropy')
	print(fle)
	print('')
	sle = data['Total',1]*entropy(data['Yes',1])+
		data['Total',2]*entropy(data['Yes',2])
	print('Conditional Entropy')
	print(sle)
	print('')
	print('Entropy Reduction')
	print(fle-sle)
}

# Does the whole thing related to Decision Tree but with Gini index
dtgini <- function(Yes,No){
	FYes = Yes
	FNo = No
	Total = Yes+No
	Yes = Yes/Total
	No = No/Total
	Total = Total/sum(Total)
	data = as.data.frame(rbind(Total,Yes,No))
	purity = as.data.frame(rbind(FYes,FNo))
	purity = cbind(purity,rowSums(purity))

	names(data) = c('split1','split2')
	names(purity) = c(names(data),'Total')
	rownames(purity) = c('Yes','No')
	
	#print('Data')
	#print(data)
	#print('')
	print('Contigency Table')
	print(purity)
	print('')
	fle = gini(purity['Yes','Total']/sum(purity[,'Total']))
	print('Base Gini Index')
	print(fle)
	print('')
	sle = data['Total',1]*gini(data['Yes',1])+
		data['Total',2]*gini(data['Yes',2])
	print('Conditional Gini Index')
	print(sle)
	print('')
	print('Gini Index Reduction')
	print(fle-sle)
}

# Does the whole thing related to k-means clustering
kmc <- function(data,k){
	kout <- kmeans(data,centers=k)
	print('Clustering')
	print(kout$cluster)
	print('Centroids')
	print(kout$centers)
	print('WSS assuming k = 1')
	print(kout$totss)
	print('WSS of each cluster')
	print(kout$withinss)
	print('The real WSS')
	print(kout$tot.withinss)
}

# Does the whole thing related to Apriori Algorithm
checkfrequentitemsets <- function(dataset,ml,mxl,minsupport,topk){
	itemsets <- apriori(dataset, parameter=list(minlen=ml, maxlen=mxl,support=minsupport, target="frequent itemsets"))
	print(inspect(head(sort(itemsets, by = "support"), topk)))
	print('')
	print('')
	print('')
	print('')
	print('')
	print('')
	print('Summary')
	print(summary(itemsets))
}

#######################################################################

##################
#   Primitives   # 
##################
x = 1:4
y = c(5,1,2,1)
getstats(x)
getstats2(x,y)
linreg(x,y)

lr = lm(y~x)
lr$coefficients
lr$fitted.values
lr$residuals 

###########
#   KNN   # 
###########
x1 = c(25,35,45,10,35,52,23,40,90,48,33)
x2 = c(4,6,8,2,12,1.8,9.5,6.2,10,22,15)*10000
test.x1 = c(48,17,11,22,33,44,55,66,77,88)
test.x2 = c(14.2,0,11,12,13,14,15,16,17,0)*10000
train.y = c(0,0,0,0,0,0,1,1,1,1,1)
test.y = c(1,0,1,0,1,0,1,0,1,1)

train.x = cbind(x1,x2)
test.x = cbind(test.x1,test.x2)

set.seed(1)
displayknn(train.x,test.x,train.y,test.y,2)

#           Predicted y
#             1   0
#           1 TP FN      -> input = (TP, FN, FP, TN)
# Actual y  0 FP TN    
diagnosticsmanual(5,1,3,1)

train.y = c(1,1,0,1,0)
x1 = c(1,2,1,3,3)
x2 = c(3,2,1,3,1)
test.x1 = 1
test.x2 = 1
train.x = cbind(x1,x2)
test.x = cbind(test.x1,test.x2)
knn.pred = knn(train.x,test.x,train.y,k=1)
knn.pred

#################
#  Naive Bayes  # 
#################
Characteristics = c("Long","Sweet","Not Yellow","Total")
Banana <- c(200,100,300,500)
#Apple <- c(1,1,1,1)
Orange <- c(20,100,120,300)
Other <- c(100,50,150,200)
tabulation = rbind(Banana,Orange,Other)
tabulation
nb(tabulation)

###################
#  Decision Tree  # 
###################
#fit <- rpart(subscribed ~job + marital + education + default + housing + loan + contact + poutcome,method="class", data=bankdata, control=rpart.control(minsplit=1),parms=list(split='information'))
#rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)
#dev.off()

# X = c( X | split 1, X | split 2)
# student yes/no 0.218376
Yes = c(4,0)
No = c(4,3)

# income low/high 0.02368
Yes = c(1,3)
No = c(3,4)

# married yes/no 0.072
Yes = c(1,3)
No = c(4,3)

dt(Yes,No)
#dtgini(Yes,No)

#############
#  K-means  # 
#############
x1 = c(1,1.5,3,3.5,4.5)
x2 = c(1,2,4,5,5)
z = c(1,2,3,4,6)
M = cbind(x1,x2,z)
M
class(M)
apply(M,1,sum)
apply(M,2,sum)
plot(x1,x2,cex=3)
points(c(1.5,3),c(1.5,4),pch=19,col='red')
data = cbind(x1,x2)

kmc(data,2)

x1 = c(1,5)
x2 = c(5,1)
data = data.frame(x1,x2)
euclidean(c(mean(x1),mean(x2)),data)
x1 = c(3,7)
x2 = c(7,3)
data = data.frame(x1,x2)
euclidean(c(mean(x1),mean(x2)),data)

#############
#  Apriori  #  GROCERIES DATA RESTRICTED!
#############
data(Groceries)

	Groceries@itemInfo[11:20,]
	Groceries@data[1:10,1:2]

checkfrequentitemsets(Adult,1,1,0.02,10)

	rules <- apriori(Groceries, parameter=list(support=0.001,
	confidence=0.6, target = "rules"))
	inspect(head(sort(rules, by="lift"), 3))

	#plot(rules)
	#highLiftRules <- head(sort(rules, by="lift"), 10)
	#plot(highLiftRules, method="graph", control=list(alpha=1))

#########################
#  Logistic Regression  # 
#########################
# Functions to use : logistic(z) and likelihood(p,n1,n0)

# Attributes
x <- c(1,0,1,0,1,0,1)
#x2 <- rnorm(7)
y <- c(1,0,1,1,1,0,0)

logistic.fit <- glm(y ~ x, family = binomial(link="logit"))
coef <- logistic.fit$coefficients
coef
summary(logistic.fit)