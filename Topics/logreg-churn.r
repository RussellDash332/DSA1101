churn <- read.csv("Datasets/churn.csv")
head(churn)
tail(churn)
dim(churn)

logreg <- function(z){
	exp(z)/(1+exp(z))
}

x = churn$Age
y = churn$Churned

churn_logit <- glm(y ~ x, family = binomial(link = 'logit'))
summary(churn_logit)

coef <- churn_logit$coefficients
coef

xs = sort(x)
xs = seq(0,max(xs),0.01)
ylog = logreg(coef[1]+coef[2]*xs)

plot(c(x,0),c(y,0),pch=19,cex=0.5,col=ifelse(y==1,"red","black"),xlab='x',ylab='y',main='LogReg Plot')
points(xs,ylog,pch=19,type='l',col='blue')

##    > head(churn)
##      ID Churned Age Married Cust_years Churned_contacts
##    1  1       0  61       1          3                1
##    2  2       0  50       1          3                2
##    3  3       0  47       1          2                0
##    4  4       0  50       1          3                3
##    5  5       0  29       1          1                3
##    6  6       0  43       1          4                3
##    > tail(churn)
##           ID Churned Age Married Cust_years Churned_contacts
##    7995 7995       1  42       0          3                4
##    7996 7996       1  40       1          3                5
##    7997 7997       0  25       1          3                5
##    7998 7998       0  60       1          3                6
##    7999 7999       1  38       1          7                6
##    8000 8000       0  36       1          3                5
##    > dim(churn)
##    [1] 8000    6