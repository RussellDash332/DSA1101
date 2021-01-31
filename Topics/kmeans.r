# Initial attributes, modify if necessary
x1 = c(1,2,4,5)
x2 = c(1,1,3,4)
x3 = c(3,5,2,9)

# Change data and k if needed
data = cbind(x1,x2,x3)
data
k = 1

# No need to modify as long as you can interpret
kout <- kmeans(data,centers=k)
kout$cluster
kout$centers
kout$withinss
kout$tot.withinss

# Change x1 and x2 below if needed
plot(x1,x2,col=kout$cluster,pch=19,cex=1.5)
points(kout$centers[,'x1'],kout$centers[,'x2'],col=1:k,cex=3,pch=18)

#   > data
#        x1 x2 x3
#   [1,]  1  1  3
#   [2,]  2  1  5
#   [3,]  4  3  2
#   [4,]  5  4  9

#   > kout$centers
#     x1   x2   x3
#   1  3 2.25 4.75