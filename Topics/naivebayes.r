## Given the characteristics of the TEST DATA are x1 = long, x2 = sweet and x3 = yellow
## EDIT HERE IF YOU HAVE DIFFERENT CHARACTERISTICS
Characteristics = c("Long","Sweet","Yellow")

# No need to modify
Characteristics = c(Characteristics, "Total")

## Creates the Y classes. The vector inputs are the number of objects with the respective value of Y and having characteristics X.
## The last element of each vector is the number of objects having that Y value.
## EDIT HERE IF YOU HAVE DIFFERENT CLASSES
Banana <- c(100,200,100,400)
Apple <- c(200,400,100,500)
Orange <- c(150,300,150,600)
Other <- c(50,100,100,150)

## Creates a quantity table based on the created classes
tabulation = rbind(Banana,Apple,Orange,Other)

##################################################################

colnames(tabulation) <- Characteristics

## Displays the table of quantity
tabulation

## Time to find the conditional probability, each entry of the table becomes P(X = ... | Y = ...)
## the last column Is.Y is P(Y = ...)
conditional = (tabulation/tabulation[,dim(tabulation)[2]])[,-dim(tabulation)[2]]
Is.Y = tabulation[,dim(tabulation)[2]]
conditional = cbind(conditional,Is.Y/sum(Is.Y))
colnames(conditional) <- c(Characteristics[-length(Characteristics)],"Is Y")

## Displays the table of conditional probability
conditional

## We shall count THE HEURISTIC VALUE of P(Y = Class | X = Characteristics)
## Recall that the denominator is unnecessary to be calculated
probability <- function(table,y){
	prob = 1;
	for (i in 1:dim(table)[2]){
		prob <- prob*table[y,i]
	};
	return (prob);
}

## Now let's get all of the probabilities at once!
naive_bayes <- function(table){
	Class <- rownames(table);
	Probability <- probability(table,Class);
	return (data.frame(Probability));
}

naive_bayes(conditional)
## The class with the highest probability is the class we needed!

#        Probability
# Banana 0.007575758
# Apple  0.019393939
# Orange 0.011363636
# Other  0.013468013