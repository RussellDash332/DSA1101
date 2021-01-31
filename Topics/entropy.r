# Function constraints : each node is expected to have 2 splits. Can only count the entropy of a single depth increment.

# Entropy function!
entropy <- function(p){
	return (-(log2(p)*p+log2(1-p)*(1-p)))
}

# c(split1, split2)
# Read : 1942 has attribute split1, 1763 of them are No. The rest 58 has attribute split 2, and 32 of them are Yes.
Yes = c(1942-1763,32)
No = c(1763,58-32)

# No need to modify
FYes = Yes
FNo = No

# Changing values to conditional probabilities, no need to modify
Total = Yes+No
Yes = Yes/Total
No = No/Total
Total = Total/sum(Total)

# Create data, no need to modify
data = as.data.frame(rbind(Total,Yes,No))
purity = as.data.frame(rbind(FYes,FNo))
purity = cbind(purity,rowSums(purity))

# Change header to make it cooler, modify names(data) if needed!
names(data) = c('not success','success')
names(purity) = c(names(data),'FTotal')

# Showcase
data
purity

# Base entropy
fle = entropy(purity['FYes','FTotal']/sum(purity[,'FTotal']))
fle

# Conditional entropy, no need to modify
sle = data['Total',1]*entropy(data['Yes',1])+
	data['Total',2]*entropy(data['Yes',2])
sle

# Entropy reduction / information gain
fle-sle

#	> # Showcase
#	> data
#	      not success   success
#	Total  0.97100000 0.0290000
#	Yes    0.09217302 0.5517241
#	No     0.90782698 0.4482759
#	> purity
#	     not success success FTotal
#	FYes         179      32    211
#	FNo         1763      26   1789