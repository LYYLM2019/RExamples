library(rpart)
library(rpart.plot)
library(rattle)

MYOCARDE = read.table("http://freakonometrics.free.fr/saporta.csv", head=TRUE, sep=";")
cart = rpart(PRONO ~ ., data=MYOCARDE)
# type = 2 -> label all nodes, not just leaves, and draw the split labels below the node labels
# extra = 1 -> display the number of observations that fall in the node
prp(cart, type = 2, extra = 1)

# function to compute the Gini for a split
computeGini = function(outcome, class) {

}