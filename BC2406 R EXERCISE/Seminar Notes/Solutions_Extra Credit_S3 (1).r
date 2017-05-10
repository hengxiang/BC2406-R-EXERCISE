## Extra Credit 1 ##
M <- sum(seq(1,20)^4) * sum(1/(3+seq(1,5))) 

#or, M <- sum((1:20)^4) * sum( 1/(4:8))

## Extra Credit 2 ##
FN3 <- function(x, n) {sum((x^seq(1,n)/seq(1,n)))}
FN3(1,3) 
