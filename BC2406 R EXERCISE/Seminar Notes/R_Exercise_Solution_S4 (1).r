##R Exercise Tasks, S4##
rm(list=ls())
 
## Task 1 ##
admission <-read.csv('admission.csv')
Summary_Data <- function(X, Y) {
mean_Y   <- mean(X[,Y])
median_Y <- median(X[,Y])
sd_Y <- sd(X[,Y])
min_Y <- min(X[,Y])
max_Y <- max(X[,Y])

summary_Y <- data.frame(MEAN = mean_Y, MEDIAN = median_Y, SD = sd_Y, MIN = min_Y, MAX = max_Y)
summary_Y
}

Summary_Data(admission, 1)
Summary_Data(admission, 2)
Summary_Data(admission, 3)
Summary_Data(admission, 4)


## Task 2 ##

#A <- sum(seq(10,100)^3 + 4*seq(10, 100)^2))

j <- 0
for(i in 10:100){ 
   j <- j + (i^3+4*i^2)
}  

print(j)

# B <- sum(seq(1,20)^4) * sum(1/(3+seq(1,5))) 
# Answer 1
m <- 0
for(i in 1:20){
  for(k in 1:5){m <- m + (i^4/(3+k))}
  }
  
print(m)
 
# Answer 2
m <- 0
n <- 0
F1 <- for (i in 1:20) {m <- m + i^4}
F2 <- for (k in 1:5)  {n <- n + 1/(3+k)}

print(m*n)



## Extra Credit Point ##
FN1 <- function(A,B){
P <- A > 0 && B > 0
N <- A < 0 && B < 0

E <- A%%2 == 0 && B%%2 == 0
O <- A%%2 != 0 && B%%2 != 0

A_PO <- A > 0 &&  A%%2 != 0   
B_PO <- B > 0 &&  B%%2 != 0
 
A_NE <- A < 0 &&  A%%2 == 0 
B_NE <- B < 0 &&  B%%2 == 0

if(P && E) {print(A+B)} else if(N && O) {print(A*B)} 
else if(A_PO || B_PO) {print(A/B)} else if(A_NE || B_NE) {print(A-B)}
else {print(A^B)}
  
}

FN1(2,2)
FN1(2,1)
FN1(1,2)
FN1(1,1)
FN1(-2,-2)
FN1(-2,-1)
FN1(-1,-2)
FN1(-1,-1)