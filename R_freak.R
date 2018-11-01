# 1_Rotational multiplication

a <- c(2,3,4) 
b <- c(1,2)

# 2_Scoping Rules

#-----start now-----#
y <- 3
f <- function(x) {
  y <- 2
  y ^ 2 + g(x)
                 }
g <- function(x) {
  x * y
}
#-----end-----#
# think about all this shit
# check result
f(5)

# Priority in loop

# ":" - higher priority(-1 for every member of vector)
for (i in 1:variable -1) 
 
# right version  
for (i in 1 : (variable - 1)) 
  




