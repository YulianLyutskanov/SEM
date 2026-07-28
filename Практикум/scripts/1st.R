(5+7)/(4-1)
9^2
sqrt(25)
log(2)
28 %% 10
27 / 100000

5000*5000

options(scipen=999)
27/10000

options(scipen=0)
27/100000

x <- c(2,4,5,7,11)
x[2:5]
x[c(1,3,5)]

x > 4
x[x<=5]

length(x)
max(x)
min(x)
sum(x)
sort(x)

sum(x>10)
which(x >10 | x < 5)

x <- c(5, 5, 5, 7, 7, 7)
y <- c(2, 2,3 ,5)
x + y

class(c(1,2,3))          # "numeric"
class(c(TRUE, FALSE))   # "logical"
class(c("a","b"))       # "character"


x <- c(5, 12, 11, 14, 2, 3, 14, 10, 3)

x[3]
x[1:5]
x[c(2,5,9)]
x[-4]
x[-c(2,3)]
x[x>10]
head(x,3)
tail(x,3)
x < 10 & x > 3
x
diff(x)
length(diff(x))
length(x)
x^2
sort(x)
x[ order(x)]
order(x)
rank(x)
x <- vector("character", length =5)
rep(5, times = 3)
rep(c(1,2) , times = 8)
rep(c(1,2) , each = 2)
rep(c(1,2), length.out=7)
3:11
10:-2
seq(from = 1, to = 10, by = 1)
seq( from = 4, to = 10, by = 2)
seq(from = 0, to  = 1, length.out = 11)
M <- rbind(c(1,2,3),c(3,2,1))
M
M[2,3]
M[1,]
m <- cbind( c(1,2), c(3,4), c(4,5))
m
t(m)
x <- c(5, 8, 11, 3, 2, 9, 4)
y <- c("Y", "Y", "N", "Y", "N", "N", "Y")
df <- data.frame(x, y)
df
df$y
