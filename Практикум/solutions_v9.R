###
# zad1

sim.balls1 <- function() {
  x <- sample( c(1:8), 2, replace=T )
  x[1]==x[2]
}

res <- replicate( 100000, sim.balls1() )
sum(res)/length(res)


###
# zad2

sim.socks <- function() {
  socks <- c(1,1,2,2,3,3)
  x <- sample( socks, 2, replace=F )
  x[1]==x[2]
}

prob.socks <- function(Nrep) {
  res <- replicate( Nrep, sim.socks() )
  sum(res)/length(res) 
}

prob.socks(100000)


###
# zad3

sim.keys <- function() {
  keys <- c(1:4)
  x <- sample( keys, 4, replace=F )
  x[4]==1  ## означаваме верният ключ с номер 1
}

prob.keys <- function(Nrep) {
  res <- replicate( Nrep, sim.keys() )
  sum(res)/length(res) 
}

prob.keys(100000)


###
# zad4

sim.exam <- function() {
  questions <- c( rep(0,3), rep(1,17) )
  x <- sample( questions, 2, replace=F )
  sum(x)==1
}

prob.exam <- function(Nrep) {
  res <- replicate( Nrep, sim.exam() )
  sum(res)/length(res)
}

prob.exam(100000)


###
# zad5

sim.bday <- function(k) {
  days <- c(1:365)
  x <- sample( days, k, replace=T )
  any( duplicated(x) )
}

prob.bday <- function(Nrep, k) {
  res <- replicate( Nrep, sim.bday(k) )
  sum(res)/length(res)
}

prob.bday(100000, 25)


###
# zad6

sim.gifts <- function(k) {
  x <- sample( c(1:k), k, replace=F )
  d <- x - c(1:k)
  any(d==0)
}

prob.gifts <- function(Nrep, k) {
  res <- replicate( Nrep, sim.gifts(k) )
  sum(res)/length(res)
}

prob.gifts(100000, 20)


###
# zad7

sim.ants <- function() {
  a1 <- sample( c(2,3), 1 )
  a2 <- sample( c(1,3), 1 )
  a3 <- sample( c(1,2), 1 ) 
  a <- c(a1,a2,a3)
  length( unique(a) ) == 3
  # any( duplicated(a) ) == F
  # all( c(1,2,3) %in% a )
}

prob.ants <- function(Nrep) {
  res <- replicate( Nrep, sim.ants() )
  sum(res)/length(res)
}

prob.ants(100000)


###
# zad8

sim.eggs <- function() {
  eggs <- c( rep("b", 2), rep("r", 6) )
  draws <- sample( eggs, 8, replace=F )
  player1 <- draws[seq(1,7,2)]
  player2 <- draws[seq(2,8,2)]
  b1 <- sum(player1=="b")
  b2 <- sum(player2=="b")
  c(b1, b2)
}

Nrep <- 100000
res <- replicate( Nrep, sim.eggs() )
res[,1:10]

# P(A)
( sum(res[1,]==2) + sum(res[2,]==2) ) / Nrep

# P(B)
sum(res[1,]==1) / Nrep
sum(res[2,]==1) / Nrep

# P(C)
sum(res[1,]==2) / Nrep

# P(D)
sum(res[2,]==2) / Nrep


###
# zad9

sim.test.ans <- function() {
  x <- sample( c(0,1), 10, replace=T, prob=c(0.75, 0.25) )
  sum(x)
}

res <- replicate( 100000, sim.test.ans() )
sum(res >= 5)/length(res)


###
# zad10

sim.airplane <- function() {
  x <- sample( c(0,1), 143, replace=T, prob=c(0.08, 0.92) )
  sum(x) 
}

res <- replicate( 100000, sim.airplane() )
# а)
sum(res <= 138)/length(res)
# б)
sum(res == 137)/length(res)


###
# zad11

# а)
sim.dice.ball.a <- function() {
  dice <- sample( c(1:6), 1 )
  if (dice==6) {
    ball <- sample( c("g", "g", "r", "r"), 1 )
  } else {
    ball <- sample( c("g", "r", "r", "r", "r"), 1 )
  }
  ball=="g"
}

Nrep <- 100000
res <- replicate( Nrep, sim.dice.ball.a() )
sum(res)/Nrep

# б)
sim.dice.ball.b <- function() {
  dice <- sample( c(1:6), 1 )
  if (dice==6) {
    ball <- sample( c("g", "g", "r", "r"), 1 )
  } else {
    ball <- sample( c("g", "r", "r", "r", "r"), 1 )
  }
  c(dice, ball)
}

res <- replicate( Nrep, sim.dice.ball.b() )
# б)
sum(res[1,]!="6" & res[2,]=="g") / sum(res[2,]=="g")
# а)
sum(res[2,]=="g") / Nrep


###
# zad12

# а)
sim.coins.a <- function() {
  coin <- sample( c(11, 11, 22, 12, 12), 1 )
  if (coin==12) {
    up <- sample( c(1,2), 1 )
  } else {
    if (coin==22) {
      up <- 2
    } else {
      up <- 1
    }
  }
  up==1
}

Nrep <- 100000
res <- replicate( Nrep, sim.coins.a() )
sum(res)/Nrep

# б)
sim.coins.b <- function() {
  coin <- sample( c("11", "11", "22", "12", "12"), 1 )
  side <- sample( c(1,2), 1 )
  up <- substr( coin, start=side, stop=side )
  c(up, coin)
}

res <- replicate( Nrep, sim.coins.b() )
# б)
sum(res[1,]=="1" & res[2,]=="12") / sum(res[1,]=="1")
# а)
sum(res[1,]=="1") / Nrep


###
###
# zad18

# X = брой шестици при 10 хвърляния на зар
# X ~ Bi(n=10, p=1/6)

# P(X = 2)
dbinom(2, 10, 1/6)
choose(10,2)*(1/6)^2*(5/6)^8

# P(X <= 2)
pbinom(2, 10, 1/6)
dbinom(0, 10, 1/6) + dbinom(1, 10, 1/6) + dbinom(2, 10, 1/6)

# P(X >= 2) = 1 - P(X <= 1)
1 - pbinom(1, 10, 1/6)

# P(3 <= X <= 8) = P(X <= 8) - P(X <= 2) 
pbinom(8, 10, 1/6) - pbinom(2, 10, 1/6)
sum( dbinom(3:8, 10, 1/6) )


###
# zad19

# X = брой хвърляния до падане на шестица (вкл.)
# X ~ Ge(p=1/6)

# P(X <= 10)
pgeom(10-1, 1/6)

# P(X >= 6) = 1 - P(X <= 5)
1 - pgeom(5-1, 1/6)


###
# zad20

# X = брой хвърляния докато се паднат общо 3 шестици
# X ~ NB(r=3, p=1/6)

# P(X <= 20)
pnbinom(20-3, 3, 1/6)


###
# zad21

# X = брой нови батерии от избраните
# От 2 избрани батерии, новите може да са 0, 1 или 2.

# Фенерчето ще заработи ако изберем 2 нови батерии, 
# няма да заработи ако изберем по-малко от 2 нови.
# P(избираме по-малко от 2 нови)  =  P(X = 0) + P(X = 1) 
#  =  1 - P(X = 2)

# P(X = 0) + P(X = 1)
dhyper(0, 5, 3, 2) + dhyper(1, 5, 3, 2)

# 1 - P(X = 2)
1 - dhyper(2, 5, 3, 2)
1 - choose(5,2)*choose(3,0)/choose(8,2)


###
# zad22

# X = брой грешки на 1500 думи
# X ~ Bi(n=1500, p=1/500)

n <- 1500
p <- 1/500

# P(X <= 2)
pbinom(2, n, p)
ppois(2, n*p)

# P(1 <= X <= 3) = P(X=1) + P(X=2) + P(X=3)
sum( dbinom(1:3, n, p) )

# P(1 <= X <= 3) = P(X <= 3) - P(X <= 0)
pbinom(3, n, p) - pbinom(0, n, p)
ppois(3, n*p) - ppois(0, n*p)

# Тук може да използваме Поасоновото разпределение като приближение на биномното.
# Виждаме, че приближението е добро (получаваме близки вероятности).


###
# zad23

# X = брой верни отговори, дадени от Иван на теста
# X ~ Bi(n=10, p=1/4)

# P(X >= 5) = 1 - P(X <= 4)
1 - pbinom(4, 10, 1/4)


###
# zad24 

# X = брой пътници, които идват навреме
# X ~ Bi(n=143, p=0.92)

# а)
# P(X <= 138)
pbinom(138, 143, 0.92)

# б)
# P(X = 137)
dbinom(137, 143, 0.92)


###
# zad25

# а)
# X = брой проверени батерии до откриване на първата дефектна (вкл.)
# X ~ Ge(p=0.03)

# P(X <= 10)
pgeom(10-1, 0.03)

# б)
# X = брой дефектни батерии измежду 50 проверени
# X ~ Bi(n=50, p=0.03)

# P(X >= 2) = 1 - P(X <= 1)
1 - pbinom(1, 50, 0.03)


###
# zad26

# X = брой дефектни батерии измежду 50 проверени от общо 100

# P(X >= 2) = 1 - P(X <= 1)
1 - phyper(1, 3, 97, 50)


###
###
# zad32

x1 <- runif(5000, 3, 5)
hist(x1)
hist(x1, probability=T)
curve( dunif(x, 3, 5), from=3, to=5, add=T, lwd=2 )


###
# zad33

x1 <- rexp(500, 1/7)
hist(x1, probability=T)
curve( dexp(x, 1/7), from=0, to=max(x1), add=T, lwd=2 )


###
# zad34

x1 <- rnorm(5000, 0, 1)
hist(x1, probability=T)
curve( dnorm(x, 0, 1), add=T, lwd=2 )


###
# zad35

n <- 200
# n <- 1000

x <- runif(n, 7, 9)
plot.ecdf(x, do.points=F)
curve( punif(x, 7, 9), add=T, col="red" ) 


###
# zad36

x <- rexp(n, 3)
plot.ecdf(x, do.points=F)
curve( pexp(x, 3), add=T, col="red" ) 


###
# zad37

x <- rnorm(n, 4, 1.2)
plot.ecdf(x, do.points=F)
curve( pnorm(x, 4, 1.2), add=T, col="red" ) 


###
# zad38

par(mfrow=c(1,3))
curve( dunif(x, 7, 9), from=7, to=9 ) 
curve( punif(x, 7, 9), from=5, to=11 ) 
curve( qunif(x, 7, 9), from=0, to=1 ) 
par(mfrow=c(1,3))


###
# zad39

curve( dexp(x, 3), from=0, to=4 ) 
curve( pexp(x, 3), from=-1, to=4 ) 
curve( qexp(x, 3), from=0, to=1 ) 


###
# zad40

curve( dnorm(x, 4, 1.2), from=0, to=8 )
curve( pnorm(x, 4, 1.2), from=0, to=8 )
curve( qnorm(x, 4, 1.2), from=0, to=1 )


###
# zad41

# X = количество портокалов сок в случайно избрана бутилка
# X ~ U(495, 502)

# а)
# P(X > 500) = 1 - P(X <= 500)
1 - punif(500, 495, 502)

# б)
# v=?  P(X > v) = 0.8
# P(X <= v) = 0.2
qunif(0.2, 495, 502)


###
# zad42

# X = време до повреда на пералня
# X ~ Exp(1/4)

# а)
# P(X > 3) = 1 - P(X <= 3)
1 - pexp(3, 1/4)

# б)
# P(X <= 2) 
pexp(2, 1/4)

# в)
# P(X > 6 | X > 3) = P(X > 6)/P(X > 3)
(1-pexp(6, 1/4)) / (1-pexp(3, 1/4))

# г)
# t=?  P(X <= t) = 0.9
qexp(0.9, 1/4)


###
# zad43

# X = изразходвано количество кашкавал за седмица
# X ~ N(mu=41, sigma=5)

# а)
# P(X>51) = 1 - P(X<=51)
1 - pnorm(51, 41, 5)

# б)
# P(45 < X < 50) = P(X < 50) - P(X < 45)
pnorm(50, 41, 5) - pnorm(45, 41, 5)

# в)
# t=?  P(X <= t) = 0.99
qnorm(0.99, 41, 5)


###
# zad44

n <- 10^7
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
4*sum(x^2 + y^2 < 1)/n
pi

# картинка
n <- 10000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
plot(x, y, pch=".", col="gray")
curve( sqrt(1-x^2), from=-1, to=1, add=T, col="red" )
curve( -sqrt(1-x^2), from=-1, to=1, add=T, col="red" )


###
# zad45

f <- function(x) {
  exp(-x^2/2)*(1/sqrt(2*pi))
}

# метод 1
a <- 0.8
b <- 4
n <- 10^7
x <- runif(n, a, b)
y <- runif(n, 0, f(a))
(sum(y < f(x))/n)*(b-a)*f(a)

# Интегралът е равен на вероятността P(0.8 < X < 4), X ~ N(0,1).
# Вероятността пресметната с функцията pnorm:
pnorm(4) - pnorm(0.8)

# картинка (метод 1)
n <- 10000
x <- runif(n, a, b)
y <- runif(n, 0, f(a))
plot(x, y, pch=".", col="gray")
curve( f(x), from=a, to=b, add=T, col="red" )

# метод 2
a <- 0.8
b <- 4
n <- 10^7
x <- runif(n, a, b)
sum(f(x))*(b-a)/n


###
###
# zad49

library(MASS)
data(survey)
?survey

table(survey$Exer)
attach(survey)
table(Exer)

sort( table(Exer), decreasing=T )
100*table(Exer)/length(Exer)
100*prop.table( table(Exer) )

barplot( table(Exer) )
barplot( sort( table(Exer), decreasing=T ) )
barplot( 100*table(Exer)/length(Exer) )
pie( table(Exer) )
pie( table(Exer), col=c("red", "yellow", "blue") )


###
# zad50

table(Pulse)
table(Pulse, useNA="ifany")

pulse.interval <- cut(Pulse, breaks=seq(30,110,10))
pulse.interval
table(pulse.interval)

barplot( table(pulse.interval) )
hist(Pulse)
hist(Pulse, breaks=seq(30,110,5))

stripchart(Pulse, method="stack", pch=20)
stripchart(Pulse, method="stack", pch=18)
stripchart(Pulse, method="stack", pch=1)
stripchart(Pulse, method="stack", pch="*")


###
# zad51

table(Age)

age.interval <- cut(Age, breaks=seq(15,75,10))
table(age.interval)

barplot( table(age.interval) )
hist(Age)

stripchart(Age, method="stack", pch="*")


###
my.summary <- function(x) {
  res <- c( median(x, na.rm=T), mean(x, na.rm=T), sd(x, na.rm=T) )
  names(res) <- c("Median", "Mean", "StDev")
  res
}


###
# zad52

v1 <- rep(4, 30)
v2 <- rep(c(3.5,4.5), 15)
v3 <- rep(c(3,5), 15)
v4 <- rep(c(2:6), 6)
v5 <- rep(c(2,6), 15)

my.summary(v1)
my.summary(v2)
my.summary(v3)
my.summary(v4)
my.summary(v5)

par(mfrow=c(2,3))
stripchart(v1, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v2, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v3, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v4, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v5, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
par(mfrow=c(1,1))


###
# zad53

load("cereals.RData")

summary(cereals)
attach(cereals)

summary(carbo)
mean(carbo, na.rm=T)
sd(carbo, na.rm=T)
my.summary(carbo)
hist(carbo)
boxplot(carbo, horizontal=T)

summary(sodium)
mean(sodium, na.rm=T)
sd(sodium, na.rm=T)
my.summary(sodium)
hist(sodium)
boxplot(sodium, horizontal=T)

summary(potass)
mean(potass, na.rm=T)
sd(potass, na.rm=T)
my.summary(potass)
hist(potass)
boxplot(potass, horizontal=T)


###
# zad54

boxplot(Pulse ~ W.Hnd)
boxplot( Pulse[W.Hnd=="Left"], Pulse[W.Hnd=="Right"] )

my.summary( Pulse[W.Hnd=="Left"] )
my.summary( Pulse[W.Hnd=="Right"] )


###
# zad55

# а)
median( Pulse, na.rm=T )
mean( Pulse, na.rm=T )
sd( Pulse, na.rm=T )
my.summary( Pulse )

# б)
median( Pulse[Sex=="Female"], na.rm=T )
mean( Pulse[Sex=="Female"], na.rm=T )
sd( Pulse[Sex=="Female"], na.rm=T )
my.summary( Pulse[Sex=="Female"] )

# в)
median( Pulse[Age<=25], na.rm=T )
mean( Pulse[Age<=25], na.rm=T )
sd( Pulse[Age<=25], na.rm=T )
my.summary( Pulse[Age<=25] )

# г)
median( Pulse[Exer=="Freq"], na.rm=T )
mean( Pulse[Exer=="Freq"], na.rm=T )
sd( Pulse[Exer=="Freq"], na.rm=T )
my.summary( Pulse[Exer=="Freq"] )

# д)
median( Pulse[Smoke=="Never" & Exer=="Freq"], na.rm=T )
mean( Pulse[Smoke=="Never" & Exer=="Freq"], na.rm=T )
sd( Pulse[Smoke=="Never" & Exer=="Freq"], na.rm=T )
my.summary( Pulse[Smoke=="Never" & Exer=="Freq"] )


###
###
# zad60

Nrep <- 1000
# Nrep <- 10000

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rexp(n, 1/5) ) )
  hist(xsum, main=paste("n =", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rexp(n, 1/5) ) )
  plot.ecdf( xsum, do.points=FALSE, col="blue", lwd=2, main=paste("n =", n) )
  curve( pnorm(x, 5*n, 5*sqrt(n)), add=T, col="coral", lty="longdash", lwd=2 )
}


###
# zad61

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rexp(n, 1/5) ) )
  hist(xmean, main=paste("n =", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rexp(n, 1/5) ) )
  plot.ecdf( xmean, do.points=FALSE, col="blue", lwd=2, main=paste("n =", n) )
  curve( pnorm(x, 5, 5/sqrt(n)), add=T, col="coral", lty="longdash", lwd=2 )
}


###
# zad62

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rpois(n, 3) ) )
  hist(xsum, main=paste("n =", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate( Nrep, sum( rpois(n, 3) ) )
  plot.ecdf( xsum, do.points=FALSE, col="blue", lwd=2, main=paste("n =", n) )
  curve( pnorm(x, 3*n, sqrt(3*n)), add=T, col="coral", lty="longdash", lwd=2 )
}


###
# zad63

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rpois(n, 3) ) )
  hist(xmean, main=paste("n =", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate( Nrep, mean( rpois(n, 3) ) )
  plot.ecdf( xmean, do.points=FALSE, col="blue", lwd=2, main=paste("n =", n) )
  curve( pnorm(x, 3, sqrt(3)/sqrt(n)), add=T, col="coral", lty="longdash", lwd=2 )
}


###
# zad65

# с използване на ЦГТ (централната гранична теорема)
a <- (980-900)/(900/sqrt(100))
1 - pnorm(a)

# със симулации
mean.vals <- replicate(100000, mean(rexp(100, 1/900)))
sum(mean.vals > 980)/length(mean.vals)
hist(mean.vals)


###
# zad66

# с използване на ЦГТ
b <- (35-30)/(60/(sqrt(12)*sqrt(50)))
a <- (25-30)/(60/(sqrt(12)*sqrt(50)))
pnorm(b) - pnorm(a)

# със симулации
mean.vals <- replicate(100000, mean(runif(50, 0, 60)))
sum(mean.vals > 25 & mean.vals < 35)/length(mean.vals)
hist(mean.vals)


###
# zad67 

# пресмятаме mu, sigma
x <- c(4:7)
p <- c(0.2,0.4,0.3,0.1)
mu <- sum(x*p)
sigma <- sqrt( sum((x^2)*p) - mu^2 )

# с използване на ЦГТ
a <- (5.5-mu)/(sigma/sqrt(49))
1 - pnorm(a)

# със симулации
mean.vals <- replicate(100000, mean(sample(x, 49, replace=T, prob=p)))
sum(mean.vals > 5.5)/length(mean.vals)


###
# zad68

# с използване на ЦГТ
a <- (4000 - 160*24)/(7*sqrt(160))
1 - pnorm(a)

# Не можем да решим задачата със симулации, 
# понеже не знаем разпределението на X_1,..., X_n.


###
###
# zad70

# p = вероятност да се падне ези при хвърляне на монетата
# H0: p = 1/2
# H1: p > 1/2

x <- 58
n <- 100
p <- 0.5

z.obs <- (x/n - p)/sqrt(p*(1-p)/n)
z.obs
p.value <- 1-pnorm(z.obs)
p.value

# p.value = 0.0548 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Въз основа на данните не можем да заключим, че вероятността 
# да се падне ези при хвърляне на тази монета е повече от 1/2.


###
# zad71

# p = вероятност да се падне ези при хвърляне на монетата
# H0: p = 1/2
# H1: p > 1/2

x <- 61
n <- 100
p <- 0.5

z.obs <- (x/n - p)/sqrt(p*(1-p)/n)
z.obs
p.value <- 1-pnorm(z.obs)
p.value

# p.value = 0.0139 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Въз основа на данните можем да заключим, че вероятността 
# да се падне ези при хвърляне на тази монета е повече от 1/2.


###
# zad72

# X_i = диаметър на i-тата топка
# mu = E(X_i), т.е. mu = среден диаметър на топките, произведени от машината
# H0: mu = 6.7
# H1: mu != 6.7

mu <- 6.7
sigma <- 0.12
n <- 45
x.bar <- 6.73

z.obs <- (x.bar - mu)/(sigma/sqrt(n))
z.obs
p.value <- 2*(1-pnorm(abs(z.obs)))
p.value

# p.value = 0.0935 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Не можем да отхвърлим твърдението, че средният диаметър на топките, 
# произведени от машината, е 6.7 см.


###
# zad73

# X_i = диаметър на i-тата топка
# mu = E(X_i), т.е. mu = среден диаметър на топките, произведени от машината
# H0: mu = 6.7
# H1: mu != 6.7

mu <- 6.7
sigma <- 0.12
n <- 45
x.bar <- 6.76

z.obs <- (x.bar - mu)/(sigma/sqrt(n))
z.obs
p.value <- 2*(1-pnorm(abs(z.obs)))
p.value

# p.value = 0.000796 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Данните не подкрепят твърдението, че средният диаметър на топките, 
# произведени от машината, е 6.7 см.


###
# zad74

# X_i = концентрация на кадмий в i-тата гъба
# mu = E(X_i), т.е. mu = средна концентрация на кадмий в горските гъбите
# H0: mu = 4
# H1: mu < 4

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)
mu <- 4
n <- length(x)
x.bar <- mean(x)
s <- sd(x)

t.obs <- (x.bar - mu)/(s/sqrt(n))
t.obs
p.value <- pt(t.obs, n-1)
p.value

t.test( x, mu=4, alternative="less" )

# p.value = 0.0137 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Имаме основание да твърдим, че средната концентрация на кадмий
# в горските гъбите в дадената местност е по-малко от 4 мг/кг.


###
# zad75

# p = вероятност гълъб от дадения вид да намери пътя към дома си
# H0: p = 0.51
# H1: p > 0.51

x <- 32
n <- 58 
p <- 0.51

z.obs <- (x/n - p)/sqrt(p*(1-p)/n)
z.obs
p.value <- 1-pnorm(z.obs)
p.value

prop.test( x=32, n=58, p=0.51, alternative="greater", correct=F )

# p.value = 0.2625 > 0.05
# => нямаме основание да отвърлим нулевата хипотеза.
# Данните не подкрепят твърдението, че вероятността гълъб от 
# дадения вид да намери пътя към дома си е над 51%.


###
# zad76 а)

# X_i = ниво на хемоглобин на i-тото дете
# mu = E(X_i), т.е. mu = средно ниво на хемоглобин при децата страдащи от болестта
# H0: mu = 14.6
# H1: mu != 14.6

x <- c(12.3, 11.2, 14.2, 15.3, 14.8, 13.5, 11.1, 15.1, 15.4, 13.2)
mu <- 14.6
n <- length(x)
t.obs <- (mean(x) - mu)/(sd(x)/sqrt(n))
t.obs
p.value <- 2*(1-pt(abs(t.obs), n-1))
p.value 

t.test( x, mu=14.6 )

# p.value = 0.0877 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Нямаме основание да твърдим, че средното ниво на хемоглобин при 
# децата страдащи от тази болест е различно от нормалното ниво от 14.6.


###
# zad76 б)

# X_i = ниво на хемоглобин на i-тото дете
# mu = E(X_i), т.е. mu = средно ниво на хемоглобин при децата страдащи от болестта
# H0: mu = 14.6
# H1: mu < 14.6

x <- c(12.3, 11.2, 14.2, 15.3, 14.8, 13.5, 11.1, 15.1, 15.4, 13.2)
mu <- 14.6
n <- length(x)
t.obs <- (mean(x) - mu)/(sd(x)/sqrt(n))
t.obs 
p.value <- pt(t.obs, n-1)
p.value 

t.test( x, mu=14.6, alternative="less" )

# p.value = 0.04387 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Имаме основание да твърдим, че средното ниво на хемоглобин при 
# децата страдащи от тази болест е по-малко от 14.6.


###
# zad77 а)

# p = вероятност стока в хипермаркета да е с грешна цена
# H0: p = 0.075
# H1: p != 0.075

x <- 14
n <- 200
p <- 0.075
z.obs <- (x/n - p)/sqrt(p*(1-p)/n)
z.obs
p.value <- 2*(1-pnorm(abs(z.obs)))
p.value

prop.test( x=14, n=200, p=0.075, correct=F )

# p.value = 0.7883 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Нямаме основание да твърдим, че през 2010 вероятността 
# стока да е с грешна цена е различна от 7.5%.

###
# zad77 б)
# p = вероятност стока в хипермаркета да е с грешна цена
# H0: p = 0.075
# H1: p < 0.075

x <- 14
n <- 200
p <- 0.075
z.obs <- (x/n - p)/sqrt(p*(1-p)/n)
z.obs
p.value <- pnorm(z.obs)
p.value

prop.test( x=14, n=200, p=0.075, alternative="less", correct=F )

# p.value = 0.3942 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Нямаме основание да твърдим, че е през 2010 вероятността 
# стока да е с грешна цена е по-малка от 7.5%.


###
# zad78

# X_i = дължина на дясното ухо на i-тата жена
# mu = E(X_i), т.е. mu = средна дължина на дясното ухо на жените (18-30 години)
# H0: mu = 60
# H1: mu != 60

x.bar <- 61.9
sigma <- 4.1
n <- 66
mu <- 60
z.obs <- (x.bar - mu)/(sigma/sqrt(n))
z.obs
p.value <- 2*(1-pnorm(abs(z.obs)))
p.value

# p.value = 0.000167 < 0.05 
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Имаме основание да твърдим, че средната дължина на дясното ухо 
# на жените на възраст между 18 и 30 години е различна от 60 мм.


###
###
# zad80

# X_i = оценка на i-тия студент с вариант А
# Y_j = оценка на j-тия студент с вариант В
# mu_X = E(X_i), т.е. mu_X = средна оценка при падане на вариант А
# mu_Y = E(Y_j), т.е. mu_Y = средна оценка при падане на вариант В
# H0: mu_X = mu_Y
# H1: mu_X > mu_Y

examAB <- read.table( "examAB.txt", header=T )
x <- examAB$points[examAB$variant=="A"]
y <- examAB$points[examAB$variant=="B"]
t.test( x, y, alternative="greater" )

# p.value = 0.01668 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Имаме основание да твърдим, че в средно студентите получават 
# по-малко точки ако им се е падне вариант В.

# алтернативен синтаксис
t.test( points ~ variant, data=examAB, alternative="greater" )


###
# zad81

# X_i = време на реакция на i-тия участник преди изпиване на 50 мл водка  
# Y_i = време на реакция на i-тия участник след изпиване на 50 мл водка
# mu_X = E(X_i), т.е. mu_X = средно време на реакция преди изпиване на 50 мл водка
# mu_Y = E(Y_i), т.е. mu_Y = средно време на реакция след изпиване на 50 мл водка
# H0: mu_X = mu_Y
# H1: mu_X < mu_Y

react <- read.table( "reacttime.txt", header=T )
x <- react$before
y <- react$after
t.test( x, y, alternative="less", paired=T )
options(scipen=99)
t.test( x, y, alternative="less", paired=T )$p.value

# това ни дава същия резултат
t.test( x-y, alternative="less" )

# p.value = 0.000000000324 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Имаме основание да твърдим, че след изпиване на 50 мл водка
# времето на реакция в средно се увеличава.


###
# zad82

# p_A = вероятност машина А да произведе дефектен болт
# p_B = вероятност машина B да произведе дефектен болт
# H0: p_A = p_B
# H1: p_A != p_B

x <- c(8, 15)
n <- c(200, 200)

p.hat <- (x[1] + x[2]) / (n[1] + n[2])
z.obs <- (x[1]/n[1] - x[2]/n[2]) / sqrt(p.hat*(1-p.hat)*(1/n[1] + 1/n[2]))
z.obs
p.value <- 2*(1-pnorm(abs(z.obs)))
p.value

# може да ползваме и функцията prop.test
prop.test( x=x, n=n, correct=F )
prop.test( x, n, correct=F )

# p.value = 0.1327 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Нямаме основание да твърдим, че двете машини се различават
# по отношение на вероятността да произведат дефектен болт.


###
# zad83
# 1) двойки наблюдения
# 2) две независими извадки
# 3) двойки наблюдения
# 4) две независими извадки


###
# zad84

sim.t2 <- function(n, mu1, mu2, sigma1, sigma2) {
  x <- rnorm(n, mean=mu1, sd=sigma1)
  y <- rnorm(n, mean=mu2, sd=sigma2)
  t.test(x,y)$p.value
}

rs <- replicate( 10000, sim.t2(n=500, mu1=5, mu2=5, sigma1=1, sigma2=0.8) )
sum(rs<=0.05)/length(rs)


###
# zad85

rs <- replicate( 10000, sim.t2(n=1000, mu1=5, mu2=5.2, sigma1=1, sigma2=1) )
sum(rs<=0.05)/length(rs)


###
# zad86

# p1 = вероятност някой да е вегетарианец през 2007
# p2 = вероятност някой да е вегетарианец през 2019
# H0: p1 = p2
# H1: p1 < p2

x <- c(26, 43)
n <- c(500, 540)
prop.test( x=x, n=n, alternative="less", correct=F )

# p.value = 0.03683 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Имаме основание да твърдим, че процентът на вегетарианците 
# през 2019 година се е увеличил спрямо 2007 година.


###
# zad87

# mu_X = средно съдържание на примеси при определяне по метод 1
# mu_Y = средно съдържание на примеси при определяне по метод 2
# H0: mu_X = mu_Y
# H1: mu_X != mu_Y

# копирали сме таблицата с данни (без заглавния ред) в "steel.txt"
steel <- read.table( "steel.txt" )
x <- steel[,2]
y <- steel[,3]

t.test( x, y, paired=T )

# p.value = 0.01026 < 0.05
# => отхвърляме нулевата хипотеза в полза на алтернативната.
# Не може да се приеме, че двата метода
# дават в средно едни и същи резултати.

d <- x - y
n <- length(d)
t.obs <- mean(d)/(sd(d)/sqrt(n))
t.obs
p.value <- 2*(1-pt(abs(t.obs), n-1))
p.value 


###
# zad88

# mu_X = средно време за четене при първия шрифт
# mu_Y = средно време за четене при втория шрифт
# H0: mu_X = mu_Y
# H1: mu_X != mu_Y

x.bar <- 7.88
s.x <- 1.73
y.bar <- 8.48
s.y <- 2.12
n <- 50
m <- 50

t.obs <- (x.bar - y.bar) / sqrt(s.x^2/n + s.y^2/m)
t.obs
df <- (s.x^2/n + s.y^2/m)^2 / (((s.x^2/n)^2)/(n-1) + ((s.y^2/m)^2)/(m-1))
df
p.value <- 2*(1-pt(abs(t.obs), df))
p.value 

# p.value = 0.1243729 > 0.05
# => нямаме основание да отхвърлим нулевата хипотеза.
# Нямаме основание да твърдим, че средното време за четене  
# е различно за двата шрифта.

# Ако използваме нормално разпределение,
# получаваме приблизително същото p.value.
p.value1 <- 2*(1-pnorm(abs(t.obs)))
p.value1 


###
###
# zad89

z1.ci <- function(x.bar, sigma, n, alpha) {
  b1 <- x.bar - qnorm(1-alpha/2)*(sigma/sqrt(n))
  b2 <- x.bar + qnorm(1-alpha/2)*(sigma/sqrt(n))
  c(b1, b2)
}

z1.ci( x.bar=6.73, sigma=0.12, n=45, alpha=0.05 )
z1.ci( x.bar=6.76, sigma=0.12, n=45, alpha=0.05 )


###
# zad90

t1.ci <- function(x.bar, s, n, alpha) {
  b1 <- x.bar - qt(1-alpha/2, df=n-1)*(s/sqrt(n))
  b2 <- x.bar + qt(1-alpha/2, df=n-1)*(s/sqrt(n))
  c(b1, b2)
}

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)

t1.ci( x.bar=mean(x), s=sd(x), n=length(x), alpha=0.05 )
t1.ci( x.bar=mean(x), s=sd(x), n=length(x), alpha=0.10 )

t.test( x, conf.level=0.95 )$conf.int[1:2]
t.test( x, conf.level=0.90 )$conf.int[1:2]


###
# zad91

prop1.ci <- function(x, n, alpha) {
  p.hat <- x/n
  b1 <- p.hat - qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n)
  b2 <- p.hat + qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n)
  c(b1, b2)
}

prop1.ci( x=58, n=100, alpha=0.05 )
prop1.ci( x=116, n=200, alpha=0.05 )
prop1.ci( x=61, n=100, alpha=0.05 )

prop.test( x=58, n=100, conf.level=0.95, correct=F )$conf.int[1:2]
prop.test( x=116, n=200, conf.level=0.95, correct=F )$conf.int[1:2]
prop.test( x=61, n=100, conf.level=0.95, correct=F )$conf.int[1:2]


###
# zad92

z1.ci( x.bar=61.9, sigma=4.1, n=66, alpha=0.05 )

z1.ci( x.bar=61.9, sigma=4.1, n=88, alpha=0.05 )


###
# zad93

prop1.ci( x=32, n=58, alpha=0.05 )

prop.test( x=32, n=58, conf.level=0.95, correct=F )$conf.int[1:2]


###
# zad94

sim1 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  ci <- t.test(x, conf.level=1-alpha)$conf.int[1:2]
  cnd1 <- ( ci[1] <= 7 & ci[2] >= 7 )
  cnd1
}

res1 <- replicate( 10000, sim1(n=500, alpha=0.05) )
table(res1)/length(res1)


###
# zad95

sim2 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  p.val <- t.test(x, mu=7)$p.value
  cnd2 <- ( p.val > alpha )
  cnd2
}

res2 <- replicate( 10000, sim2(n=500, alpha=0.05) )
table(res2)/length(res2)


###
# zad96

sim3 <- function(n, alpha) {
  x <- runif(n, 5, 9)
  t.result <- t.test(x, mu=7, conf.level=1-alpha)
  ci <- t.result$conf.int[1:2]
  p.val <- t.result$p.value
  cnd1 <- ( ci[1] <= 7 & ci[2] >= 7 )
  cnd2 <- ( p.val > alpha )
  c( cnd1, cnd2 )
}

res3 <- replicate( 10000, sim3(n=500, alpha=0.05) )
cnd1r <- res3[1, ]
cnd2r <- res3[2, ]
table(cnd1r, cnd2r)


###
###
# zad98

# p_i = вероятност да се падне i, i=1...6
# H0: p_i = 1/6, i=1...6
# H1: p_i != 1/6 за поне едно i

x <- c(28, 36, 36, 30, 27, 23)
probs <- rep(1/6, 6)

n <- sum(x)
k <- length(probs)
chi2.obs <- sum( (x - n*probs)^2 / (n*probs) )
chi2.obs
p.value <- 1-pchisq(chi2.obs, df=k-1)
p.value

chisq.test(x=x, p=probs)
chisq.test(x=x, p=probs)$p.value

# p.value = 0.4844 > 0.05 
# => нямаме основания да отхвърлим H0.
# Не можем да отхвърлим хипотезата, че зарът е балансиран 
# (всяка от страните се пада с вероятност 1/6).

tb1 <- rbind( probs, x/sum(x) )
colnames(tb1) <- as.character(c(1:6))
rownames(tb1) <- c("probability (H0)", "observed freq")
barplot(tb1, beside=T, legend=T, ylim=c(0,0.3))


###
# zad99

# p_E = вероятност да срещнем буква Е
# p_T = вероятност да срещнем буква T
# ...
# H0: (p_E, p_T, ...) = (0.1202, 0.0910, ...)
# H1: (p_E, p_T, ...) != (0.1202, 0.0910, ...)

load("letterFreq.RData")
probs*100
x1

chisq.test(x=x1, p=probs)

# p.value < 0.05 
# => отхвърляме H0 в полза на H1.
# Нямаме основания да твърдим, че вероятностите за срещане 
# на съответните букви са както в английския език.

tb1 <- rbind( probs, x1/sum(x1) )
rownames(tb1) <- c("probability (H0)", "observed freq")
barplot(tb1, beside=T, legend=T, args.legend=list(x="top"))


###
# zad102

library(MASS)
tb <- table( survey$Smoke, survey$Sex )
tb

# H0: пушенето и пола са независими
# H1: пушенето и пола не са независими (има връзка между пушене и пол)

n <- sum(tb)
df <- (nrow(tb)-1)*(ncol(tb)-1)
smk <- apply(tb, 1, sum)
sex <- apply(tb, 2, sum)

expected <- (smk %o% sex)/n
observed <- tb
chi2.obs <- sum( (observed - expected)^2 / expected )
chi2.obs
p.value <- 1-pchisq(chi2.obs, df)
p.value

chisq.test(tb)

# p.value = 0.3139 > 0.05 
# => нямаме основания да отхвърлим нулевата хипотеза.
# Нямаме основания да твърдим, че има връзка между пушенето и пола.

100*prop.table(tb, 2)
barplot( 100*prop.table(tb, 2), 
         legend=T, xlim=c(0,4),
         args.legend=list(x="right") )

observed
chisq.test(tb)$observed
expected
chisq.test(tb)$expected


###
# zad103

eyecol <- read.table("ManWomanEye.txt", header=T)
attach(eyecol)
tb <- table(man, woman)
tb

# H0: цвета на очите на мъжа и цвета на очите на жената са независими
# H1: има връзка между цвета на очите на мъжа и цвета на очите на жената
#     (не са независими)

n <- sum(tb)
df <- (nrow(tb)-1)*(ncol(tb)-1)
males <- apply(tb, 1, sum)
females <- apply(tb, 2, sum)

expected <- (males %o% females)/n
observed <- tb
chi2.obs <- sum( (observed - expected)^2 / expected )
chi2.obs
p.value <- 1-pchisq(chi2.obs, df)
p.value

chisq.test(tb)

# p.value < 0.05 
# => отхвръляме нулевата хипотеза в полза на алтернативната.
# Можем да твърдим, че има връзка между
# цвета на очите на мъжа и цвета на очите на жената.

mosaicplot(tb)
barplot(tb)

100*prop.table(tb, 2)
barplot( 100*prop.table(tb, 2), xlab="Woman",
         legend=T, xlim=c(0,6),
         args.legend=list(x="right") )


###
# zad106

sim.dice1 <- function(n) {
  dice <- sample( c(1:6), n, replace=T )
  x <- tabulate(dice, nbins=6)
  chisq.test( x=x, p=rep(1/6, 6) )$p.value
}

res <- replicate( 10000, sim.dice1(100) )
sum(res>0.05)/length(res)

# Заключението на теста е вярно ако не отхвърлим H0, 
# т.е. ако p.value > 0.05.


###
###
# zad107

# а)
n <- 100
x <- rnorm(n, 5, 1)
y <- 2*x
plot(x,y)
cor(x,y)

# б)
x <- rnorm(n, 5, 1)
eps <- rnorm(n, 0, 1)
y <- 2*x + eps
plot(x,y)
cor(x,y)

# в)
x <- rnorm(n, 5, 1)
eps <- rnorm(n, 0, 2)
y <- 2*x + eps
plot(x,y)
cor(x,y)

# г)
x <- rnorm(n, 5, 1)
eps <- rnorm(n, 0, 2)
y <- 0.1*x + eps
plot(x,y, ylim=c(-8,8))
cor(x,y)

# д)
x <- rnorm(n, 5, 1)
y <- rnorm(n, 5, 1)
plot(x,y)
cor(x,y)

# е)
x <- rnorm(n, 5, 1)
eps <- rnorm(n, 0, 1)
y <- (-2)*x + eps
plot(x,y)
cor(x,y)


###
# zad108

alco <- read.table("bac.txt", header=T)

# a)
m1 <- lm( bac ~ beers, data=alco )
m1
# y = съдържание на алкохол в кръвта (bac)
# x = брой изпити бутилки бира (beers)
# модел:
# y = beta0 + beta1*x + eps
#  друг запис: bac = beta0 + beta1*beers + eps
# оценено регресионно уравнение:
# y.hat = -0.01270 + 0.01796*x
#  друг запис: bac.hat = -0.01270 + 0.01796*beers

plot( bac ~ beers, data=alco )
abline( coef(m1), lwd=2 )

# графика на остатъците срещу y.hat
plot( fitted(m1), resid(m1) )

# б)
# Според оцененото регресионно уравнение:
# 
# При изпиване на още една бира съдържанието на алкохол в кръвта
# се увеличава средно с 0.01796.
# 
# При нула изпити бири средното съдъжание на алкохол в кръвта е -0.01270.

# в)
# H0: beta1 = 0
# H1: beta1 != 0

summary(m1)
summary(m1)$coefficients
p.value <- summary(m1)$coefficients[2,4]
p.value < 0.05

# p.value < 0.05 => отхвърляме H0.
# Можем да твърдим, че има линейна връзка между броя на 
# изпитите бутилки бира и съдържанието на алкохол в кръвта.

# г)
# H0: beta1 = 0.02
# H1: beta1 != 0.02

# доверителни интервали (ДИ) за beta0 и beta1
confint(m1)

# 0.02 е в ДИ [0.0128, 0.0231] за beta1 => не отхвърляме H0.
# Нямаме основания да отхвърлим хипотезата, че при изпиването на 
# още една бира съдържанието на алкохол в кръвта се увеличава 
# средно с 0.02.

# д)
predict( m1, data.frame( beers=5 ), interval="confidence" )

# [ -0.01270 + 0.01796*5 = fit ]
# 
# Доверителният интервал е [0.066, 0.088]

# е)
plot( bac ~ beers, data=alco )
abline( coef(m1), lwd=2 )

xx <- data.frame( beers=seq(1, 9, 0.5) )

# доверителни интервали
cint <- predict( m1, xx, interval="confidence" )
lines( xx$beers, cint[,2], type="l", lty="dotted", lwd=2 )
lines( xx$beers, cint[,3], type="l", lty="dotted", lwd=2 )

# интервали за прогноза
pint <- predict( m1, xx, interval="prediction" )
lines( xx$beers, pint[,2], type="l", lty="dotted", col="coral", lwd=2 )
lines( xx$beers, pint[,3], type="l", lty="dotted", col="coral", lwd=2 )


###
# zad109

# a)
set.seed(12)
n <- 50
beta0 <- 2
beta1 <- 1.5
sigma <- 2
x <- runif(n, 1, 7)
eps <- rnorm(n, 0, sigma)
y <- beta0 + beta1*x + eps

m1 <- lm(y~x)
plot(x, y)
abline( coef(m1), lwd=2 )
abline( a=beta0, b=beta1, col="coral", lwd=2 )

summary(m1)
summary(m1)$r.squared
confint(m1)

# б)
set.seed(12)
n <- 50
beta0 <- 2
beta1 <- 1.5
sigma <- 1
x <- runif(n, 1, 7)
eps <- rnorm(n, 0, sigma)
y <- beta0 + beta1*x + eps

m2 <- lm(y~x)
plot(x, y)
abline( coef(m2), lwd=2 )
abline( a=beta0, b=beta1, col="coral", lwd=2 )

summary(m2)
summary(m2)$r.squared
confint(m2)

# в)
set.seed(12)
n <- 50
beta0 <- 2
beta1 <- 0.17
sigma <- 1
x <- runif(n, 1, 7)
eps <- rnorm(n, 0, sigma)
y <- beta0 + beta1*x + eps

m3 <- lm(y~x)
plot(x, y, ylim=c(0,13))
abline( coef(m3), lwd=2 )
abline( a=beta0, b=beta1, col="coral", lwd=2 )

summary(m3)
summary(m3)$r.squared
confint(m3)


###
# zad110

set.seed(12)
n <- 50
beta0 <- 2
beta1 <- 1.1
sigma <- 2.5
x <- runif(n, 1, 7)
eps <- rnorm(n, 0, sigma)
y <- beta0 + beta1*x^2 + eps

m4 <- lm(y~x)
plot(x, y)
abline( coef(m4), lwd=2 )

summary(m4)
summary(m4)$r.squared
confint(m4)

plot( fitted(m4), resid(m4) )


###
# zad111

satgpa <- read.table("satgpa.txt", header=T)

# scatter plot matrix
pairs( satgpa, gap=0, cex.labels=0.9 )

# а)
m1 <- lm( fy_gpa ~ hs_gpa, data=satgpa )
m1
# y = успех от първата година в колежа (fy_gpa)
# x = успех от гимназията (hs_gpa)
# модел:
# y = beta0 + beta1*x + eps
# оценено регресионно уравнение:
# y.hat = 0.09132 + 0.74314*x

plot( fy_gpa ~ hs_gpa, data=satgpa )
abline( coef(m1), lwd=2 )

# графика на остатъците срещу y.hat
plot( fitted(m1), resid(m1) )

# б)
# Според оцененото регресионно уравнение,
# успех от гимназията по-висок с 1 съответства на 
# успех от първата година в колежа по-висок средно с 0.74314.

# H0: beta1 = 0
# H1: beta1 != 0
summary(m1)
p.value <- summary(m1)$coefficients[2,4]
p.value < 0.05 
# => отхвърляме H0.
# Можем да твърдим, че има линейна връзка между успеха от гимназията
# и успеха от първата година в колежа.

# в)
# доверителен интервал
predict( m1, data.frame( hs_gpa=3.5 ), interval="confidence" )

# интервал за прогноза
predict( m1, data.frame( hs_gpa=3.5 ), interval="prediction" )

# г)
m2 <- lm( fy_gpa ~ hs_gpa + sat_sum, data=satgpa )
summary(m2)
plot( fitted(m2), resid(m2) )

m3 <- lm( fy_gpa ~ hs_gpa + sat_m, data=satgpa )
summary(m3)
plot( fitted(m3), resid(m3) )

m4 <- lm( fy_gpa ~ hs_gpa + sat_v, data=satgpa )
summary(m4)
plot( fitted(m4), resid(m4) )

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
summary(m4)$adj.r.squared
# Моделът m2 има най-голям adj.r.squared => е най-добър от трите
# (ако считаме, че моделът с най-висок adj.r.squared е най-добър).


###
# zad112

cher <- read.table("cherry.txt", header=T)
# scatter plot matrix
pairs( cher, gap=0, cex.labels=0.9 )

# а)
m1 <- lm( volume ~ diam, data=cher )
m1
# y = обем (volume)
# x = диаметър (diam)
# модел:
# y = beta0 + beta1*x + eps
# оценено регресионно уравнение:
# y.hat = -36.943 + 5.066*x

plot( volume ~ diam, data=cher )
abline( coef(m1), lwd=2 )

summary(m1)
plot( fitted(m1), resid(m1) )

# б)
# Според оцененото регресионно уравнение, ако диаметърът е по-голям с 1 инч, 
# обемът ще е по-голям средно с 5.066 кубични фута.

# в)
m2 <- lm( volume ~ diam + height, data=cher )
m2
# y = обем (volume)
# x1 = диаметър (diam)
# x2 = височина (height)
# модел:
# y = beta0 + beta1*x1 + beta2*x2 + eps
# оценено регресионно уравнение:
# y.hat = -57.9877 + 4.7082*x1 + 0.3393*x2

summary(m2)
plot( fitted(m2), resid(m2) )

# Според оцененото регресионно уравнение:
# 
# Ако диаметърът е по-голям с 1 инч при фиксирана височина, 
# обемът ще е по-голям средно с 4.7082 кубични фута.
# 
# Ако височината е по-голяма с 1 фут при фиксиран диаметър, 
# обемът ще е по-голям средно с 0.3393 кубични фута.

# г)
ci.g <- predict( m2, data.frame( diam=14, height=70 ), interval="confidence" )
ci.g

# д)
cher$diam2 <- cher$diam^2
m3 <- lm( volume ~ diam2 + height, data=cher )
summary(m3)
plot( fitted(m3), resid(m3) )

ci.d <- predict( m3, data.frame( diam2=14^2, height=70 ), interval="confidence" )
ci.d

# Доверителният интервал с модел m3 е по-къс:
ci.g[3] - ci.g[2]
ci.d[3] - ci.d[2]

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
# Моделът m3 има най-голям adj.r.squared

# Друг начин да включим квадрата на диаметъра:
m4 <- lm( volume ~ I(diam^2) + height, data=cher )
predict( m4, data.frame( diam=14, height=70 ), interval="confidence" )


###
# zad113

duke <- read.csv("duke_forest.csv", header=T)
pairs( duke[,c(2,3,4,5,7,11)], gap=0, cex.labels=0.9 )

# backward elimination:
# Построяваме модел включващ всички възможни предиктори.
# Премахваме предиктора с най-голямо p.value докато получим модел,
# в който всички предиктори имат p.value < 0.05.

options(scipen=999)

m01 <- lm( price ~ bed + bath + area + year_built + lot, data=duke )
summary(m01)$coefficients

m02 <- lm( price ~ bath + area + year_built + lot, data=duke )
summary(m02)$coefficients

m03 <- lm( price ~ bath + area + lot, data=duke )
summary(m03)$coefficients

# Получаваме модел m03 като резултат от backward elimination.

# Друг подход:
# В модела не е желателно да има предиктори, които са силно корелирани.
# От корелационната матрица виждаме, че bed, bath и area са силно корелирани помежду си.
cor( duke[,c(3,4,5)], use="complete.obs" )

# Затова в модела може да включим само една от променливите bed, bath и area, 
# като запазим year_built + lot.

m21 <- lm( price ~ area + year_built + lot, data=duke )
summary(m21)$coefficients

m22 <- lm( price ~ bed + year_built + lot, data=duke )
summary(m22)$coefficients

m23 <- lm( price ~ bath + year_built + lot, data=duke )
summary(m23)$coefficients

summary(m21)$adj.r.squared
summary(m22)$adj.r.squared
summary(m23)$adj.r.squared

# От горните три модела, m21 има най-голям adj.r.squared. 
# В модела m21 всички предиктори имат p.value < 0.05.

# Ако сравним adj.r.squared на m21 и m03, ще видим, че не се различават съществено.
summary(m03)$adj.r.squared


###
# zad114

sim.lm.a <- function(n) {
  x <- runif(n, 1, 10)
  eps <- rnorm(n, 0, 5) 
  y <- 2 + 5*x + eps
  m1 <- lm(y~x)
  c( coef(m1)["x"], confint(m1, "x"), use.names=F )
}

sim.lm.b <- function(n) {
  x <- runif(n, 1, 10)
  eps <- rexp(n, 1/5)
  y <- 2 + 5*x + eps
  m1 <- lm(y~x)
  c( coef(m1)["x"], confint(m1, "x"), use.names=F )
}

smry <- function(rs) {
  mean.b1.hat <- mean( rs[1,] )
  ci.coverage <- sum( rs[2,] <= 5 & 5 <= rs[3,] ) / ncol(rs)
  mean.ci.len <- mean( rs[3,] - rs[2,] )
  c(mean.b1.hat, ci.coverage, mean.ci.len)
}


res.a.30 <- replicate( 10000, sim.lm.a(30) )
res.b.30 <- replicate( 10000, sim.lm.b(30) )

smry(res.a.30)
smry(res.b.30)

res.a.50 <- replicate( 10000, sim.lm.a(50) )
res.b.50 <- replicate( 10000, sim.lm.b(50) )

smry(res.a.50)
smry(res.b.50)

res.a.100 <- replicate( 10000, sim.lm.a(100) )
res.b.100 <- replicate( 10000, sim.lm.b(100) )

smry(res.a.100)
smry(res.b.100)

res.a.500 <- replicate( 10000, sim.lm.a(500) )
res.b.500 <- replicate( 10000, sim.lm.b(500) )

smry(res.a.500)
smry(res.b.500)


###
###



