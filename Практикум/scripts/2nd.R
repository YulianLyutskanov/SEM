sim.gifts <- function(k) {
	x <- sample(1:k, k, replace=FALSE)
	y <- x - c(1:k)
	any(y == 0)
}

prob.gifts <- function(times, k) {
	x <- replicate(times, sim.gifts(k))
	sum(x)/length(x)	
}

#prob.gifts(100000, 20)

sim.bday <- function(k) {
	x <- sample(1:365, k, replace=TRUE)
	anyDuplicated(x) > 0
}

prob.bday <- function(times, k) {
	x <- replicate(times, sim.bday(k))
	sum(x)/length(x)
}

#prob.bday(10000,25)

sim.bw <- function() {
	card <- sample( c("bb", "ww", "wb"), 1)
	side <- sample( c(1,2), 1)
	up <- substr( card, start=side, stop=side)
	c(up, card)
}

prob.bw <- function(times) {
	x <- replicate(times, sim.bw())
	sum(x[2,] == "ww") / sum(x[1,] == "w")
}

#prob.bw(10000)