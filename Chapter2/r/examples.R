# implementation of examples from chapter 2 in R
library(rethinking)
# R code 2.3
# define grid
num_grid_points <- 20
# Evaluate for different values of p in this grid
p_grid <- seq(from=0, to=1, length.out=num_grid_points)
# all values of p are equally likely
prior <- rep(1, num_grid_points) 
likelihood <- dbinom(x=6, size=9, prob=p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

# plot
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")

# R code 2.8
n_samples <- 1000
p <- rep(NA, n_samples)
p[1] <- 0.5
W <- 6
L <- 3
for (i in 2:n_samples) {
	p_new <- abs(rnorm(1, p[i-1], 0.1))
	if(p_new > 1) {
		p_new <- 2 - p_new
	}
	q0 <- dbinom(W, W + L, p[i-1])
	q1 <- dbinom(W, W + L, p_new)
	p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

dens(p, xlim=c(0, 1))
curve(dbeta(x, W+1, L+1), lty=2, add=TRUE)

# Create a function for water problem
posterior_function <- function(p_grid, prior, x, size) {
	likelihood <- dbinom(x=x, size=size, prob=p_grid)
	unstd.posterior <- likelihood * prior
	posterior <- unstd.posterior / sum(unstd.posterior)
	return(posterior)
}
num_grid_points <- 20
p_grid <- seq(from=0, to=1, length.out=num_grid_points)
prior <- rep(1, num_grid_points) 

posterior <- posterior_function(p_grid, prior, 3, 3)
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")

posterior <- posterior_function(p_grid, prior, 3, 4)
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")

posterior <- posterior_function(p_grid, prior, 5, 7)
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")

prior <- ifelse(p_grid <0.5, 0, 1)
posterior <- posterior_function(p_grid, prior, 5, 7)
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")
