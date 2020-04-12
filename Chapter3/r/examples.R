# implementation of examples from chapter 2 in R
library(rethinking)

# Create a function for water problem
posterior_function <- function(p_grid, prior, x, size) {
	likelihood <- dbinom(x=x, size=size, prob=p_grid)
	unstd.posterior <- likelihood * prior
	posterior <- unstd.posterior / sum(unstd.posterior)
	return(posterior)
}


birth1 = c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,  0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,  1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,  1,0,1,1,1,0,1,1,1,1) 
birth2 = c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,  1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,  1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,  0,0,0,1,1,1,0,0,0,0)

num_grid_points <- 1000
p_grid <- seq(from=0, to=1, length.out=num_grid_points)
prior <- rep(1, num_grid_points) 
posterior <- posterior_function(p_grid, prior, 111, 200)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

print(p_grid[which.max(posterior)]) #3H1
print(HPDI(samples, prob=0.5)) # 3H2
print(HPDI(samples, prob=0.89)) #3H2
print(HPDI(samples, prob=0.97)) #3H2

boys <- rbinom(n=10000, size=200, prob=samples)
dens(boys)
abline(v=111, col='red')

first.boys <-rbinom(n=10000, size=100, prob=samples)
dens(first.boys)
abline(v=sum(birth1), col='red')

first.girls = length(birth1) - sum(birth1)
boys.following.first.girl <- rbinom(n=10000, size=first.girls, prob=samples)
boys.following.first.girl.obs = sum(birth2[birth1 == 0])
dens(boys.following.first.girl)
abline(v=boys.following.first.girl.obs, col='red')
print(boys.following.first.girl.obs)