
stargazer::stargazer(lm(Sepal.Length ~ Sepal.Width + Species, data = iris), type = "text")

stargazer::stargazer(glm(Sepal.Length ~ Sepal.Width + Species, data = iris,
                         family = "gaussian"), type = "text")
# log likelihood and akaike inf. crit. added 

stargazer::stargazer(glm(am ~ cyl + disp, data = mtcars,
                         family = "binomial"), type = "text")
# terms are different 

x <- 1:1000
plot(pbinom(x, size = 1000, prob = 0.2), type = "s", col = "blue",
     main = "Binomial distribution function",
     xlab = "Number of successes", ylab = "F(x)")
lines(pbinom(x, size = 1000, prob = 0.4), type = "s", col = "red")
lines(pbinom(x, size = 1000, prob = 0.6), type = "s", col = "green")
legend("bottomright", 
       legend = c("0.2", "0.4", "0.6"), 
       col = c("blue","red","green"),
       title = "probability")



dat <- iris
dat$set <- ifelse(iris$Species == "setosa", 1, 0)
mod1 <- lm(set ~ Petal.Length + Petal.Width, data = dat)
stargazer::stargazer(mod1, type = "text")

newdat <- data.frame(Petal.Length = 5.4,
                     Petal.Width = 2.4) # have to make a data frame which is identical to the input data in the first model 
predict(mod1, newdat) # negative result for probability - no sense 


mod2 <- glm(set ~ Petal.Length + Petal.Width, data = dat,
            family = "binomial")
predict(mod2, dat)
stargazer::stargazer(mod2, type = "text")
predict(mod2, newdat) # -106.8992 
# less likely to be a setosa, but less likely than what? 
predict(mod2, newdat, type = "response") # specifying gets us probability scale 
# vvvvv small number but bounded between 0 and 1 

newdat2 <- data.frame(Petal.Length = 2.3,
                      Petal.Width = 0.8)
predict(mod2, newdat2, type = "response")
# 86% yes it's a setosa  

log_odds <- predict(mod2, newdat2)
exp(log_odds)/(1+exp(log_odds))
## same same 

set.seed(2022)
options(scipen = 500)

x <- rnorm(10000) # 1000 random draws from the normal distribution with mean of 0 and 
#sd of 1.

# setting terms i want to find - seeing if R can estimate them for me from a Y value 
intercept <- 3 # the coefficient of our intercept
slope <- 0.2 # the coefficient of our slope

y_binom <- rbinom(10000, 1, exp(intercept + slope*x)/(1+exp(intercept + slope*x))) # look at rbinom help file l8r
# our outcome variable.  
# inverse of the logodds function from above - getting p 

binom_glm <- glm(y_binom ~ x, family = "binomial")
stargazer::stargazer(binom_glm, type = "text")
# gets 3.025 close to 3 our initial value
# gets v close to 0.2 (0.213)
# so did well
# but what's going on beneath that

# optim() to figure out
# minimises (or maximises) a function
# not sure i get this 

f <- function(b) {
  p <- exp(b[1] + b[2]*x)/(1+exp(b[1] + b[2]*x))
  -sum(dbinom(y_binom, 1, p, log=TRUE)) # density function of the binomial, because another way of doing logit regression (-u) - "read the assigned methods chapter carefully" - sort of another term in the logit equation "
}

ans <- optim(fn = f, par = 0:1, hessian=TRUE) # hessian parameters 
ans$par # R is maximising or minimising a function 










