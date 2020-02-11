

rm(list=ls())
# setwd("/Users/tonyfischetti/Desktop/travel-stats/")


library(readr)
library(magrittr)
library(dplyr)
library(lubridate)
library(parallel)
library(assertr)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat <- read_csv("./data/travel-stats.csv") %>%
  mutate(thedate=ymd(thedate)) %>%
  mutate(total_time = minutes_to_train + waiting_for_train + train_ride +
           to_library)

fulldat <- dat

# only traditional route
dat %<>% filter(method=="c_163")




#########################################
###       normal distribution         ###
#########################################


send.data <- list(
  total_time = dat$total_time,
  theLength = length(dat$total_time)
)

fit <- stan(file='normal.stan',
            data=send.data, 
            iter=100000, chains=3)


plot(dnorm(0:110, 49.55, 12.27), type="l", xlab="minutes",
     ylab="probability density", main="bayesian models of travel time to work",
     col="red", lwd=2, ylim=c(0, .07))


normal.lower <- qnorm(.025, 49.55, 12.27)
normal.upper <- qnorm(.975, 49.55, 12.27)







#########################################
###       gamma distribution          ###
#########################################


send.data <- list(
  total_time = dat$total_time,
  theLength = length(dat$total_time)
)

fit <- stan(file='gamma.stan',
            data=send.data, 
            iter=100000, chains=3)

lines(dgamma(1:110, shape=42.93, rate=.87), col="green",
      lwd=2)

gamma.lower <- qgamma(.025, shape=42.93, rate=.87)
gamma.upper <- qgamma(.975, shape=42.93, rate=.87)






#########################################
###       weibull distribution        ###
#########################################


send.data <- list(
  total_time = dat$total_time,
  theLength = length(dat$total_time)
)

fit <- stan(file='weibull.stan',
            data=send.data, 
            iter=100000, chains=3)

lines(dweibull(1:110, shape=4.58, scale=54.93), col="blue",
      lwd=2)


weibull.lower <- qweibull(.025, shape=4.58, scale=54.93)
weibull.upper <- qweibull(.975, shape=4.58, scale=54.93)




###

lines(c(normal.lower, normal.upper), c(0.002, 0.002), lwd=3, col="red")
lines(c(gamma.lower, gamma.upper), c(.004, .004), col="green", lwd=3)
lines(c(weibull.lower, weibull.upper), c(.000, .000), col="blue", lwd=3)



legend("topright", legend=c("normal    (25 - 74)",
                            "gamma   (35 - 65)",
                            "weibull    (25 - 73)"),
       col=c("red", "green", "blue"), 
       title="distribution  95% CI" ,pch=19)








