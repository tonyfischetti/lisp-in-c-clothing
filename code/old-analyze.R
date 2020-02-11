

rm(list=ls())


library(readr)
library(magrittr)
library(dplyr)
library(lubridate)
library(parallel)
library(assertr)
library(coda)
library(rjags)
library(runjags)

dat <- read_csv("./travel-stats.csv") %>%
  mutate(thedate=ymd(thedate)) %>%
  mutate(total_time = minutes_to_train + waiting_for_train + train_ride +
           to_library)

add.cis <- function(thesamples, height=0, length=.5,
                    printout, lwid=1, color="blue"){
  inter <- quantile(thesamples, c(.05, .975))
  if(printout) print(inter)
  lines(c(inter[1], inter[2]), c(height, height), col=color, lwd=lwid)
  lines(c(inter[1], inter[1]), c(height-(length/2), height+(length/2)), col=color, lwd=lwid)
  lines(c(inter[2], inter[2]), c(height-(length/2), height+(length/2)), col=color, lwd=lwid)
}

add.cis.mcmc <- function(theobject, row=1, height=0,
                    printout=FALSE, lwid=1, color="blue"){
  inter <- HPDinterval(as.mcmc(theobject))
  if(printout) print(inter)
  thelow <- inter[row,1]
  thehigh <- inter[row,2]
  print("")
  print(thelow)
  print(thehigh)
  lines(c(thelow, thehigh), c(height, height), col=color, lwd=lwid)
}

fulldat <- dat

# only traditional route
dat %<>% filter(method=="c_163")



##########################################
###  normal likelihood uniform priors  ###
##########################################

the.model <- "
  model {
  
    # priors
    mu ~ dunif(0, 160)
    stddev ~ dunif(0, 100)

    tau <- pow(stddev, -2)

    # likelihood function
    for(i in 1:theLength){
      total_time[i] ~ dnorm(mu, tau)
    }
  }
"

send.data <- list(
  total_time = dat$total_time,
  theLength = length(dat$total_time)
)

results <- autorun.jags(the.model,
                        data=send.data,
                        n.chains = 3,
                        startsample = 1000000,
                        monitor = c('mu', 'stddev'))

plot(results,
     plot.type=c("histogram", "trace"),
     layout=c(2,2))

results.matrix <- as.matrix(results$mcmc)

mu.samples <- results.matrix[,'mu']
stddev.samples <- results.matrix[,'stddev']


############################## added
theintervals <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  lower <- qnorm(.025, mu.samples[k], stddev.samples[k])
  upper <- qnorm(.975, mu.samples[k], stddev.samples[k])
  theintervals[k,1] <- lower
  theintervals[k,2] <- upper
}
theintervals <- data.frame(theintervals)
names(theintervals) <- c("lower", "upper")

mean(theintervals$lower)
mean(theintervals$upper)
lines(c(mean(theintervals$lower), mean(theintervals$upper)),
      c(.02, .02), col="purple", lwd=3)


for(j in 1:50){
  lines(dnorm(1:150, mu.samples[j], stddev.samples[j]), col="gray")
}
lines(density(mu.samples), lwd=2)
############################## added




mu.samples %>% density %>% plot(main="travel time to work (normal likelihood, uniform prior)",
                                ylab="", xlab="minutes")

add.cis.mcmc(results, height=.0004, printout=TRUE)
abline(v=modeest::naive(mu.samples), col="red")

lines(dunif(0:160, min=0, max=160), col="gray")
text(130, .0072, "prior", col="gray")

old.results.normal.flat <- results



##############################################
###  normal likelihood informative priors  ###
##############################################

the.model <- "
  model {

    # priors
    mu ~ dnorm(50, .0012)
    stddev ~ dnorm(20,.0001)

    tau <- pow(stddev, -2)

    # likelihood function
    for(i in 1:theLength){
      total_time[i] ~ dnorm(mu, tau)
    }
  }
"

send.data <- list(
  total_time = dat$total_time,
  theLength = length(dat$total_time)
)

results <- autorun.jags(the.model,
                        data=send.data,
                        n.chains = 3,
                        startsample = 1000000,
                        monitor = c('mu', 'stddev'))

plot(results,
     plot.type=c("histogram", "trace"),
     layout=c(2,2))

results.matrix <- as.matrix(results$mcmc)

mu.samples2 <- results.matrix[,'mu']
stddev.samples2 <- results.matrix[,'stddev']


############################## added
theintervals <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  lower <- qnorm(.025, mu.samples2[k], stddev.samples2[k])
  upper <- qnorm(.975, mu.samples2[k], stddev.samples2[k])
  theintervals[k,1] <- lower
  theintervals[k,2] <- upper
}
theintervals <- data.frame(theintervals)
names(theintervals) <- c("lower", "upper")

mean(theintervals$lower)
mean(theintervals$upper)
lines(c(mean(theintervals$lower), mean(theintervals$upper)),
      c(.02, .02), col="purple", lwd=3)


for(j in 1:50){
  lines(dnorm(1:150, mu.samples2[j], stddev.samples2[j]), col="gray")
}
############################## added



mu.samples2 %>% density %>% plot(main="travel time to work (normal likelihood, weak normal prior)",
                                 ylab="", xlab="minutes",
                                 xlim=c(0,140))

add.cis.mcmc(results, height=.002, printout=TRUE, lwid=2)
abline(v=modeest::naive(mu.samples2), col="pink", lty=2)

lines(dnorm(0:160, mean=50, sd=28), col="gray")
text(90, .008, "prior", col="gray")

add.cis.mcmc(old.results.normal.flat, height=.0006, printout=TRUE, col="red", lwid=2)

legend("topright", legend=c("95% CI of posterior (informative prior)",
                            "95% CI of posterior (flat prior)"),
       col=c("blue", "red"), pch=19)

old.results.normal.normal <- results





#########################################
###  gamma likelihood uniform priors  ###
#########################################

the.model <- "
  model {
  
  # priors
  mshape ~ dunif(0,150)
  mrate ~ dunif(0,2)

  # likelihood function
  for(i in 1:theLength){
    total_time[i] ~ dgamma(mshape, mrate)
  }
}"

send.data <- list(
  total_time = dat$total_time,
  theLength = length(dat$total_time)
)


results <- autorun.jags(the.model,
                        data=send.data,
                        n.chains = 3,
                        startsample = 1000000,
                        monitor = c('mshape', 'mrate'))

plot(results,
     plot.type=c("histogram", "trace"),
     layout=c(2,2))

results.matrix <- as.matrix(results$mcmc)

shape.samples <- results.matrix[,'mshape']
rate.samples <- results.matrix[,'mrate']


mu.samples %>% density %>% plot(main="travel time to work (gamma likelihood, flat priors)",
                                ylab="", xlab="minutes", ylim=c(0, .09),
                                xlim=c(0,165))
for(j in 1:50){
  lines(dgamma(1:150, 
               shape=results.matrix[j,1], 
               rate=results.matrix[j,2]), 
        col="gray")
}
lines(density(mu.samples), lwd=2)


theintervals <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  lower <- qgamma(.025, shape=shape.samples[k], rate=rate.samples[k])
  upper <- qgamma(.975, shape=shape.samples[k], rate=rate.samples[k])
  theintervals[k,1] <- lower
  theintervals[k,2] <- upper
}
theintervals <- data.frame(theintervals)
names(theintervals) <- c("lower", "upper")

lines(c(mean(theintervals$lower), 
        mean(theintervals$upper)), c(.002, .002), col="red", lwd=2)

add.cis.mcmc(old.results.normal.normal, height=.004, printout=TRUE, lwid=2, col="blue")

legend("topright", legend=c("posterior with normal likelihood",
                            "posteriors with gamma likelihood",
                            "95% CI of posterior (normal, informative prior)",
                            "95% CI of posterior (gamma, flat prior)"),
       col=c("black", "gray", "blue", "red"), pch=19)








##############################################
#####          PLOTTING STUFF            #####
##############################################

mu.samples %>% density %>% plot(main="travel time to work",
                                ylab="", xlab="minutes")

lines(dgamma(1:160, shape=4.5, rate=.07), col="gray")


lines(dgamma(1:160, shape=4, rate=.08), col="gray")
lines(dgamma(1:160, shape=5, rate=.08), col="gray")
lines(dgamma(1:160, shape=5, rate=.09), col="gray")
lines(dgamma(1:160, shape=8, rate=.09), col="gray")
lines(dgamma(1:160, shape=3, rate=.05), col="gray")
lines(dgamma(1:160, shape=15, rate=.05), col="gray")
# unlikey lines(dgamma(1:160, shape=4.5, rate=.1), col="gray")
# very unlikely lines(dgamma(1:160, shape=4.5, rate=.2), col="gray")
# no lines(dgamma(1:160, shape=4.5, rate=.01), col="gray")
# no (but less) lines(dgamma(1:160, shape=4.5, rate=.02), col="gray")




plot(density(rnorm(10000, mean=4.5, sd=4)))
plot(density(rnorm(100000, mean=0.7, sd=.02)))








############################################# 
#####        THE OTHER ROUTES           #####
#############################################
sum(fulldat[6, c(5, 6, 7, 10)], na.rm=TRUE)


















##########################################
###  both in a plot                    ###
##########################################

mu.samples %>% density %>% plot(main="travel time to work (posteriors)",
                                ylab="", xlab="minutes", ylim=c(0, .05))
for(j in 1:50){
  lines(dgamma(1:150, 
               shape=results.matrix[j,1], 
               rate=results.matrix[j,2]), 
        col="gray")
}
lines(density(mu.samples), lwd=2)

mlower <- mean(theintervals$lower)
mupper <- mean(theintervals$upper)
lines(c(mlower, mupper), c(.002, .002), col="red", lwd=3)
add.cis(mu.samples, height=.0004, length=0, show=TRUE, lwid=3)

legend("topright", legend=c("posterior with normal likelihood",
                            "posteriors with gamma likelihood",
                            "95% credible interval of posterior (normal)",
                            "95% credible interval of posterior (gamma)"),
       col=c("black", "gray", "blue", "red"), pch=19)





# NOTE:
# coda::HPDinterval(coda::as.mcmc(m_jags_sims$b)))
