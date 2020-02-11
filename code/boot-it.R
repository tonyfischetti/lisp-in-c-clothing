
rm(list=ls())

library(data.table)
library(magrittr)
library(dplyr)
library(lubridate)



dat <- fread("./travel-stats.csv")[,thedate:=ymd(thedate)][
                                   ,total_time:=minutes_to_train + 
                                                waiting_for_train +
                                                train_ride +
                                                to_library]
fulldat <- dat

dat <- dat[method=="c_163",]


library(parallel)
library(boot)


dasboot.mean <- boot(dat[,total_time],
                     function(x, i){mean(x[i])}, 
                     1000000,
                     parallel="multicore",
                     ncpus=4)


dasboot.median <- boot(dat[,total_time],
                       function(x, i){median(x[i])}, 
                       1000000,
                       parallel="multicore",
                       ncpus=4)


dasboot.mean$t %>% {.[,1]} %>% hist(main="bootstrap of total travel time means")
bac.ci.mean <- boot.ci(dasboot.mean, type="bca")
lines(c(45.8, 59), c(1000, 1000), lwd=3, col="blue")


dasboot.median$t %>% {.[,1]} %>% hist(main="bootstrap of total travel time medians")
bac.ci.median <- boot.ci(dasboot.median, type="bca")
lines(c(42.5, 50.5), c(1000, 1000), lwd=3, col="blue")



base::sample(dat[,total_time], 1000000, replace=TRUE) -> dasamples
dasamples %>% density(bw=10) %>% plot

