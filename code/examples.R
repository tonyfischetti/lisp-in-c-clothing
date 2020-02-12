#!/usr/local/bin//Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(datatable.prettyprint.char=50)
options(width = 80)

args <- commandArgs(trailingOnly=TRUE)

library(colorout)
library(data.table)
library(magrittr)
library(stringr)

source("~/.rix/tony-utils.R")
# ------------------------------ #


our.vect <- c(8, 6, 7, 5, 3, 0, 9)

c(8, 6, 7, "-", 5, 3, 0, 9)

our.vect[length(our.vect)]

our.vect + 1
our.vect ^ 2
our.vect * our.vect
our.vect ^ our.vect

num <- 10^250
num^2

1/3

(expt 10 250)
(expt (expt 10 250) 2)

mean(our.vect)

# One of the most powerful ideas
# in R is that you can use vectors
# to subset other vectors

our.vect[1:5]

our.vect > 5

our.vect[our.vect > 5]

sum(our.vect > 5)

messy.vector <- c(8, 6, NA, 7, 5, NA, 3, 0, 9)

messy.vector[!is.na(messy.vector)]

messy.vector[is.na(messy.vector)] <- 0


# --------------------------------------------------------------- #

# matrices

a.matrix <- matrix(c(1, 2, 3, 4, 5, 6), ncol=2)

dim(a.matrix)

dim(a.matrix) <- c(2, 3)

attributes(a.matrix)

a.matrix <- matrix(c(1, 2, 3, 4, 5, 6), ncol=2)

a2.matrix <- cbind(c(1, 2, 3), c(4, 5, 6))

a3.matrix <- rbind(c(1, 2, 3), c(4, 5, 6))


a2.matrix %*% a3.matrix

a2.matrix %*% a2.matrix

a2.matrix %*% t(a2.matrix)

a2.matrix[2,1]

a2.matrix[c(1, 3), 2]



# --------------------------------------------------------------- #

# mention packages
library(ggplot2)

# --------------------------------------------------------------- #



mtcars.copy <- mtcars

mtcars.copy$transmission <- ifelse(mtcars$am==0, "auto", "manual")

mtcars.copy$transmission <- factor(mtcars.copy$transmission)

qplot(transmission, mpg, data=mtcars.copy,
  geom="boxplot", fill=transmission) +
  guides(fill=FALSE)


# note the labels



automatic.mpgs <- mtcars$mpg[mtcars$am==0]
manual.mpgs <- mtcars$mpg[mtcars$am==1]
t.test(automatic.mpgs, manual.mpgs, alternative="less")

t.test(mpg ~ am, data=mtcars, alternative="less")


plot(mpg ~ wt, data=mtcars)

model <- lm(mpg ~ wt, data=mtcars)

abline(model)

summary(model)





# --------------------------------------------------------------- #

# meta things


a <- 10

get("a")
assign("b", 20)

foo <- function(quux) quux^2

get("foo")
foo(2)

environment(foo)
attr(foo, "this") <- "that"
attributes(foo)

formals(foo)
body(foo)
body(foo)[1]
body(foo)[2]
body(foo)[3]

`^`(2, 5)
`+`(2, 5)

`(`

`(` <- function(e1) print("this probably shouldn't be allowed")

("hi")
rm("(")


body(foo)[3]
body(foo)[3] <- 3

foo(2)


body(foo)
eval(body(foo))
eval(body(foo), env=list(quux=2))



# --------------------------------------------------------------- #


debug.these <- function(...){
  cl <- match.call()
  lofdots <- as.list(cl[-1])
  devnull <- lapply(lofdots,
                    function(x){
                      cat(x)
                      cat("\t->\t")
                      cat(eval(x))
                      cat("\n")
                    })
}

debug.these(a, b)


square.it <- function(something) something*something

`%->%` <- function(e1, e2){
  cl <- match.call()
  print(cl)
  e2(e1)
}

5 %->% square.it

5 %->% `*`(5)

mult.it <- function(s1, s2) s1 * s2

5 %->% mult.it(5)

mult.it <- function(s1){ function(s2) s2*s1 }

5 %->% mult.it(5)



`%->%` <- function(e1, e2) {
  cl <- match.call()
  if (length(cl[[3]]) == 1) {
    e2(e1)
  }
  else {
    e  <- do.call(substitute, list(cl[[3]], list(LEFT = cl[[2]])))
    eval(e, parent.frame(), parent.frame())
  }
}

5 %->% square.it

5 %->% mult.it(5, LEFT)

5 %->% mult.it(5, .)

5 %->% mult.it(5)


`%->%` <- function(e1, e2) {
  cl <- match.call()
  if (length(cl[[3]]) == 1) {
    e2(e1)
  }
  else {
    e  <- do.call(substitute, list(cl[[3]], list(. = cl[[2]])))
    if (e != cl[[3]]){
      return(eval(e, parent.frame(), parent.frame()))
    }
    j <- length(e)
    while (j > 1) {
      e[[j + 1]] <- e[[j]]
      j <- j - 1
    }
    e[[2]] <- as.symbol(".")
    e <- do.call(substitute, list(e, list(. = cl[[2]])))
    return(eval(e, parent.frame(), parent.frame()))
  }
}


5 %->% mult.it(5)




# https://tonyfischetti.shinyapps.io/InteractiveLogisticRegression/

