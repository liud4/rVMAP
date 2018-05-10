# 05 April 2016, JN
# Added a new function for -7777
# Functions to deal with missing vals
setmiss   <- function(vec, val) ifelse(vec == val, NA, vec)
minus99   <- function(x) setmiss(x, -99)
minus9999 <- function(x) setmiss(x, -9999)
minus2222 <- function(x) setmiss(x, -2222)
zeroNA    <- function(x) setmiss(x, 0) 
minus8888 <- function(x) setmiss(x, -8888)
minus7777 <- function(x) setmiss(x, -7777)
