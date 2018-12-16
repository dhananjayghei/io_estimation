source("rust_functions.R")
library(plyr)
# Reading in the Rust data from the zip file
dat <- read.zip("../data/rust-data.zip")
# The files are read in alphabetical order
# The dimensions of these files are as given below
Rows <- c(137*18, 137*10, 137*18, 137*12, 128*37, 110*4, 36*15, 60*4, 81*48)
names(Rows) <- names(dat)
# Some rows have weird codes at the end. Getting rid of these
dat <- checkRows(dat, Rows)
# Constructing the data set in the right dimension
dat <- createRust(dat)

# The first 11 rows in all the data sets are in the following order
## 1. Bus Number
## 2. Month Purchased
## 3. Year Purchased
## 4. Month of 1st engine replacement
## 5. Year of 1st engine replacement
## 6. Odometer at replacement
## 7. Month of 2nd engine replacement
## 8. Year of 2nd engine replacement
## 9. Odometer at replacement
## 10. Month Odometer data begins
## 11. Year odometer data begins
dim.fp <- 90
omax <- 450000

## # Function to get input data set for estimation
## discretise_data <- function(dat, dim.fp=90, omax=450000){
##     # Get odometer values at replacement
##     oval1 <- dat[6,]
##     oval2 <- dat[9,]
##     # Store in the dimensions
##     nr <- dim(dat)[1]
##     nc <- dim(dat)[2]
##     # Getting the odometer reading stopping points (for replacement)
##     dtc <- (dat[12:nr,] >= oval1)*(oval1>0)+(dat[12:nr,] >= oval2)*(oval2>0)
##     dtx <- (dat[12:nr,] + oval1*dtc*(dtc-2) - .5*oval2*dtc*(dtc-1))
##     dtx <- ceiling(dim.fp*dtx/omax)
##     dtc <- rbind(dtc[2:(nr-11),]-dtc[1:(nr-12),], rep(0,nc))
##     mil <- dat[13:nr,]-dat[12:(nr-1),]
##     mil <- (dtx[2:(nr-11),]-dtx[1:(nr-12),])+
##         dtx[1:(nr-12),]*dtc[1:(nr-12),]
##     return(list(dtc=dtc, dtx=dtx, mil=mil))
## }

# Discretise the data set
temp <- lapply(dat, discretise_data)
# Getting the probabilities
theta3_within <- lapply(temp, function(x) probs(x[[3]]))

# Storing in the estimates
# These estimates match Table V of Rust (1987)
theta3_withEst <- do.call(rbind, lapply(theta, function(x){
    est <- x
    est[2,] <- paste("(", est[2,], ")", sep="")
    return(est)
}))
#theta3_est <- data.frame(theta3_est)
colnames(theta3_withEst) <- c("$\\theta_{31}$", "$\\theta_{32}$",
                          "$\\theta_{33}$")

# Between Group Estimates of Mileage Process
# Construct the groups
# 1 - g870, 2-rt50, 3-t8h203, 4-a530875
# 5-a530874, 6-a452374, 7-a530872, 8-a452372
# Group 1,2,3
group.123 <- do.call(smartbind, lapply(temp[7:9], function(x){
    return(data.frame(t(x[[3]])))
}))
group.123 <- t(group.123)
# Group 1,2,3,4
group.1234 <- do.call(smartbind, lapply(temp[c(7:9,5)], function(x){
    return(data.frame(t(x[[3]])))
}))
group.1234 <- t(group.1234)
# Group 4,5
group.45 <- do.call(smartbind, lapply(temp[c(5,4)], function(x){
    return(data.frame(t(x[[3]])))
}))
group.45 <- t(group.45)
# Group 6,7
group.67 <- do.call(smartbind, lapply(temp[c(2,3)], function(x){
    return(data.frame(t(x[[3]])))
}))
group.67 <- t(group.67)
# Group 6,7,8
group.678 <- do.call(smartbind, lapply(temp[c(2,3,1)], function(x){
    return(data.frame(t(x[[3]])))
}))
group.678 <- t(group.678)
# Group 5,6,7,8
group.5678 <- do.call(smartbind, lapply(temp[c(2,3,1,4)], function(x){
    return(data.frame(t(x[[3]])))
}))
group.5678 <- t(group.5678)
# Full Sample
full <- do.call(smartbind, lapply(temp, function(x){
    return(data.frame(t(x[[3]])))
}))
rownames(full) <- NULL
# Transposing it back to get the full 
full <- t(full)

## Getting the probability for between group estimators
groups <- list(group.123, group.1234, group.45, group.67, group.678,
               group.5678, full)

theta3_between <- lapply(groups, probs)

theta3_bwEst <- do.call(rbind, lapply(theta3_between, function(x){
    est <- x
    est[2,] <- paste("(", est[2,], ")", sep="")
    return(est)
}))
colnames(theta3_bwEst) <- c("$\\theta_{31}$", "$\\theta_{32}$",
                            "$\\theta_{33}$")

# Getting the transition probabilities
P <- lapply(theta, function(x){
    y <- x[1,]
    P <- transition_probabilities(y)
    return(P)
})













