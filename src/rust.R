source("rust_functions.R")
library(gtools)
library(xtable)
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
beta <- .9999

# Discretise the data set
temp <- lapply(dat, discretise_data)
# Getting the probabilities
theta3_within <- lapply(temp, function(x) probs(x[[3]]))
# These estimates match Table V of Rust (1987)
theta3_within <- do.call(cbind, theta3_within)

# Storing in the estimates
colnames(theta3_within) <- c("1972 GMC (Group 8)", "1974 GMC (Group 6)",
                             "1972 GMC (Group 7)", "1974 GMC (Group 5)",
                             "1975 GMC (Group 4)", "D309",
                             "1983 Grumman (Group 1)",
                             "1981 Chance (Group 2)", "1979 GMC (Group 3)")

theta3_within$parameters <- c("$\\theta_{31}$", NA, "$\\theta_{32}$", NA, 
                            "$\\theta_{33}$", NA)
bn <- ncol(theta3_within)
theta3_within <- theta3_within[, c(bn, 7,8,9,5,4,2,3,1)]
colnames(theta3_within)[1] <- "Parameters"

sink(file="../doc/tables/within_est.gen")
print(xtable(theta3_within, align="llrrrrrrrr"),
      sanitize.text.function=function(x){x},
      include.rownames=FALSE, floating=FALSE)
sink()

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

# Getting the transition probabilities
P <- lapply(theta3_between, function(x){
    y <- as.numeric(x[c(1,3,5),])
    P <- transition_probabilities(y)
    return(P)
})

# Storing the estimates of \theta_3 (Between group)
theta3_between <- do.call(cbind, theta3_between)
colnames(theta3_between) <- c("Group 1,2,3", "Group 1,2,3,4", "Group 4,5",
                              "Group 6,7", "Group 6,7,8", "Group 5,6,7,8",
                              "Full Sample")
theta3_between$parameters <- c("$\\theta_{31}$", NA, "$\\theta_{32}$", NA, 
                            "$\\theta_{33}$", NA)
bn <- ncol(theta3_between)
theta3_between <- theta3_between[, c(bn, 1:(bn-1))]
colnames(theta3_between)[1] <- "Parameters"

sink(file="../doc/tables/between_est.gen")
print(xtable(theta3_between, align="llrrrrrrr"),
      sanitize.text.function=function(x){x},
      include.rownames=FALSE, floating=FALSE)
sink()

dat.1234 <- for_rust_estimation(temp[c(7:9,5)])


## Full likelihood
## This is the same as l^f in Rust's notation
fullLL <- function(dat, params){
    theta <- params[1]
    RC <- params[2]
    pll <- partialLL(dat, theta, RC, P)
    return(pll)
}

## Running the optimisation routine for Groups 1,2,3,4
P <- P[[2]]
system.time(rustEst <- optim(c(1,4), fullLL, dat=dat.1234))

dat.123 <- for_rust_estimation(temp[7:9])
## Running the optimisation routine for Groups 1,2,3
system.time(rustEst1 <- optim(c(1,4), fullLL, dat=dat.123))


