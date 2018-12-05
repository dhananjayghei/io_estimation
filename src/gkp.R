# Sourcing the functions file
source("functions.R")
# Loading in the required libraries
library(dplyr)
library(xtable)
library(stargazer)
library(AER)

# Reading in the BLP data
dat <- read.table("../data/cardata.txt", header=TRUE)

# Finding the sum of market shares
dat <- dat %>%
    group_by(year) %>%
    mutate(sum_shares = sum(market_share)) %>%
    ungroup() %>% data.frame()
# Outside share
dat$outside_shares <- 1-dat$sum_shares
# Getting the diff log to estimate the linearized version of the logit demand
dat$diff_shares <- log(dat$market_share)-log(dat$outside_shares)
dat$cons <- rep(1, nrow(dat))

# Renaming the column names because they are too long
colnames(dat)[3:6] <- c("hpwt", "space", "air", "mpd")

# Constructing the own firm product IV
dat <- dat %>%
    group_by(firmid, year) %>%
    mutate(own_firm_hpIV = sum(hpwt)-hpwt,
           own_firm_spaceIV = sum(space) - space,
           own_firm_airIV = sum(air) - air,
           own_firm_mpdIV = sum(mpd) - mpd,
           own_firm_consIV = sum(cons) - cons) %>%
    ungroup() %>% data.frame()
# Constructing the rival firm product IV
dat.yr <- split(dat, dat$year)

dat <- do.call(rbind, lapply(dat.yr, function(x){
    firmid <- unique(x$firmid)
    # Split by firm id
    sum_vars <- do.call(rbind, lapply(firmid, function(y){
        prod_hp <- sum(x[which(x$firmid != y), "hpwt"])
        prod_ac  <- sum(x[which(x$firmid != y), "air"])
        prod_mpd  <- sum(x[which(x$firmid != y), "mpd"])
        prod_size <- sum(x[which(x$firmid != y), "space"])
        prod_cons  <- sum(x[which(x$firmid != y), "cons"])
        dat <- cbind(prod_hp, prod_ac, prod_mpd, prod_size, prod_cons)
        return(dat)
    }))
    sum_vars <- data.frame(sum_vars)
    sum_vars$firmid <- firmid
    x$rival_firm_hpIV <- sum_vars[match(x$firmid, sum_vars$firmid), "prod_hp"]
    x$rival_firm_airIV <- sum_vars[match(x$firmid, sum_vars$firmid), "prod_ac"]
    x$rival_firm_mpdIV <- sum_vars[match(x$firmid, sum_vars$firmid), "prod_mpd"]
    x$rival_firm_spaceIV <- sum_vars[match(x$firmid, sum_vars$firmid), "prod_size"]
    x$rival_firm_consIV <- sum_vars[match(x$firmid, sum_vars$firmid), "prod_cons"]
    return(x)
}))
rownames(dat) <- NULL

# Replicate Column I and II from GKP
# No Correction (OLS)
columnI <- lm(diff_shares~hpwt+air+mpd+space+price, data=dat)
# 2sls (No intercations)
columnII <- ivreg(diff_shares~hpwt+air+mpd+space+price | hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat)




# Try to get the wrong instruments (one last time)
temp <- dat %>%
    group_by(firmid, year) %>%
    mutate(no_firms = n(),
           incorrect_hpIV = sum(no_firms*hpwt),
           incorrect_airIV = sum(no_firms*air),
           incorrect_spaceIV = sum(no_firms*space),
           incorrect_mpdIV = sum(no_firms*mpd),
           incorrect_consIV = sum(no_firms*cons)) %>%
    ungroup() %>% data.frame()

newIV <- ivreg(diff_shares~hpwt+air+mpd+space+price | hpwt+air+mpd+space+incorrect_hpIV+
                      incorrect_airIV+incorrect_mpdIV+incorrect_spaceIV+incorrect_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=temp)

# Conditional Moment Restrictions
# Getting the first control
# Regress prices on the instruments and get estimates for \xi

dat$xi <- resid(lm(price~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

# Get the other two refinements for the residuals
dat <- dat %>%
    group_by(firmid, year) %>%
    mutate(own.xi = sum(xi)-xi) %>%
    ungroup() %>% data.frame()

# Constructing the rival xi
dat.yr <- split(dat, dat$year)
dat <- do.call(rbind, lapply(dat.yr, function(x){
    firmid <- unique(x$firmid)
    # Split by firm id
    sum_vars <- do.call(rbind, lapply(firmid, function(y){
        prod_xi <- sum(x[which(x$firmid != y), "xi"])
        return(prod_xi)
    }))
    sum_vars <- data.frame(sum_vars)
    colnames(sum_vars) <- "prod_xi"
    sum_vars$firmid <- firmid
    x$rival.xi <- sum_vars[match(x$firmid, sum_vars$firmid), "prod_xi"]
    return(x)
}))
rownames(dat) <- NULL

# Setting up higher order terms
dat$xi.square <- dat$xi^2
dat$xi.cube <- dat$xi^3
dat$own.xi.square <- dat$own.xi^2
dat$own.xi.cube <- dat$own.xi^3
dat$rival.xi.square <- dat$rival.xi^2
dat$rival.xi.cube <- dat$rival.xi^3

# Getting the controls
dat$V2 <- resid(lm(xi.square~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

dat$V3 <- resid(lm(xi.cube~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

dat$V5 <- resid(lm(own.xi.square~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

dat$V6 <- resid(lm(own.xi.cube~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

dat$V8 <- resid(lm(rival.xi.square~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

dat$V9 <- resid(lm(rival.xi.cube~hpwt+air+mpd+space+own_firm_hpIV+
                      own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+
                      rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+
                      rival_firm_consIV, data=dat))

colnames(dat)[24:26]  <- c("V1", "V4", "V7")


# CMRCF
columnIII <- lm(diff_shares~hpwt*V1+air*V1+mpd*V1+space*V1+V1+V2+V3+V4+V5+V6+V7+V8+V9, data=dat)
























## # Write a function to construct the instruments
## # You need market and firm specific variables to identify the data
## # Need product characteristics to construct the instruments
## getIV <- function(dat, market.id, firm.id, variables){
##     # Construct the sum of own firm product characteristics (excluding the product)
##     dat.yr <- split(dat, dat[, market.id])
##     own_iv <- do.call(rbind, lapply(dat.yr, function(x){
##         x.firm <- split(x, x[, firm.id])
##         k <- do.call(rbind, lapply(x.firm, function(y){
##             ownIV <- do.call(cbind, lapply(variables, function(z){
##                 iv <- sum(y[, z])-y[,z]
##                 return(iv)
##             }))
##             ownIV <- data.frame(ownIV)
##             colnames(ownIV) <- var.name <- paste0("own_firm_", variables, "_iv")
##             y <- data.frame(cbind(y,ownIV))
##             return(y)
##         }))
##         k <- data.frame(k)
##         rownames(k) <- NULL
##         return(k)
##     }))
##     own_iv <- data.frame(own_iv)
##     rownames(own_iv) <- NULL
## #    return(l)
##     # Construct the rival firm characteristics as instruments
    
## }

























