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


# Storing the regression results for Column I and II
sink(file="../doc/tables/gkp_ols.gen")
stargazer(columnI, columnII, type="latex",
          dep.var.labels="",
          model.names=FALSE,
          column.labels=c("OLS", "2SLS"),
          covariate.labels=c("Constant", "HP/Weight", "Air", "MPD", "Size", "Price"),
          intercept.bottom=FALSE, float=FALSE)
sink()




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

# Adding the income means
incomeMeans <- c(2.01156, 2.06526, 2.07843, 2.05775, 2.02915, 2.05346,
                 2.06745, 2.09805, 2.10404, 2.07208, 2.06019, 2.06561,
                 2.07672, 2.10437, 2.12608, 2.16426, 2.18071, 2.18856,
                 2.21250, 2.18377)
dat$income <- rep(incomeMeans, table(dat$year))
dat$y_p <- dat$income-dat$price
dat$YP <- exp(dat$income)-dat$price

# CMRCF
# Tried doing it with y_p as well. The results don't change much.
columnIII <- nls(diff_shares~c*cons+beta[1]*hpwt+beta[2]*air+beta[3]*mpd+beta[4]*space+
              alpha*price+(pi[1]*V1+pi[2]*V2+pi[3]*V3+pi[4]*V4+pi[5]*V5+pi[6]*V6+
                           pi[7]*V7+pi[8]*V8+pi[9]*V9)*(1+gamma[1]*hpwt+gamma[2]*air+gamma[3]*mpd+
                                                        gamma[4]*space+gammaP*YP), data=dat,
          start=list(c=-10, beta=c(2,1,.1,2), alpha=-.23,
                     pi=c(3,-1,.04,-.7,.1,1.1,-.08,.3,-.1), gamma=c(.9,.4,-.04,.2), gammaP=-.02),
          control=list(maxiter=1000, tol=1e-06))


ss <- summary(columnIII)
ssC <- ss$coefficients
colnames(ssC) <- c("coef", "se", "t", "p")
ssC <- as.data.frame(ssC)
namesCoef <- c("Constant", "HP/Weight", "Air", "MPD", "Size", "Price", paste0("V", 1:9), "gamma1",
               "gamma2", "gamma3", "gamma4", "gammaP")
nCoef <- ssC$coef ; names(nCoef) <- namesCoef
nSE <- ssC$se ; names(nSE) <- namesCoef
nT <- ssC$t; names(nT) <- namesCoef
nP <- ssC$p; names(nP) <- namesCoef

nlsReg <- matrix(nrow=2*length(namesCoef), ncol=2)
for(i in 2:((nrow(nlsReg)+2)/2)){
    nlsReg[(2*i-3), 2] <- as.numeric(round(nCoef[i-1], 5))
    nlsReg[(2*i-2), 2] <- paste("(", round(nSE[i-1], 5), ")", sep="")
    nlsReg[2*i-3, 1] <- namesCoef[i-1]
}
colnames(nlsReg) <- c("Variable", "Estimate")
nlsReg <- data.frame(nlsReg)

# Printing manual stars
## kk <- lm(diff_shares~hpwt+air+mpd+space+price+(V1+V2+V3+V4+V5+V6+V7+V8+V9)*(1+hpwt+air+space+mpd+YP), data=dat)

## for(i in 1:nrow(ssC)){
##     if(ssC[i, ]$p <= .001){
##         print(paste(as.numeric(round(ssC[i, "coef"], 5)), "***", sep=""))
##     }else(.001 < ssC[i, ]$p <= 0.05){
##         print(paste(as.numeric(round(ssC[i, "coef"], 5)), "**", sep=""))
##     }else(.005 < ssc[i, ]$p <= .01){
##         print(paste(as.numeric(round(ssC[i, "coef"], 5)), "*", sep=""))
##     }else{
##         print(round(ssC[i, "coef"], 5))
##     }
## }

# Getting the elasticities
dat90 <- dat[which(dat$year==90), ]

# For OLS regression (No interactions)
# Full data set
ols.elas <- coef(columnI)["price"]*dat$price*(1-dat$market_share)
ols <- sum_elas(ols.elas)
# For 1990 only
ols.elas90 <- coef(columnI)["price"]*dat90$price*(1-dat90$market_share)
ols90 <- sum_elas(ols.elas90)
# For IV regression (No interactions)
# Full data set
iv.elas <- coef(columnII)["price"]*dat$price*(1-dat$market_share)
iv <- sum_elas(iv.elas)
# For 1990 only
iv.elas90 <- coef(columnII)["price"]*dat90$price*(1-dat90$market_share)
iv90 <- sum_elas(iv.elas90)
# For CMRCF
cmrcf.elas <- coef(columnIII)["alpha"]*dat$price*(1-dat$market_share)
cmrcf <- sum_elas(cmrcf.elas)
# For 1990 only
cmrcf.elas90 <- coef(columnIII)["alpha"]*dat90$price*(1-dat90$market_share)
cmrcf90 <- sum_elas(cmrcf.elas90)


# BLP Table VI
cars <- c("MZ323", "HDACCO", "ACLEGE", "BW735i")
blp <- dat[which(dat$year==90 & dat$vehicle_name %in% cars), ]
blp.ols <- round(coef(columnI)["price"]*blp$price*(1-blp$market_share),2)
blp.iv <- round(coef(columnII)["price"]*blp$price*(1-blp$market_share),2)
blp.cmrcf <- round(coef(columnIII)["alpha"]*blp$price*(1-blp$market_share),2)

# Generating LaTeX table
# Results for 1971-1990
fullDat <- data.frame(cbind(ols, iv, cmrcf))
colnames(fullDat) <- c("OLS", "IV", "CMRCF")
# Elasticities from 1990
latestDat <- data.frame(cbind(ols90, iv90, cmrcf90))
colnames(latestDat) <- c("OLS", "IV", "CMRCF")
# 1990 Models (from BLP, Table  VI)
blpDat <- data.frame(cbind(blp.ols, blp.iv, blp.cmrcf))
cars <- data.frame(code=cars, name=c("Mazda 323", "Honda Accord", "Acura Legend", "BMW 735i"))
rownames(blpDat) <- cars[c(3,4,2,1), ]$name
colnames(blpDat) <- colnames(fullDat)
# Create an empty row
rows <- data.frame(OLS=NA, IV=NA, CMRCF=NA)
cols <- matrix(nrow=14, ncol=1)
tab2 <- data.frame(cbind(cols, rbind(fullDat, rows, latestDat, rows, blpDat)))
tab2$cols <- c(rownames(fullDat), "Elasticities from 1990", rownames(fullDat), "1990 Models (from BLP)",
               rownames(blpDat))
rownames(tab2) <- NULL
tab2[nrow(tab2)+1, ] <- c("Interactions", "No", "No", "Yes")
tab2 <- tab2[c(nrow(tab2), 1:nrow(tab2)-1), ]
colnames(tab2)[1] <- "Elasticities"

print(xtable(tab2, align="llrrr"), type="latex", include.rownames=FALSE, table.placement="tp",
      hline.after=c(-1,0,5,10, nrow(tab2)), floating=FALSE,
      file="../doc/tables/gkp_table2.gen")

# Write summary stats
sum_elas <- function(tab){
    N <- length(tab)
    # Calculating the mean, median, and sd
    tb <- rbind(median(tab), mean(tab), sd(tab))
    # Number of inelastic demands
    inelastic <- sum(as.numeric(abs(tab)<1))
    no_inelastic <- inelastic*100/N
    tb <- rbind(tb, no_inelastic)
    tb <- round(tb, 2)
    rownames(tb) <- c("Median", "Mean", "Standard Deviation",
                      "Percent of Inelastic Demands")
    tb <- data.frame(tb)
    return(tb)
}


















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

























