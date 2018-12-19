# Loading in the required libraries
library(foreign)
# Reading in the data sets
# Price
price <- read.dta("../data/prices_for_exam.dta")
# Revenues
dat <- read.dta("../data/predicted_revenue_for_exam.dta")
# Getting rid of the Stata attributes
dat <- data.frame(dat)
# Making the origin country more readable
# 1 - France, 2 - Germany, 3 - Spain, 4 - UK, 5 - US
dat$ono <- factor(dat$ono, levels=sort(unique(dat$ono)),
                  labels=c("France", "Germany", "Spain", "UK", "US"))

# Generating some basic summary statistics 
# For each country, in each year, how many movies were exported to the other countries?
dat.cy <- split(dat, dat$ono)
tab <- do.call(rbind, lapply(dat.cy, function(x){
    k <- x[, paste0("s", 1:5)]
    export <- nrow(k)-colSums(is.na(k))
    names(export) <- levels(dat$ono)
    return(export)
}))

# Cleaning the data set
# There are movies which originated from a country but do not have revenue data in the origin country. There are 643 of these movies. Removing these
movie_clean <- sum(table(dat$ono)-diag(tab)) # 643 of such movies
# Removing them
newdat <- do.call(rbind, lapply(dat.cy, function(x){
    country <- unique(as.numeric(x$ono))
    x <- x[!is.na(x[, paste0("s", country)]), ]
    return(x)
}))
rownames(newdat) <- NULL
# newdat should have 5500-643 = 4857 rows

# Merging the movie data set with the price data set
price[which(price$countryname=="United Kingdom"), "countryname"] <- "UK"
price[which(price$countryname=="United States"), "countryname"] <- "US"
# Creating a unique identifier for the two data sets
price$country_time <- paste(price$countryname, price$year, sep="-")
newdat$country_time <- paste(newdat$ono, newdat$year, sep="-")
# Merging the data sets with the unique identifier
temp <- merge(newdat, price[, 3:4], by="country_time", all.x=TRUE,
              all.y=FALSE)

cols <- paste0("d", 1:5)
temp[, cols] <- NA
## Creating the variable (decision to export) d_ijt
temp$d1 <- ifelse(!is.na(temp$s1), 1, 0)
temp$d2 <- ifelse(!is.na(temp$s2), 1, 0)
temp$d3 <- ifelse(!is.na(temp$s3), 1, 0)
temp$d4 <- ifelse(!is.na(temp$s4), 1, 0)
temp$d5 <- ifelse(!is.na(temp$s5), 1, 0)

## Function to store moment inequalities
moment_inequalities <- function(dij, rij, eta, params){
    gamma0 <- params[1]
    gamma1 <- params[2]
    CDF <- pnorm(gamma1^(-1)*(eta^-1*rij-gamma0))
    PDF <- dnorm(gamma1^-1*(eta^-1*rij-gamma0))
    # Moment Inequality 1
    mom1 <- -(1-dij)*(eta^(-1)*rij-gamma0)+
        dij*gamma1*(PDF/CDF)
    # Moment Inequality 2
    mom2 <- dij*(eta^-1*rij-gamma0)+(1-dij)*gamma1*(PDF/(1-CDF))
    return(list(mom1=mom1, mom2=mom2))
}

## Function for perfect foresight likelihood
pforesight_LL <- function(dij, rij, params){
    gamma0 <- params[1]
    gamma1 <- params[2]
    CDF <- pnorm(gamma1^(-1)*(eta^-1*rij-gamma0))
    LL <- dij*log(CDF)+(1-dij)*log(1-CDF)
    return(-sum(LL)) 
}

## Estimating the log likelihood
perfectForesight <- optim(par=c(), fn=pforesight_LL, method="Nelder-Mead",
      hessian=TRUE, dij=, rij=)

## # ColMeans
## colMeans(newdat[, c(4,6,8,10,12)], na.rm=TRUE)
