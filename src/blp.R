# Loading in the required libraries
library(dplyr) # For data wrangling
library(AER) # For IV regression
# library(BLPestimatoR)
# Reading in the BLP data
dat <- read.table("../data/cardata.txt", header=T)

# Finding the sum of market shares
temp <- dat %>%
    group_by(year) %>%
    mutate(sum_shares = sum(market_share)) %>%
    ungroup() %>% data.frame()
# Outside share
temp$outside_shares <- 1-temp$sum_shares
# Getting the diff log to estimate the linearized version of the logit demand
temp$diff_shares <- log(temp$market_share)-log(temp$outside_shares)

# -----------------------Question 3 
# Part 1. Estimate the linearised version of the logit demand 
# Note that the results of this regression matches exactly
# with Table III, Column I on Page 873 of BLP (1995) paper
linear_demand <- glm(diff_shares~horsepower_weight+ac_standard+
                         miles_per_dollar+length_width+price, data=temp)
# Part 2. Calculating the price elasticities 
# Getting the 5% confidence interval estimate for price
coeffs_linear_demand <- data.frame(confint(linear_demand))
price_coeff  <- cbind(coef(linear_demand)["price"],
                      coeffs_linear_demand["price",])
colnames(price_coeff) <- c("Coefficient", "2.5%", "97.5%")

cars <- c("HDACCO", "FDESCO", "VWJETT", "VWPASS")

dat.cars <- temp[which(temp$vehicle_name %in% cars & temp$year==90), ]

# Get own price and cross price elasticities
## dat.cars.cal <- split(dat.cars, dat.cars$vehicle_name)
## lapply(dat.cars.cal, function(x){
##     ope <- -price_coeff[, "Coefficient"]*x[,"price"]*(1-x[,"market_share"])
##     cpe <- 
## })
## sapply(dat.cars, function(x){
##     ope <- -price_coeff[, "Coefficient"]*x[,"price"]*(1-x[,"market_share"])
##     return(ope)
## })

# Honda Accord (1990) "HDACCO"
hdacco90 <- temp[which(temp$vehicle_name=="HDACCO" & temp$year==90), ]
# The own price elasticity (-alpha * p_j * (1-s_j)
# with 95% confidence interval
hdacco90_ope <- apply(price_coeff, 2, function(x){
    -x*hdacco90$price*(1-hdacco90$market_share)
})
# Cross-price elasticity w.r.t. Ford Escort (1990) "FDESCO"
# The cross price elasticity (alpha * s_j * s_k)
# with 95% confidence interval
fdesco90 <- temp[which(temp$vehicle_name=="FDESCO" & temp$year==90), ]
hdacco90_cpe <- apply(price_coeff, 2, function(x){
    x * hdacco90$market_share * fdesco90$market_share
})

# -----------------------Question 4 
# Part 1. Construct the instruments 
# Market is defined as firmid and year
temp$cons <- rep(1, nrow(temp))

# These are the correct instruments as stated in BLP.
# As a test, they match the BLP data from the hdm package.
# Constructing the instrument as sum of product characteristics
# for all models marketed by a single firm
# in a given market excluding the product. Instruments from own-firm characteristics
correct_blp <- temp %>%
    group_by(firmid, year) %>%
    mutate(own_firm_hpIV = sum(horsepower_weight)-horsepower_weight,
           own_firm_spaceIV = sum(length_width) - length_width,
           own_firm_airIV = sum(ac_standard) - ac_standard,
           own_firm_mpdIV = sum(miles_per_dollar) - miles_per_dollar,
           own_firm_consIV = sum(cons) - cons) %>%
    ungroup() %>% data.frame()

# Instruments from rival-firm characteristics
# Constructing the instrument as sum of product characteristics
# for all models of the rival firms in the given market
# Split by year
correct_blp.yr <- split(correct_blp, correct_blp$year)

correct_blp <- do.call(rbind, lapply(correct_blp.yr, function(x){
    firmid <- unique(x$firmid)
    # Split by firm id
    sum_vars <- do.call(rbind, lapply(firmid, function(y){
        prod_hp <- sum(x[which(x$firmid != y), "horsepower_weight"])
        prod_ac  <- sum(x[which(x$firmid != y), "ac_standard"])
        prod_mpd  <- sum(x[which(x$firmid != y), "miles_per_dollar"])
        prod_size <- sum(x[which(x$firmid != y), "length_width"])
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
rownames(correct_blp) <- NULL

# IV Regression
# These results are based on the correct instruments as defined in BLP.
# The code in BLP has mistakes.
# These results match Page 24 Column 2 of Gandhi, Kim and Petrin (NBER WP-2011)
blpIV <- ivreg(diff_shares~horsepower_weight+ac_standard+miles_per_dollar+length_width+price | horsepower_weight+ac_standard+miles_per_dollar+length_width+own_firm_hpIV+own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+rival_firm_consIV, data=correct_blp)



# Function to get own and cross price elasticites of cars
# Also, generate LaTeX tables
# Inputs:
# obj - object of type "lm", "ivreg"
# cars - vector, contains vehicle names coded as in the data set
# data - BLP data set
# year - numeric, contains the year for which we calculate the elasticity
calc_elasticity <- function(obj, cars, data, year){
    dat <- data[which(data$vehicle_name %in% cars & data$year == year), ]
    dat$vehicle_name <- as.character(dat$vehicle_name)
    dat <- dat[, c("vehicle_name", "year", "market_share", "price")]
    # Getting the price estimates from the regression object
    price_est <- coef(obj)["price"]
    # Calculate own price elasticites
    dat.vnm  <- split(dat, dat$vehicle_name)
    own_price <- do.call(rbind, lapply(dat.vnm, function(x){
        ope <- round(-price_est* x$price *(1-x$market_share),digits=5)
        return(ope)
    }))
    # Calculate cross price elasticities
    cross_price <- lapply(dat.vnm, function(x){
        k <- dat.vnm[which(names(dat.vnm) != x$vehicle_name)]
        cpe <- do.call(rbind, lapply(k, function(y){
            cpe <- round(-price_est*x$price*y$market_share, digits=5)
            return(cpe)
        }))
        return(cpe)
    })
    return(list(own_price=own_price, cross_price=cross_price))
})




## Trying the incorrect version of BLP
## Doesn't work
## pp <- temp %>%
##     add_count(firmid, year) %>%
##     group_by(firmid, year) %>%
##     mutate(sum_horsepower_mkt = n*(sum(horsepower_weight)-horsepower_weight),
##            sum_size_mkt = n*(sum(length_width) - length_width),
##            sum_ac_mkt = n*(sum(ac_standard) - ac_standard),
##            sum_mpd_mkt = n*(sum(miles_per_dollar) - miles_per_dollar),
##            sum_price_mkt = n*(sum(price)-price),
##            sum_cons_mkt = n*(sum(cons)-cons)) %>%
##     ungroup() %>% data.frame()
## names(pp)[13] <- "count_firm_year"
