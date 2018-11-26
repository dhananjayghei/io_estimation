# Sourcing the functions file
source("functions.R")
# Loading in the required libraries
library(dplyr) # For data wrangling
library(xtable) # For LaTeX tables
library(stargazer) # For regression tables in LaTeX
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
temp$cons <- rep(1, nrow(temp))

# -----------------------Question 2
# Writing the log likelihood function
log_ll <- function(theta, y=y, X=X, Z=Z){
    beta  <- theta[1:6]
    probs <- exp(X %*% beta)/(1+sum(exp(X %*% beta)))
    logl <- sum(y*log(probs)+Z*log(1-probs))
    return(-logl)
}

# Constructing the variables
y <- temp$market_share
X  <- as.matrix(temp[, c("cons", "horsepower_weight", "ac_standard", "miles_per_dollar", "length_width", "price")])
Z <- temp$outside_shares
# Maximum likelihood estimation
mle.res <- optim(par=c(-10,-0.12,-0.03, 0.26,2.34,-0.088), fn=log_ll, method="Nelder-Mead", y=y, X=X, Z=Z, hessian=TRUE)

mle.res2 <- optim(par=rep(1,6), fn=log_ll, method="Nelder-Mead", y=y, X=X, Z=Z, hessian=TRUE)

# -----------------------Question 3 
# Replicating Table 1 (with the available variables in the homework)
# Descriptive statistics (Sales weighted mean)
table1 <- temp %>%
    group_by(year) %>%
    summarise(avg_price = sum(market_share*price)/sum(market_share),
              avg_hp = sum(market_share*horsepower_weight)/sum(market_share),
              avg_size = sum(market_share*length_width)/sum(market_share),
              avg_air = sum(market_share*ac_standard)/sum(market_share),
              avg_mpg = sum(market_share*miles_per_dollar)/sum(market_share)) %>%
    ungroup() %>% data.frame()
table1[,2:ncol(table1)] <- apply(table1[,2:ncol(table1)], 2, function(x) round(x, digits=3))
colnames(table1) <- c("Year", "Price", "HP/Wt", "Size", "Air", "MPD")
table1[, 1] <- paste("19", table1[, 1], sep="")
# Storing the LaTeX table (Replicating Table 1)
genxtable(xtable(table1, align="llrrrrr", digits=c(0,0,rep(3,5))), basename="blp_table1")

# Part 1. Estimate the linearised version of the logit demand 
# Note that the results of this regression matches exactly
# with Table III, Column I on Page 873 of BLP (1995) paper
linear_demand <- lm(diff_shares~horsepower_weight+ac_standard+
                         miles_per_dollar+length_width+price, data=temp)
# Part 2. Calculating the price elasticities 
# Selecting the cars
cars <- c("HDACCO", "FDESCO", "VWJETT", "VWPASS")
# Adding the car names
car.names <- data.frame(vehicle_name=cars, name=c("Honda Accord", "Ford Escort", "VW Jetta", "VW Passat"))
# Get elasticities
ols_elasticities <- calc_elasticity(obj=linear_demand, cars=cars, data=temp, year=90)
# Writing the car names for generating LaTeX tables
rownames(ols_elasticities) <- car.names[match(rownames(ols_elasticities), car.names$vehicle_name), "name"]
colnames(ols_elasticities) <- car.names[match(colnames(ols_elasticities), car.names$vehicle_name), "name"]
# Generating the LaTeX table
genxtable(xtable(ols_elasticities, align="lrrrr", digits=c(0,rep(5,4))), basename="ols_elast", include.rownames=TRUE)

# -----------------------Question 4 
# Part 1. Construct the instruments 
# Market is defined as firmid and year

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

# Get elasticities
iv_elasticities <- calc_elasticity(obj=blpIV, cars=cars, data=correct_blp, year=90)
# Writing the car names for generating LaTeX tables
rownames(iv_elasticities) <- car.names[match(rownames(iv_elasticities), car.names$vehicle_name), "name"]
colnames(iv_elasticities) <- car.names[match(colnames(iv_elasticities), car.names$vehicle_name), "name"]
# Generating the LaTeX table
genxtable(xtable(iv_elasticities, align="lrrrr", digits=c(0,rep(5,4))), basename="iv_elast", include.rownames=TRUE)

# Storing the regression results in a LaTeX file
sink(file="../doc/tables/ols_iv_reg.gen")
stargazer(linear_demand, blpIV, type="latex",
          covariate.labels=c("Constant", "HP/Weight", "Air",
                             "MPD", "Size", "Price"),
          column.labels=c("OLS", "IV"),
          float=FALSE, intercept.bottom=FALSE, dep.var.labels="",
          model.names=FALSE)
sink()

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


# -----------------------Question 5
# Random coefficients model
library(BLPestimatoR)
# Prepare data for BLP estimation
blp_model <- as.formula("market_share ~ horsepower_weight+ac_standard+miles_per_dollar+length_width+price |
horsepower_weight+ac_standard+miles_per_dollar+length_width+0 |
horsepower_weight+ac_standard+miles_per_dollar+length_width+price |
horsepower_weight+ac_standard+miles_per_dollar+length_width+own_firm_hpIV+own_firm_airIV+own_firm_mpdIV+own_firm_spaceIV+own_firm_consIV+rival_firm_hpIV+rival_firm_airIV+rival_firm_mpdIV+rival_firm_spaceIV+rival_firm_consIV")

dd <- distinct(correct_blp, vehicle_name, year, .keep_all=TRUE)
dd$vehicle_name <- as.character(dd$vehicle_name)

# Getting the data ready for BLP estimation
what <- BLP_data(model=blp_model,
         market_identifier="year",
         product_identifier="vehicle_name",
         productData=dd,
         blp_inner_tol=1e-6,
         blp_inner_maxit = 5000,
         integration_method="MLHS",
         integration_accuracy = 40,
         integration_seed = 1)

# Theta guesses
theta_guesses  <- as.matrix(rep(1,6))
rownames(theta_guesses) <- c("(Intercept)", "horsepower_weight", "ac_standard",
                             "miles_per_dollar", "length_width", "price")
colnames(theta_guesses)[1] <- "unobs_sd"

# Estimates BLP
blp_est <- estimateBLP(blp_data=what,
            par_theta2=theta_guesses,
            solver_method="BFGS", solver_maxit=1000, solver_reltol=1e-6,
            standardError="heteroskedastic",
            extremumCheck=FALSE,
            printLevel=1)
# Function to get elasticities

get_elasticities(blp_data=what,
                 blp_estimation=blp_est,
                 variable="price",
                 products=c("HDACCO", "FDESCO", "VWJETT"),
                 market=90)

# -----------------------Question 6
# Random coefficients model (with income effects)
# Construct the demographic data
incomeMeans <- c(2.01156, 2.06526, 2.07843, 2.05775, 2.02915, 2.05346,
                 2.06745, 2.09805, 2.10404, 2.07208, 2.06019, 2.06561,
                 2.07672, 2.10437, 2.12608, 2.16426, 2.18071, 2.18856,
                 2.21250, 2.18377)
# Sigma (y)
sigma_v <- 1.72
# Number of draws
ns <- 100
# Drawing random data
demoDat <- do.call(rbind, lapply(incomeMeans, function(x){
    set.seed(123)
    k <- rnorm(n=ns, mean=x, sd=sigma_v)
    return(k)
}))

demoDat <- data.frame(demoDat)
colnames(demoDat) <- paste0("draw_", 1:ncol(demoDat))
demoDat$year <- unique(correct_blp$year)
# Rearranging the columns for the demographic data
demoDat <- demoDat[, c(ncol(demoDat), 1:(ncol(demoDat)-1))]
demoDat <- list(demoDat)
names(demoDat) <- "income"

blp_incomeEffect <- BLP_data(model=blp_model,
                             market_identifier="year",
                             product_identifier="vehicle_name",
                             productData=dd,
                             demographic_draws=demoDat,
                             blp_inner_tol=1e-6,
                             blp_inner_maxit = 5000,
                             integration_method="MLHS",
                             integration_accuracy = ns,
                             integration_seed = 1)

# Start with the guesses
theta_guesses  <- matrix(c(rep(1,6), rep(1,6)), nrow=6, ncol=2)
theta_guesses[1,2] <- 0

# Write in the names in the correct order
rownames(theta_guesses) <- c("(Intercept)", "horsepower_weight",
                             "ac_standard", "miles_per_dollar",
                             "length_width", "price")
colnames(theta_guesses) <- c("unobs_sd", "income")

# Estimates BLP
blpEst_incomeEffect <- estimateBLP(blp_data=blp_incomeEffect,
                             par_theta2=theta_guesses,
                             solver_method="BFGS", solver_maxit=1000,
                             solver_reltol=1e-6,
                             standardError="heteroskedastic",
                             extremumCheck=FALSE,
                             printLevel=1)
