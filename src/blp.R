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
## marks <- temp %>%
##     group_by(firmid) %>%
##     transmute(shares=sum(market_share))
## sum.share <- unique(temp$sum_shares)
## out.share <- unique(temp$outside_shares)
y <- temp$market_share
X  <- as.matrix(temp[, c("cons", "horsepower_weight", "ac_standard", "miles_per_dollar", "length_width", "price")])
Z <- temp$outside_shares
# Maximum likelihood estimation
mle.res <- optim(par=c(-10,-0.12,-0.03, 0.26,2.34,-0.088), fn=log_ll, method="Nelder-Mead", y=y, X=X, Z=Z, hessian=TRUE)
mle.se <- sqrt(diag(mle.res$hessian))

mle.results <- data.frame(Coefficient=mle.res$par, StdError=mle.se)
rownames(mle.results) <- c("(Intercept)", "HP/Wt", "Air", "MPD", "Size", "Price")
# Generating LaTeX table
genxtable(xtable(mle.results, align="lrr", digits=c(0, rep(4,2))), basename="mle_est", include.rownames=TRUE)

# Calculating the elasticities
coef.mle <- as.numeric(mle.results[,1])
names(coef.mle) <- c("(Intercept)", "horsepower_weight", "ac_standard", "miles_per_dollar", "length_width", "price")
dat <- temp[which(temp$vehicle_name %in% cars & temp$year == 90), ]
dat$vehicle_name <- as.character(dat$vehicle_name)
dat <- dat[, c("vehicle_name", "year", "market_share", "price")]
# Getting the price estimates from the regression object
price_est <- coef.mle["price"]
# Calculate own price elasticites
dat.vnm  <- split(dat, dat$vehicle_name)
own_price <- do.call(rbind, lapply(dat.vnm, function(x){
        ope <- round(price_est* x$price *(1-x$market_share),digits=5)
        return(ope)
    }))
# Calculate cross price elasticities
cross_price <- lapply(dat.vnm, function(x){
        k <- dat.vnm[which(names(dat.vnm) != x$vehicle_name)]
        cpe <- do.call(rbind, lapply(k, function(y){
            cpe <- round(-price_est*y$price*y$market_share, digits=5)
            return(cpe)
        }))
        cpe <- data.frame(cpe)
        cpe$vehicle_name <- rownames(cpe)
        rownames(cpe) <- NULL
        return(cpe)
    })
nn <- names(cross_price)
for(i in 1:length(cross_price)){
        colnames(cross_price[[i]])[1] <- nn[i]
    }
elasticity <- Reduce(function(x, y) merge(x, y, all=T, by.x="vehicle_name",
                                              by.y="vehicle_name"), cross_price)
elasticity <- data.frame(elasticity)
rownames(elasticity) <- elasticity[, 1]
elasticity[, 1] <- NULL
# Fill in the diagonal elements
elasticity <- as.matrix(elasticity)
diag(elasticity) <- own_price
elasticities <- elasticity
# Writing the car names for generating LaTeX tables
rownames(elasticities) <- car.names[match(rownames(elasticities), car.names$vehicle_name), "name"]
colnames(elasticities) <- car.names[match(colnames(elasticities), car.names$vehicle_name), "name"]
# Generating the LaTeX table
genxtable(xtable(elasticities, align="lrrrrr", digits=c(0,rep(5,5))), basename="mle_elast", include.rownames=TRUE)

# Trying alternate guess (doesn't work) - sensitive to the initial guesses
## mle.res2 <- optim(par=rep(1,6), fn=log_ll, method="Nelder-Mead", y=y, X=X, Z=Z, hessian=TRUE)

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
cars <- c("HDACCO", "FDESCO", "VWJETT", "MB420S", "BW735i")
# Adding the car names
car.names <- data.frame(vehicle_name=cars, name=c("Honda Accord", "Ford Escort", "VW Jetta", "Mercedez Benz S420", "BMW 735i"))
# Get elasticities
ols_elasticities <- calc_elasticity(obj=linear_demand, cars=cars, data=temp, year=90)
# Writing the car names for generating LaTeX tables
rownames(ols_elasticities) <- car.names[match(rownames(ols_elasticities), car.names$vehicle_name), "name"]
colnames(ols_elasticities) <- car.names[match(colnames(ols_elasticities), car.names$vehicle_name), "name"]
# Generating the LaTeX table
genxtable(xtable(ols_elasticities, align="lrrrrr", digits=c(0,rep(5,5))), basename="ols_elast", include.rownames=TRUE)

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
genxtable(xtable(iv_elasticities, align="lrrrrr", digits=c(0,rep(5,5))), basename="iv_elast", include.rownames=TRUE)

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
# Run this only once and FIX it.
blp_noIncome <- BLP_data(model=blp_model,
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
blp_est <- estimateBLP(blp_data=blp_noIncome,
            par_theta2=theta_guesses,
            solver_method="BFGS", solver_maxit=1000, solver_reltol=1e-6,
            standardError="heteroskedastic",
            extremumCheck=FALSE,
            printLevel=1)

# Function to get elasticities
rand_coef_elasticity <- get_elasticities(blp_data=blp_noIncome,
                 blp_estimation=blp_est,
                 variable="price",
                 products=sort(cars),
                 market=90)

# Writing the car names for generating LaTeX tables
rownames(rand_coef_elasticity) <- car.names[match(rownames(rand_coef_elasticity), car.names$vehicle_name), "name"]
colnames(rand_coef_elasticity) <- car.names[match(colnames(rand_coef_elasticity), car.names$vehicle_name), "name"]
# Generating the LaTeX table
genxtable(xtable(rand_coef_elasticity, align="lrrrrr", digits=c(0,rep(5,5))), basename="rand_coef_elast", include.rownames=TRUE)

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

# Drawing log-normal random data
demoDat <- do.call(rbind, lapply(incomeMeans, function(x){
    set.seed(123)
    # Adjusting for mean and standard deviation
    m_x  <- log(x^2/sqrt(sigma_v^2+x^2))
    sigma_x  <- sqrt(log(1+(sigma_v^2/x^2)))
    ## k <- rlnorm(n=ns, meanlog=x, sd=sqrt(sigma_v))
    k <- rlnorm(n=ns, meanlog=m_x, sd=sigma_x)
    return(1/k)
}))

# Arranging the data in the format required for the package
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
                             blp_inner_maxit = 1000,
                             integration_method="MLHS",
                             integration_accuracy = ns,
                             integration_seed = 1)

# Start with the guesses
theta_guesses  <- matrix(c(rep(1,6), rep(1,6)), nrow=6, ncol=2)
# theta_guesses <- cbind(rep(0,6), rep(0,6))
# Write in the names in the correct order
rownames(theta_guesses) <- c("(Intercept)", "horsepower_weight",
                             "ac_standard", "miles_per_dollar",
                             "length_width", "price")
colnames(theta_guesses) <- c("unobs_sd", "income")

# Estimates BLP
blpEst_incomeEffect <- estimateBLP(blp_data=blp_incomeEffect,
                             par_theta2=theta_guesses,
                             solver_method="BFGS", solver_maxit=1000,
                             solver_reltol=1e-2,
                             standardError="heteroskedastic",
                             extremumCheck=TRUE,
                             printLevel=1, hessian=FALSE)

rand_coef_elasticityInc <- get_elasticities(blp_data=blp_incomeEffect,
                 blp_estimation=blpEst_incomeEffect,
                 variable="price",
                 products=sort(cars),
                 market=90)

# Storing in the data for Stata
write.csv(correct_blp, file="../data/stata_cars.csv")



mkt.id <- correct_blp[, "year"]

ind_sh <- function(delta.in, mu.in){
    # This function computes the "individual" probabilities of choosing each brand
    # Requires global variables: mkt.id, X, v
    numer <- exp(mu.in) * matrix(rep(exp(delta.in), n.sim), ncol = n.sim);
    denom <- as.matrix(do.call("rbind", lapply(mkt.id, function(tt){
        1 + colSums(numer[mkt.id %in% tt, ])
    })))
    return(numer / denom);  
}


### Trying other way
income_means <- c(2.01156, 2.06526, 2.07843, 2.05775, 2.02915,
                  2.05346, 2.06745, 2.09805, 2.10404, 2.07208,
                  2.06019, 2.06561, 2.07672, 2.10437, 2.12608,
                  2.16426, 2.18071, 2.18856, 2.21250, 2.18377)

income_std <- (1.72)

num_draw <- 20

income_draw <- do.call(rbind, lapply(income_means, function(x){
  income_temp <- rlnorm(n=num_draw, meanlog = income_means, sd = income_std)
  return(1/income_temp)
}))

income_row_mean <- rowMeans(income_draw)

income_draw <- income_draw - income_row_mean
data_iv_distinct <- dd
income_draw <- data.frame(income_draw)
colnames(income_draw) <-paste0("draw_", 1:num_draw)
income_draw$year <- unique(data_iv_distinct$year)
income_draw <- income_draw[,c(num_draw+1,1:num_draw)]
income_draw <- list(income_draw)

names(income_draw) <- "price"

# Drawing random coefficient v part #
constant_draw <- list(matrix(rnorm(20*num_draw, mean = 0, sd = 1),20,num_draw))
constant_draw <- data.frame(constant_draw)
colnames(constant_draw) <-paste0("draw_", 1:num_draw)
constant_draw$year <- unique(data_iv_distinct$year)
constant_draw <- constant_draw[,c(num_draw+1,1:num_draw)]
constant_draw <- list(constant_draw)

names(constant_draw) <- "(Intercept)"

horsepower_weight <- list(matrix(rnorm(20*num_draw, mean = 0, sd = 1),20,num_draw))
horsepower_weight <- data.frame(horsepower_weight)
colnames(horsepower_weight) <-paste0("draw_", 1:num_draw)
horsepower_weight$year <- unique(data_iv_distinct$year)
horsepower_weight <- horsepower_weight[,c(num_draw+1,1:num_draw)]
horsepower_weight <- list(horsepower_weight)

names(horsepower_weight) <- "horsepower_weight"

length_width <- list(matrix(rnorm(20*num_draw, mean = 0, sd = 1),20,num_draw))
length_width <- data.frame(length_width)
colnames(length_width) <-paste0("draw_", 1:num_draw)
length_width$year <- unique(data_iv_distinct$year)
length_width <- length_width[,c(num_draw+1,1:num_draw)]
length_width <- list(length_width)

names(length_width) <- "length_width"

ac_standard <- list(matrix(rnorm(20*num_draw, mean = 0, sd = 1),20,num_draw))
ac_standard <- data.frame(ac_standard)
colnames(ac_standard) <-paste0("draw_", 1:num_draw)
ac_standard$year <- unique(data_iv_distinct$year)
ac_standard <- ac_standard[,c(num_draw+1,1:num_draw)]
ac_standard <- list(ac_standard)

names(ac_standard) <- "ac_standard"

miles_per_dollar <- list(matrix(rnorm(20*num_draw, mean = 0, sd = 1),20,num_draw))
miles_per_dollar <- data.frame(miles_per_dollar)
colnames(miles_per_dollar) <-paste0("draw_", 1:num_draw)
miles_per_dollar$year <- unique(data_iv_distinct$year)
miles_per_dollar <- miles_per_dollar[,c(num_draw+1,1:num_draw)]
miles_per_dollar <- list(miles_per_dollar)

names(miles_per_dollar) <- "miles_per_dollar"

originalDraws <- c(constant_draw, income_draw, 
         horsepower_weight, length_width, ac_standard, miles_per_dollar)


data_blp_q6 <- BLP_data(model = blp_model,
                        market_identifier="year",
                        product_identifier = "vehicle_name",
                        productData = data_iv_distinct,
                        blp_inner_tol = 1e-6,
                        blp_inner_maxit = 5000,
                        integration_draws = originalDraws, 
                        integration_weights= rep(1/num_draw,num_draw))

theta_guesses_q6 <- as.matrix(cbind(c(0,0,0,0,0,0)))
colnames(theta_guesses_q6) <- c("unobs_sd")
rownames(theta_guesses_q6) <- c("(Intercept)","price", "horsepower_weight", "length_width","ac_standard", "miles_per_dollar")



blp_q6 <- estimateBLP(blp_data = data_blp_q6,
                      par_theta2 = theta_guesses_q6,
                      solver_method = "BFGS", solver_maxit = 1000, solver_reltol = 1e-6,
                      standardError = "heteroskedastic",
                      extremumCheck = FALSE ,
                      printLevel = 1 )

rand_coef_elasticityInc <- get_elasticities(blp_data=data_blp_q6,
                 blp_estimation=blp_q6,
                 variable="price",
                 products=sort(cars),
                 market=90)

# Writing the car names for generating LaTeX tables
rownames(rand_coef_elasticityInc) <- car.names[match(rownames(rand_coef_elasticityInc), car.names$vehicle_name), "name"]
colnames(rand_coef_elasticityInc) <- car.names[match(colnames(rand_coef_elasticityInc), car.names$vehicle_name), "name"]
# Generating the LaTeX table
genxtable(xtable(rand_coef_elasticityInc, align="lrrrrr", digits=c(0,rep(5,5))), basename="rand_coef_elast_inc", include.rownames=TRUE)
