# Loading in the required libraries
library(dplyr)
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

# Honda Accord (1990) "HDACCO"
hdacco90 <- temp[which(temp$vehicle_name=="HDACCO" & temp$year==90), ]
# The own price elasticity (-alpha * p_j * (1-s_j)
# with 95% confidence interval
hdacco90_ope <- apply(price_coeff, 2, function(x){
    -x*hdacco90$price*(1-hdacco90$market_share)
})
# Cross-price elasticity w.r.t. Ford Escort (1990) "FDESCO"
# The cross price elasticity (-alpha * s_j * s_k)
# with 95% confidence interval
hdacco90_cpe <- apply(price_coeff, 2, function(x){
    x * hdacco90$market_share * fdesco90$market_share
})
