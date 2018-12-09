# Loading the required library
library(AER) # For IV regression
library(stargazer) # For LaTeX regression tables
library(xtable) # For LaTeX tables
# Reading in the data set
dat <- read.table("../data/PORTER.PRN")
# Setting in the column names in order
colnames(dat) <- c("week", "quantity", "price", "lakes", "collusion",
                   paste0("d", 1:4), paste0("seas", 1:13))
# Generating summary statistics
# Replicating Table II of Porter to check for the consistency of data
vars <- c("price", "quantity", "lakes", "collusion")
sum_vars <- do.call(rbind, lapply(vars, function(x){
    k <- c(Mean=mean(dat[, x]), Sdev=sd(dat[, x]), Min=min(dat[, x]),
           Max=max(dat[, x]))
    return(round(k, 4))
}))
sum_vars[2, ] <- round(sum_vars[2, ], 0)
sum_vars <- data.frame(sum_vars)
rownames(sum_vars) <- c("Price", "Quantity", "Lakes", "Collusion")

# Generating log variables
dat$logQ <- log(dat$quantity)
dat$logP <- log(dat$price)
# Converting the seasonal dummies to factor
mat <- as.matrix(dat[, 11:22])
# Storing it as a factor
dat$seasonal <- factor((mat %*% (1:ncol(mat)))+1,
                       labels=c("1", 2:13))
# Run a simple OLS regression
# Elasticity = -.638 (Problem? The demand looks inelastic?)
ols <- lm(logQ~logP+lakes+seasonal, data=dat)
# Run IV with collusion as instrument for price
iv_collusion <- ivreg(logQ~lakes+seasonal+logP | lakes+seasonal+collusion, data=dat)
# Run IV with dummy and collusion as instruments for price
# These results look much closer to Column I of Table III from Porter
iv_collusion_dv <- ivreg(logQ~lakes+seasonal+logP | lakes+seasonal+collusion+d1+d2+d3+d4, data=dat)

# Estimating the supply equation
ols_supply <- lm(logP~d1+d2+d3+d4+logQ+collusion, data=dat)
# Run IV with lakes as instrument for log quantity
iv_supply <- ivreg(logP~d1+d2+d3+d4+seasonal+collusion+logQ | d1+d2+d3+d4+seasonal+collusion+lakes, data=dat)

# Storing the regression results (Demand equation)
sink(file="../doc/tables/porter_demand.gen")
stargazer(ols, iv_collusion, iv_collusion_dv,
          type="latex", keep=c("logP", "lakes"),
          covariate.labels=c("Log(Price)", "Lakes"),
          floating=FALSE)
sink()

## # Markov-switching regime
## library(MSwM)

## kk <- msmFit(object=ols_supply, k=2, sw=c(rep(TRUE, 7), TRUE), control=list(parallel=FALSE))
