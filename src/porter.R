# Sourcing the functions file
source("functions.R")
# Loading the required library
library(AER) # For IV regression
library(stargazer) # For LaTeX regression tables
library(xtable) # For LaTeX tables
library(lmtest) # For robust standard errors
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
genxtable(xtable(sum_vars, align="lrrrr", digits=c(0, rep(3,4))),
          basename="sumstats_porter", include.rownames=TRUE)
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
ols_supply <- lm(logP~d1+d2+d3+d4+collusion+logQ, data=dat)
# Run IV with lakes as instrument for log quantity
iv_supply <- ivreg(logP~d1+d2+d3+d4+seasonal+collusion+logQ | d1+d2+d3+d4+seasonal+collusion+lakes, data=dat)

stargazer(ols, type="latex", float=FALSE, keep=1:3)

# Storing the regression results (Demand equation)
sink(file="../doc/tables/porter_demand.gen")
stargazer(ols, iv_collusion, iv_collusion_dv,
          type="latex", covariate.labels=c("Constant", "Log(Price)", "Lakes", paste0("SEAS", 1:12)),
          column.labels=c("OLS", "IV", "IV"), intercept.bottom=FALSE, dep.var.labels="",
          model.names=FALSE,
          float=FALSE)
sink()

# Storing the regrssion results (Supply equation)
sink(file="../doc/tables/porter_supply.gen")
stargazer(ols_supply, iv_supply,
          type="latex", covariate.labels=c("Constant", paste0("D", 1:4), paste0("SEAS", 1:12),
                                           "Collusion", "Log(Quantity)"),
          column.labels=c("OLS", "IV"), intercept.bottom=FALSE, dep.var.labels="",
          model.names=FALSE,
          float=FALSE)
sink()

# Figure 1 (Porter)
pdf(file="../doc/pics/fig1_porter.pdf", width=5.4, height=3.8)
par(mai=c(.8,.8,.3,.2))
plot(dat$week, dat$price, type="l", lwd=2, main="", xlab="Time (in weeks) from January 1, 1980",
     ylab="Grain rate", las=1)
dev.off()

# Cause of price-wars
# Generate an indicator variable for price war

dat$price_war <- as.numeric(c(FALSE, sapply(2:length(dat$collusion), function(i)
    dat$collusion[i] < dat$collusion[(i-1)])))

# Run probit model
options(scipen=12)
probit <- glm(price_war~quantity+lakes+d1+d2+d3+d4, family=binomial(link="probit"), data=dat)
# Getting the robust standard errors
probit.se <- vcovHC(probit, type="HC1")
probit.se <- sqrt(diag(probit.se))

sink(file="../doc/tables/probit_porter.gen")
stargazer(probit, type="latex", se=list(probit.se),
          dep.var.labels="Price war",
          covariate.labels=c("Constant", "Quantity", "Lakes", paste0("D", 1:4)),
          intercept.bottom=FALSE, float=FALSE)
sink()


