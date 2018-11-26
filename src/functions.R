genxtable <- function(x, basename, include.rownames=FALSE,...) {
  print(x,
        type="latex",
        file=paste("../doc/tables/",basename,".gen", sep=""),
        include.rownames=include.rownames,
        table.placement="tp",
        caption.placement="top",
        sanitize.text=function(x)x,
        latex.environments=c("center","scriptsize"),...)
  print(x,
        type="latex",
        file=paste("../doc/tables/",basename,"_float",".gen", sep=""),
        include.rownames=include.rownames,
        sanitize.text=function(x)x,
        latex.environments=c("center","scriptsize"),
        floating=FALSE)
}


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
    return(elasticity)
}
