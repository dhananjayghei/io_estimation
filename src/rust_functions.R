# Reading in the zip file
read.zip <- function(file,...){
    # Unzipping the zip file
    zipFileInfo <- unzip(file, list=FALSE)
    filename <- gsub(".asc", "", gsub("./", "", zipFileInfo))
    dat <- lapply(zipFileInfo, function(x){
        l <- read.csv(x, header=FALSE, stringsAsFactors=FALSE)
        return(l)
    })
    names(dat) <- filename
    # Removing the unzipped files
    file.remove(zipFileInfo)
    return(dat)
}

# Data set to check if the number of rows are correct
checkRows <- function(dat, Rows){
    for(i in 1:length(dat)){
        if(Rows[i]==nrow(dat[[i]])){
            dat[[i]] <- as.matrix(dat[[i]])
        }else{
            dat[[i]] <- as.matrix(dat[[i]][-nrow(dat[[i]]), ])
        }
    }
    return(dat)
}

# Create Rust's data set
createRust <- function(dat, rows){
    dat <- lapply(dat, function(x) as.matrix(as.numeric(trimws(x))))
    dim(dat[["d309"]]) <- c(110,4)
    dim(dat[["g870"]]) <- c(36,15)
    dim(dat[["rt50"]]) <- c(60,4)
    dim(dat[["t8h203"]]) <- c(81,48)
    dim(dat[["a452372"]]) <- c(137,18)
    dim(dat[["a452374"]]) <- c(137,10)
    dim(dat[["a530872"]]) <- c(137,18)
    dim(dat[["a530874"]]) <- c(137,12)
    dim(dat[["a530875"]]) <- c(128,37)
    return(dat)
}

discretise_data <- function(data, dim.fp=90, omax=450000){
    nr <- dim(data)[1]
    nc <- dim(data)[2]
    tt <- apply(data, 2, function(x){
        dtc <- (x[12:nr]>x[6])*(x[6]>0)+(x[12:nr]>x[9])*(x[9]>0)
        dtx <- x[12:nr]+x[6]*dtc*(dtc-2)-.5*x[9]*dtc*(dtc-1)
        dtx <- ceiling(dim.fp*dtx/omax)
        dtc <- c(dtc[2:(nr-11)]-dtc[1:(nr-12)], 0)
        mil <- (dtx[2:(nr-11)]-dtx[1:(nr-12)])+
            dtx[1:(nr-12)]*dtc[1:(nr-12)]
        return(list(dtc=dtc, dtx=dtx, mil=mil))
    })
    dtc <- do.call(cbind, lapply(tt, function(x) x[[1]]))
    dtx <- do.call(cbind, lapply(tt, function(x) x[[2]]))
    mil <- do.call(cbind, lapply(tt, function(x) x[[3]]))
    return(list(dtc=dtc, dtx=dtx, mil=mil))
}


# Calculate the transition probabilities non-parametrically
# Estimating \theta_{31}, \theta_{32}, \theta_{33}
# Within Group Estimates of Mileage Process (Rust Table V)
probs <- function(mileage){
    nr <- nrow(mileage)
    nc <- ncol(mileage)
    pi.1 <- mean(mileage==0, na.rm=TRUE)
    pi.2  <- mean(mileage==1, na.rm=TRUE)
    pi.3 <- mean(mileage==2, na.rm=TRUE)
    pi <- round(rbind(pi.1, pi.2, pi.3),3)
    pi.se <- round(sqrt(pi*(1-pi)/(nr*nc)), 3)
    est <- data.frame(matrix(nrow=6, ncol=1))
    for(i in 1:nrow(pi)){
        est[2*i-1, 1] <- pi[i]
        est[2*i, 1] <- paste("(", pi.se[i], ")", sep="")
        }
    colnames(est) <- "bus"
    return(est)
}

## probs <- function(mileage){
##     nr <- nrow(mileage)
##     nc <- ncol(mileage)
##     pi.1 <- mean(mileage==0, na.rm=TRUE)
##     pi.2  <- mean(mileage==1, na.rm=TRUE)
##     pi.3 <- mean(mileage==2, na.rm=TRUE)
##     pi <- cbind(pi.1, pi.2, pi.3)
##     pi.se <- round(sqrt(pi*(1-pi)/(nr*nc)), 3)
##     pi <- round(pi, 3)
##     pp <- rbind(pi, pi.se)
##     return(pp)
## }


transition_probabilities <- function(Pi, dim.fp=90){
    # Construct the matrix
    t <- 3
    prob <- matrix(data=0, nrow=dim.fp-t, ncol=dim.fp)
    for(i in 1:nrow(prob)){
        for(j in 0:2){
            prob[i, i+j] <- Pi[j+1]
        }
    }
    # Absorbing the state probabilities
    absorb <- matrix(data=0, nrow=3, ncol=3)
    absorb[1,] <- Pi
    absorb[2,2:3] <- c(Pi[1], 1-Pi[1])
    absorb[3,3] <- 1
    zeros <- matrix(data=0, nrow=t, ncol=dim.fp-t)
    prob <- rbind(prob, cbind(zeros, absorb))
    return(prob)
}


## Utility function
utility <- function(theta, RC, dim.fp=90){
    d0 <- rep(0, dim.fp)
    d1 <- rep(0, dim.fp)
    for(i in 1:dim.fp){
        d0[i] <- -.001*theta*(i)
        d1[i] <- -RC
    }
    return(cbind(d0, d1))
}

## Social surplus
social_surplus <- function(theta, RC, EV, beta=.9999){
    val <- exp(utility(theta, RC)[,1]+beta*EV-EV)+
        exp(utility(theta, RC)[,2]+beta*EV[1]-EV)
    val <- EV + log(val)
    return(val)
}

## Contraction Mapping
contraction <- function(theta, RC, EV, P){
    EV0 <- EV
    tol <- 1
    EV1 <- rep(0, length(EV0))
    while(tol > .001){
        EV1  <- P %*% social_surplus(theta=theta, RC=RC, EV=EV0)
        tol <- max(abs(EV1-EV0), na.rm=TRUE)
        EV0 <- EV1
        EV1 <- rep(0, length(EV0))
    }
    return(EV0)
}

## Conditional choice probabilities
choice_probs <- function(theta, RC, P){
    EV <- contraction(theta, RC, rep(1,dim.fp), P)
    maxEV <- max(EV, na.rm=TRUE)
    PK <- exp(utility(theta, RC)[,1]+beta*EV-maxEV)/
        (exp(utility(theta,RC)[,1]+beta*EV-maxEV)+
         exp(utility(theta,RC)[,2]+beta*EV[1]-maxEV))
    return(PK)
}


## Partial likelihood
## This is the same as l^2 in Rust's notation
partialLL <- function(dat, theta, RC, P){
#    choice_probs(theta=theta, RC=RC, P=P)
    decision <- dat[,"dtc"]
    state <- dat[,"dtx"]
    cp_tmp <- choice_probs(theta, RC, P)
    relevant_probs <- cp_tmp[state]
    pll <- ifelse(decision==0, log(relevant_probs), log(1-relevant_probs))
    return(-sum(pll, na.rm=TRUE))
}

## Creating Rust's data for estimation
for_rust_estimation <- function(dat){
    temp <- do.call(rbind, lapply(dat, function(x){
        dtc <- as.matrix(x[[1]])
        dtc <- dtc[-nrow(dtc),]
        dim(dtc) <- c(prod(dim(dtc)),1)
        dtx <- as.matrix(x[[2]])
        dtx <- dtx[-nrow(dtx),]
        dim(dtx) <- c(prod(dim(dtx)),1)
        mil <- as.matrix(x[[3]])
        dim(mil) <- c(prod(dim(mil)), 1)
        return(data.frame(dtc=dtc,dtx=dtx,mil=mil))
    }))
    rownames(temp) <- NULL
    return(temp)
}
