source("rust_functions.R")
# Reading in the Rust data from the zip file
dat <- read.zip("../data/rust-data.zip")
# The files are read in alphabetical order
# The dimensions of these files are as given below
Rows <- c(137*18, 137*10, 137*18, 137*12, 128*37, 110*4, 36*15, 60*4, 81*48)
names(Rows) <- names(dat)

# Some rows have weird codes at the end. Getting rid of these
dat <- checkRows(dat, Rows)

# Constructing the data set in the right dimension
dat <- createRust(dat)

# The first 11 rows in all the data sets are in the following order
## 1. Bus Number
## 2. Month Purchased
## 3. Year Purchased
## 4. Month of 1st engine replacement
## 5. Year of 1st engine replacement
## 6. Odometer at replacement
## 7. Month of 2nd engine replacement
## 8. Year of 2nd engine replacement
## 9. Odometer at replacement
## 10. Month Odometer data begins
## 11. Year odometer data begins



# Function to get input data set for estimation
discretise_data <- function(dat, dim.fp=90, omax=450000){
    # Get odometer values at replacement
    oval1 <- dat[6,]
    oval2 <- dat[9,]
    # Store in the dimensions
    nr <- dim(dat)[1]
    nc <- dim(dat)[2]
    # Getting the odometer reading stopping points (for replacement)
    dtc <- (dat[12:nr,] >= oval1)*(oval1>0)+(dat[12:nr,] >= oval2)*(oval2>0)
    dtx <- (dat[12:nr, ] + oval1*dtc*(dtc-2) - .5*oval2*dtc*(dtc-1))
    dtx <- ceiling(dim.fp*dtx/omax)
    dtc <- rbind(dtc[2:(nr-11),]-dtc[1:(nr-12),], rep(0,nc))
    mil <- (dtx[2:(nr-11),]-dtx[1:(nr-12),])+(dtx[1:(nr-12),]*dtc[1:(nr-12),])
    return(list(dtc=dtc, dtx=dtx, mil=mil))
}


## i <- 1
## while(i < dim.fp){
##     dt <- which(dtx==i)
##     nc[i,1] <- sum(dt)
## }

