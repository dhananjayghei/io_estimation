source("rust_functions.R")
# Reading in the Rust data from the zip file
dat <- read.zip("../data/rust-data.zip")
Rows <- c(137*18, 137*10, 137*18, 137*12, 128*37, 110*4, 36*15, 60*4, 81*48)
names(Rows) <- names(dat)


# Some rows have weird codes at the end
dat <- checkRows(dat, Rows)


# Constructing the data set in the right dimension
dat <- createRust(dat)

# Get odometer values at replacement







