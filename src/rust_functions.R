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
