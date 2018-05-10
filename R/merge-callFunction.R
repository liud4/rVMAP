callFunction <- function(functionName, filepath, purpose, dat, ...){
    # calls function functionName on dataset dat with arguments ...

    cat("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

    oldnames <- names(dat)

    otherArgs <- list(...)

    # if that doesn't work:
    #http://stackoverflow.com/questions/3142731/r-using-a-list-for-ellipsis-arguments
    #otherArgs <- as.list(substitute(list(...)))[-1L]

    cat("Calling function ", functionName, "().",
        "\n    Purpose: ", purpose, ".\n", sep= "")
    source(file.path(filepath, paste0(functionName, ".R")))
    dat <- do.call(functionName, c(list(dat= dat), otherArgs))

    newnames <- names(dat)
    cat("New variables:\n")
    addedvars <- setdiff(newnames, oldnames)
    numAddedvars <- length(addedvars)
    if (numAddedvars == 0) {
        cat("None.\n")
    } else if (numAddedvars <= 100) {
        print(data.frame(addedvars)) 
    } else {
        cat("Added ", numAddedvars, " new variables.\n", sep= "")
    }

    cat("\nCurrent dimensions:", dim(dat), "\n")
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    dat
}
