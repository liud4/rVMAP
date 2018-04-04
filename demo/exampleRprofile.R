# exampleRprofile.R
#
# This is an example .Rprofile script that will be sourced when R is started.
#     For ease of use, modify this template and put your .Rprofile file in a
#     personal GitHub repository called dotfiles. In your home directory,
#     create a .Rprofile that sources the version controlled file.
# -----------------------------------------------------------------------------

# Useful Functions

## create a hidden environment
.env = new.env()

## ht: show the first and last n rows of a data object (default: n = 10)
.env$ht <- function(data, n = 10) {
  n.total <- dim(data)[1]
  n.head <- min(n, n.total)
  n.tail <- min(n, n.total - n.head)
  if(n.total <= n.head) {
    head(data, n.head)
  } else {
    rbind(head(data, n.head), tail(data, n.tail))
  }
}

## hh: show the first n rows and first n columns of a data object (default: n = 5)
.env$hh <- function(data, n = 5) {
  n.row <- min(dim(data)[1], n)
  n.col <- min(dim(data)[2], n)
  data[1:n.row, 1:n.col]
}

## lsa: (l)i(s)t (a)ll objects and classes
.env$lsa <- function() {
    obj_type <- function(x) class(get(x, envir = .GlobalEnv)) # define environment
    foo = data.frame(sapply(ls(envir = .GlobalEnv), obj_type))
    foo$object_name = rownames(foo)
    names(foo)[1] = "class"
    names(foo)[2] = "object"
    return(unrowname(foo))
  }

## lsp: (l)i(s)t all functions in a (p)ackage
.env$lsp <- function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

## macopen: open macOS Finder to the current directory
.env$macopen <- function(...) if(Sys.info()[1] == "Darwin") system("open .")

## read.cb: read data on clipboard
.env$read.cb <- function(...) {
  ismac <- Sys.info()[1] == "Darwin"
  if(!ismac) read.table(file = "clipboard", ...)
  else read.table(pipe("pbpaste"), ...)
}

## Attach all the variables above
attach(.env)

## print.functions: print functions defined in .Rprofile
print.functions <- function() {
  cat("ht(data, n = 10) | show the first and last n rows\n")
  cat("hh(data, n = 5) | show the first n rows and first n columns\n")
  cat("lsa() | list all objects and classes\n")
  cat("lsp(package, all.names = FALSE, pattern) | list all functions in a package\n")
  cat("macopen() | open macOS Finder to current working directory\n")
  cat("read.cb() | read data on clipboard\n")
}

# -----------------------------------------------------------------------------

# Initial Settings
box.dir <- file.path("~", "box")

# -----------------------------------------------------------------------------

# .First() and .Last()

.First <- function() {
  options(
    repos = c(CRAN = "http://cran.rstudio.com"),
    keep.source = TRUE,
    keep.source.pkgs = TRUE,
    devtools.name = "Omair",
    devtools.desc.name = "Omair A. Khan",
    devtools.desc.author = "Omair A. Khan <omair.a.khan@vanderbilt.edu, omair@prettynumbe.rs> [aut, cre]"
    stringsAsFactors = FALSE
  )
  cat("\nSuccessfully loaded .Rprofile at", date(), "\n")
}

.Last <- function(){
  if (interactive()) {
    ## check to see if we're in an RStudio project (requires the rstudioapi package)
    if (!requireNamespace("rstudioapi"))
      return(NULL)
    pth <- rstudioapi::getActiveProject()
    if (is.null(pth))
      return(NULL)

    ## append date + sessionInfo to a file called sessionInfoLog
    cat("Recording session info into the project's sesionInfoLog file...")
    info <-  capture.output(sessionInfo())
    info <- paste("\n----------------------------------------------",
                  paste0('Session Info for ', Sys.time()),
                  paste(info, collapse = "\n"),
                  sep  = "\n")
    f <- file.path(pth, "sessionInfoLog")
    cat(info, file = f, append = TRUE)
  }
}
