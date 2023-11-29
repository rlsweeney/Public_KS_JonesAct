detachAllPackages <- function() {
  
    basic.packages <- c("package:stats",
                        "package:graphics",
                        "package:grDevices",
                        "package:utils",
                        "package:datasets",
                        "package:methods",
                        "package:base")
  
    pl <- search()
    pl <- pl[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  
    pl <- setdiff(pl, basic.packages)
  
    if (length(pl) > 0) {
        for (package in pl) detach(package, character.only=TRUE)
    }
}

detachAllPackages()

# Load the packages we need 
packages <- c("here","tidyverse", "lubridate", "haven", 
                "readxl", "gridExtra", "stringdist", "cowplot",
                "xtable", "readxl")

# Load packages and install if not already installed
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
package.check <- invisible(
  lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
)

# PATHS 
root <- here()
## untracked dropbox path (could prob just put these right in this file)
source(file.path(root, "code", "paths.R"))

# COMMON CONSTANTS -----------------------------------------------------------
YearHours <- 8760