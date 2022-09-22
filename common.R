#
# Utility tools
#

LoadRequiredPackages <- function(required_pacakges) {
    for (package in required_packages) {
        if (!require(package, character.only = TRUE)) {
            install.packages(package, repos = "http://cran.us.r-project.org")
            library(package, character.only = TRUE)
        }
    }
}


ParsePatientID <- function(x) {
    return(unlist(strsplit(x, "/"))[2])
}


ReplaceNA <- function(df) {
    return(replace(df, is.na(df), ""))
}
