#
# Utility tools
#
ParsePatientID <- function(x) {
    return(unlist(strsplit(x, "/"))[2])
}


ReplaceNA <- function(df) {
    return(replace(df, is.na(df), ""))
}
