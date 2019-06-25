CapStr <- function(x) {
  c <- strsplit(x, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))


