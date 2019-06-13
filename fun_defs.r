CapStr <- function(x) {
  c <- strsplit(x, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
