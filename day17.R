
sumjars <- function(d, n = 25){
  o <- list()
  for (i in seq_along(d)){
    k <- utils::combn(d, i)
    k <- k[, apply(k, 2, function(x) sum(x) == n)]
    if (length(k))
      o[[length(o) + 1]] <- k
  }
  o
}

sumjars(c(20, 15, 10, 5, 5))

d <- readLines('data/day17.txt', warn = FALSE)
d <- as.numeric(d)

# part one
o <- sumjars(d, n = 150)
sum(vapply(o, ncol, numeric(1)))

#part two
ncol(o[[1]])
