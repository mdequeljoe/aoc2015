d <- list(row = 3010, column = 3019)

get_code <- function(last){
  v <- last * 252533
  v %% 33554393
}

set_m <- function(rw, cl){
  m <- matrix(nrow = rw, ncol = cl)
  
  rval <- numeric(rw)
  rval[1:2] <- c(1, 2)
  for (i in 3:rw)
    rval[i] <- (i - 1) + rval[i - 1]
  m[, 1] <- rval
  
  for (r in 1:rw)
    for (k in 2:cl)
      m[r, k] <- m[r, k - 1] + k + r - 1
  m
}

set_val <- function(n){
  v <- numeric(n)
  v[1] <- 20151125
  for (i in 2:n)
    v[i] <- get_code(v[i - 1])
  v
}

# part 1 
x <- set_m(d$row, d$column)
i <- x[d$row, d$column]
m <- set_val(i)
m[i]

# part 2 is a free star for last day