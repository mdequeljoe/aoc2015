

set_m <- function(d){
  d <- readLines(d, warn = FALSE)
  m <- matrix("", nrow = n <- length(d), ncol = n)
  d <- strsplit(d, "")
  d <- lapply(seq_along(d), function(r){
    m[r, ] <<- d[[r]]
  })
  m
}


update <- function(m, exclude = list()){
  o <- matrix("", nrow = nrow(m), ncol = ncol(m))
  for (r in 1:nrow(m)){
    for (k in 1:ncol(m)){
      o[r, k] <- update_cell(m, c(r, k), exclude)
    }
  }
  o
}

update_cell <- function(m, x, exclude) {

  ex <- lapply(exclude, identical, x)
  if (any(unlist(ex)))
    return("#")
  lim <- c(nrow(m) + 1, ncol(m) + 1, 0, 0)
  k <- list(c(-1, 0),
            c(-1, 1),
            c(0, 1),
            c(1, 1),
            c(1, 0),
            c(1, -1),
            c(0, -1),
            c(-1, -1))
  k <- lapply(k, function(d) {
    d <- d + x
    if (any(d %in% lim))
      return()
    m[d[1], d[2]]
  })
  k <- unlist(k)
  s <- m[x[1], x[2]]
  n_on <- length(k[k == "#"])
  if (s == "#") {
    if (n_on %in% c(2, 3))
      return("#")
    return(".")
  }
  if (n_on == 3)
    return("#")
  "."
  
}

run_lights <- function(m, n = 100, exclude = list()) {
  for (i in 1:n)
    m <- update(m, exclude)
  m
}

set_m('data/day18bis.txt') %>%
  run_lights(4) %>%
  .[. == "#"] %>%
  length

set_m('data/day18.txt') %>%
  run_lights() %>%
  .[. == "#"] %>%
  length

# part two
# corners always on
corners <- function(m) {
  r <- nrow(m)
  k <- ncol(m)
  lapply(list(c(1, 1),
              c(1, k),
              c(r, k),
              c(r, 1)), as.integer)
}

set_corners <- function(m){
  lapply(corners(m), function(x){
    m[x[1], x[2]] <<- "#"
  })
  m
}

set_m('data/day18.txt') %>% 
  set_corners() %>%
  run_lights(exclude = corners(.)) %>%
  .[. == "#"] %>%
  length

