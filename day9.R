library(magrittr)

calc_travel <- function(x, calc_by = min) {
  d <- lapply(strsplit(x, " "), function(d) {
    list(loc = c(d[1], d[3]), dist = as.numeric(d[5]))
  })
  locations <- find_cities(x)
  nloc <- length(locations)
  o <- vapply(locations, function(loc) {
    
    tv <- travel(loc, d, n = nloc)
    calc_by(unlist(tv))
    
  }, numeric(1))
  calc_by(o)
}

find_cities <- function(x){
  rx <- "([A-z]+)(\\sto\\s)([A-z]+)(\\s.+)"
  union(gsub(rx, "\\1", x), gsub(rx, "\\3", x))
}

travel <- function(from,
                   routes,
                   n,
                   seen = from,
                   distance = 0
                   ) {
  lapply(routes, function(x) {
    
    if (!from %in% x$loc ||
        (dest <- x$loc[x$loc != from]) %in% seen)
      return()
    
    seen <- c(seen, dest)
    dist <- distance + x$dist
    if (length(seen) == n)
      dist
    else
      travel(dest, routes, n, seen, dist)
  })
  
}

c("London to Dublin = 464",
  "London to Belfast = 518",
  "Dublin to Belfast = 141") %>% calc_travel()

#part 1
readLines('data/day9.txt', warn = FALSE) %>% 
  calc_travel()

#part 2
readLines('data/day9.txt', warn = FALSE) %>% 
  calc_travel(calc_by = max)

