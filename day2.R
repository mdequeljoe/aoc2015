


calc_sa <- function(l, w, h) {
  dim <- c(s1 = l * w,
           s2 = w * h,
           s3 = h * l)
  sum(dim * 2) + min(dim)
}
 
calc_sa(2, 3, 4)
calc_sa(1, 1, 10)

rl <- readLines('data/day2.txt')
d <- lapply(rl, function(k){
  k <- strsplit(k, "x")[[1]]
  k <- as.numeric(k)
  do.call(calc_sa, as.list(k))
})

sum(unlist(d))

calc_ribbon <- function(l, w, h) {
  d <- sort(c(l, w, h))[1:2]
  2 * d[1] + 2 * d[2] + Reduce(`*`, c(l, w, h))
}
calc_ribbon(2, 3, 4)
calc_ribbon(1, 1, 10)

d <- lapply(rl, function(k){
  k <- strsplit(k, "x")[[1]]
  k <- as.numeric(k)
  do.call(calc_ribbon, as.list(k))
})
sum(unlist(d))
