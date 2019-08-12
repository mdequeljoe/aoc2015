

# d <- readLines('data/day13bis.txt', warn = FALSE)
d <- readLines('data/day13.txt', warn = FALSE)
d <- strsplit(d, " |\\.")
persons <- unique(vapply(d, `[[`, character(1), 1))
l <- list()
d <- lapply(d, function(d) {
  v <- as.numeric(d[4])
  v <- if ("lose" %in% d)
    - v
  else
    v
  l[[d[1]]][[d[length(d)]]] <<- v
})

expand <- function(v) {
  if (length(v) %in% c(0, 1))
    return(v)
  l <- list()
  for (i in seq_along(v)) {
    x <- v[i]
    rem <- v[-i]
    for (k in expand(rem))
      l[[length(l) + 1]] <- c(x, k)
  }
  l
}

# part two add ME with zero score for everyone
persons <- c(persons, "ME")  
persons <- expand(persons)
lapply(persons, function(p) {
  score <- 0
  for (i in seq_along(p)) {
    j <- i %% length(p) + 1
    # part two >
    if ("ME" %in% p[c(i, j)])
      next
    s <- l[[p[i]]][p[j]]
    s2 <- l[[p[j]]][p[i]]
    score <- score + s + s2
  }
  score
}) %>%
  unlist() %>%
  max()
