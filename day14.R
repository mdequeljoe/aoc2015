


d <- readLines("data/day14bis.txt", warn = FALSE)
d <- readLines("data/day14.txt", warn = FALSE)

set_deers <- function(d) {
  reindeer <- list()
  lapply(strsplit(d, " "), function(d) {
    v <-  d[-1][grepl("\\d", d[-1])]
    v <- as.numeric(v)
    reindeer[[d[1]]] <<-
      list(
        speed = v[1],
        duration = v[2],
        rest = v[3],
        interval = v[2] + v[3],
        dist = 0,
        points = 0
      )
  })
  reindeer
}

reindeer <- set_deers(d)

lapply(1:2503, function(t) {
  reindeer <<- lapply(reindeer, function(d) {
    s <- t %% d$interval
    if (s %in% 1:d$duration)
      d$dist <- d$dist + d$speed
    d
  })
}) -> o


lapply(reindeer, `[[`, 'dist') %>%
  unlist %>%
  max

#part two
reindeer <- set_deers(d)
get_leader <- function(x) {
  d <- vapply(x, `[[`, numeric(1), 'dist')
  names(x[d == max(d)])
}

lapply(1:2503, function(t) {
  reindeer <<- lapply(reindeer, function(d) {
    s <- t %% d$interval
    if (s %in% 1:d$duration)
      d$dist <- d$dist + d$speed
    d
  })
  lead <- get_leader(reindeer)
  l <- lapply(lead, function(l_) {
    reindeer[[l_]]$points <<-
      reindeer[[l_]]$points + 1
  })
}) -> o


lapply(reindeer, `[[`, 'points') %>%
  unlist %>%
  max
