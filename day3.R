

move_to <- function(x){
  list(
    ">" = c(1, 0),
    "<" = c(-1, 0),
    "^" = c(0, 1),
    "v" = c(0, -1)
  )[[x]]
}

track <- function(x){
  if (length(x) == 1) x <- strsplit(x, "")[[1]]
  path <- vector("list", length(x) + 1)
  path[[1]] <- c(0, 0)
  for (i in seq_along(x)){
    path[[i + 1]] <- path[[i]] + move_to(x[i])
  }
  path
}


len <- function(l) length(unique(l))
track("^>v<")  %>% len
track(">") %>% len
track("^v^v^v^v^v") %>% len

rl <- readLines('data/day3.txt')
track(rl) %>% len

track2 <- function(x){
  x <- strsplit(x, "")[[1]]
  c(
    track(x[seq(x) %% 2 == 1]),
    track(x[seq(x) %% 2 == 0])
  )
}

track2("^>")  %>% len
track2("^>v<")  %>% len
track2("^v^v^v^v^v")  %>% len
track2(rl) %>% len

