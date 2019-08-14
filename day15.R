

cook <- function(d, ...){
  s <- colSums(
    apply(d, 1, function(r) r * c(...))
  )
  if (any(s < 0)) 0 else Reduce(`*`, s)
}

score_cook <- function(d) {
  d <- trimws(gsub("[^-|0-9| ]", "", d))
  d <- lapply(strsplit(d, "  "), function(x) {
    as.numeric(x[-length(x)])
  })
  d <- matrix(unlist(d), nrow = 4)
  k <- do.call(expand.grid, lapply(1:ncol(d), function(k)
    1:100))
  scores <- apply(k, 1, function(x) {
    if (sum(x) != 100)
      return(0)
    cook(d, x)
  })
  max(scores)
}

readLines('data/day15bis.txt', warn = FALSE) %>% score_cook
readLines('data/day15.txt', warn = FALSE)    %>% score_cook

cook2 <- function(d, ...){
  s <- colSums(
    apply(d, 1, function(r) r * c(...))
  )
  if (s[length(s)] != 500) return(0)
  if (any(s < 0)) 0 else Reduce(`*`, s[-length(s)])
}

score_cal <- function(d) {
  d <- trimws(gsub("[^-|0-9| ]", "", d))
  d <- lapply(strsplit(d, "  "), as.numeric)
  d <- matrix(unlist(d), nrow = 5)
  k <- do.call(expand.grid, lapply(1:ncol(d), function(k)
    1:100))
  scores <- apply(k, 1, function(x) {
    if (sum(x) != 100)
      return(0)
    cook2(d, x)
  })
  max(scores)
}
readLines('data/day15bis.txt', warn = FALSE) %>% score_cal
readLines('data/day15.txt', warn = FALSE) %>% score_cal
