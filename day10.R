library(magrittr)

# part 1
looksay <- function(x){
  o <- character()
  n <- 1
  for (i in seq_along(x)){
    
    if (isTRUE(x[i] == x[i + 1])){
      n <- n + 1
      next
    } 
    
    o[length(o) + 1] <- n
    o[length(o) + 1] <- x[i]
    n <- 1
  }
  o
}

#slightly faster
looksay <- function(x){
  len <- length(x)
  o <- character(len + floor(len / 2))
  it <- 1
  n <- 1
  for (i in seq_along(x)){
    
    if (isTRUE(x[i] == x[i + 1])){
      n <- n + 1
      next
    } 
    
    o[it] <- n
    o[it + 1] <- x[i]
    it <- it + 2
    n <- 1
  }
  o[nzchar(o)]
}

rep_looksay <- function(x = "1", times = 5){
  for (i in 1:times) x <- looksay(x)
  length(x)
}

#via regex matching
looksay_rx <- function(s){
  
  rx <- gregexpr("(\\d)\\1*", s)
  m <- regmatches(s, rx)[[1]]
  
  s <- paste0(nchar(m), substr(m, 1, 1))
  paste(s, collapse = "")
}

rep_looksay_rx <- function(x = "1", times = 5) {
  for (i in 1:times) x <- looksay_rx(x)
  nchar(x)
}

#via rle
looksay_rle <- function(s){
  len <- rle(strsplit(s, "")[[1]])
  paste(paste0(len$lengths, len$values), collapse = "")
}

rep_looksay_rle <- function(x, times = 5){
  for (i in 1:times) x <- looksay_rle(x)
  nchar(x)
}


#part 1
strsplit("3113322113", "")[[1]] %>% 
  rep_looksay(times = 40) 

rep_looksay_rle("3113322113", 40)

#part 2
rep_looksay_rle("3113322113", 50)

strsplit("3113322113", "")[[1]] %>% 
  rep_looksay(times = 50) 
