
input <- 36e6
find_house <- function(input){
  x <- input / 10
  o <- numeric(x)
  for (i in seq_len(x)){
    rng <- seq(i, x, i)
    o[rng] <- o[rng] + i * 10
  }
  which(o >= input)[1]
}

find_house(input)

find_house2 <- function(input){
  x <- input / 10
  o <- numeric(x)
  for (i in seq_len(x)){
    rng <- seq(i, x, i)
    rng <- rng[1:min(50, length(rng))]
    o[rng] <- o[rng] + i * 11
  }
  which(o >= input)[1]
}

find_house2(input)



