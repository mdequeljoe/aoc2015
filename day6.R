

coords <- function(d, i)
  as.numeric(strsplit(d[i], ",")[[1]]) + 1

light <- function(x) {
  m <- matrix(data = 0,
              nrow = 1000,
              ncol = 1000)
  for (d in x) {
    d <- strsplit(d, " ")[[1]]
    
    if (d[1] == "toggle") {
      from <- coords(d, 2)
      to <- coords(d, 4)
      value <- m[from[1]:to[1], from[2]:to[2]]
      k0 <- value == 0
      k1 <- value == 1
      value[k0] <- 1
      value[k1] <- 0
    } else {
      from <- coords(d, 3)
      to <- coords(d, 5)
      value <- if (d[2] == "on") 1 else 0
    } 
    
    m[from[1]:to[1], from[2]:to[2]] <- value
  }
  length(m[m == 1])
}

x <- c(
  "turn on 0,0 through 999,999",
  "toggle 0,0 through 999,0",
  "turn off 499,499 through 500,500"
)
light(x)
d <- readLines('data/day6.txt', warn = FALSE)
light(d)


light2 <- function(x) {
  m <- matrix(data = 0,
              nrow = 1000,
              ncol = 1000)
  for (d in x) {
    d <- strsplit(d, " ")[[1]]
    
    if (d[1] == "toggle") {
      from <- coords(d, 2)
      to <- coords(d, 4)
      value <- 2
    } else {
      from <- coords(d, 3)
      to <- coords(d, 5)
      value <- if (d[2] == "on") 1 else -1
    }
    
    m[from[1]:to[1], from[2]:to[2]] <-
      m[from[1]:to[1], from[2]:to[2]] + value
    m[m < 0] <- 0
  }
  sum(m)
}


d <- readLines('data/day6.txt', warn = FALSE)
light2(d)
