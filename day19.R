

d <- readLines('data/day19.txt', warn = FALSE)
s <- d[length(d)]
d <- d[-length(d)]
d <- d[nzchar(d)]

mutate_mol <- function(rp, s) {
  o <- character()
  rx <- "^(.+) => (.+)"
  key <- gsub(rx, "\\1", rp)
  val <- gsub(rx, "\\2", rp)
  
  for (k in seq_along(key)) {
    m <- gregexpr(key[k], s)[[1]]
    if (any(m < 0))
      next
    
    for (i in m) {
      o[length(o) + 1] <-
        paste0(substring(s, 1, i - 1),
               val[k],
               substring(s, i + nchar(key[k])))
    }
  }
  unique(o)
}




# part 1
mutate_mol(d, s) %>% length


#part 2

cnt_steps <- function(d, s){

  rx <- "^(.+) => (.+)"
  key <- gsub(rx, "\\1", d)
  val <- gsub(rx, "\\2", d)
  cnt <- 0
  #reduce molecule 
  repeat{
    m <- vapply(val, function(v) {
      max(gregexpr(sprintf("%s", v), s)[[1]])
    }, integer(1))
    m <- m[m > 0]
    m <- m[m == min(m)]
    if (length(m) > 1){
      nk <- nchar(names(m))
      m <- m[nk == max(nk)][1]
    }
    v <- val[val == names(m)]
    k <- key[val == v]
    cnt <- cnt + 1
    if (k == "e")
      break
    s <- paste0(
      substring(s, 1, m - 1),
      k,
      substring(s, m + nchar(v))
    )
  }
  cnt
}

rp <- 'e => H
e => O
H => HO
H => OH
O => HH'
rp <- strsplit(rp, "\n")[[1]]
cnt_steps(rp, 'HOHOHO')

cnt_steps(d, s)
