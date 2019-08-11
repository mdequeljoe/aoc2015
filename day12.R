library(magrittr)

#part 1
d <- readLines('data/day12.txt', warn = FALSE)
d <- gsub("[^-|0-9]", " ", d)
d <- gsub("\\s+", " ", d)
strsplit(trimws(d), " ")[[1]] %>% 
  as.numeric() %>% 
  sum()

#part 2
d <- readLines('data/day12.txt', warn = FALSE)

sym <- function() c("{", "[", "]", "}")
find_next_sym <- function(d, m, type = c("open", "close")) {
  type <- match.arg(type)
  s <- list(open = list(step = `-`, msym = sym()[3:4]),
            close = list(step = `+`, msym = sym()[1:2]))[[type]]
  i <- m
  lvl <- 1
  repeat {
    i <- s$step(i, 1)
    if (!d[i] %in% sym())
      next
    if (d[i] %in% s$msym) {
      lvl <- lvl + 1
      next
    }
    lvl <- lvl - 1
    if (lvl == 0)
      break
  }
  list(sym = d[i], ind = i)
}

rm_red <- function(d) {
  d <- gsub("[^-0-9{}[\\]|red]", " ", d, perl = TRUE)
  d <- gsub("ree| e | r | d ", "", d)
  d <- gsub("\\s+", " ", d)
  for (k in c("\\{", "\\}", "\\]", "\\["))
    d <- gsub(k, paste0(" ", k, " "), d)
  d <- strsplit(trimws(d), " ")[[1]]
  markers <- which(d == "red")
  rng <- numeric()
  for (m in markers) {
    if (m %in% rng)
      next
    op <- find_next_sym(d, m)
    if (op$sym == "[")
      next
    cl <- find_next_sym(d, m, 'close')
    rng <- c(rng, op$ind:cl$ind)
  }
  
  if (length(rng)) d[-rng] else d
}

'[1,{"c":"red","b":2},3]' %>% rm_red
'{"d":"red","e":[1,2,3,4],"f":5}' %>% rm_red
'[1,"red",5]' %>% rm_red

readLines('data/day12.txt', warn = FALSE) %>% 
  rm_red %>%
  .[grepl("\\d", .)] %>% 
  as.numeric() %>% 
  sum
