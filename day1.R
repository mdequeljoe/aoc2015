

walk <- function(x){
  up <- nchar(gsub("\\)", "", x))
  up - (nchar(x) - up)
}

walk("(())") == 0
walk("()()") == 0
walk("))(((((") == 3
walk(")())())") == -3

d <- readLines('data/day1.txt')
walk(d)


walk2 <- function(x){
  x <- strsplit(x, "")[[1]]
  l <- 0
  for (i in seq_along(x)) {
    l <- if (x[i] == "(") l + 1 else l - 1
    if (l == -1)
      break
  }
  i
}

walk2("(") == 1
walk2("()())") == 5
walk2(d)
