pkg <- strsplit(
  '1
2
3
5
7
13
17
19
23
29
31
37
41
43
53
59
61
67
71
73
79
83
89
97
101
103
107
109
113',
  split = "\n"
)[[1]]
pkg <- as.numeric(pkg)
pkg <- sort(pkg, decreasing = TRUE)

first_pkg <- function(pkg, group = numeric(0), wt = sum(pkg) / 3, len){
  grp <- list()
  seen <- numeric()
  pk <- function(pkg, group, wt, len){
    
    if (length(group) > len)
      return()
    
    d <- wt - sum(group)
    
    if (isTRUE(d %in% pkg)){
      grp[[length(grp) + 1]] <<- sort(c(group, d))
      grp <<- unique(grp)
    }
    
    if (isTRUE(d < 0))
      return()
    
    for (i in seq_along(pkg)){
      x <- pkg[i]
      seen <<- c(seen, x)
      g <- c(group, x)
      rem <- pkg[-i]
      rem <- rem[!rem %in% seen]
      rem <- rem[ (sum(g) + rem) <= wt ]
      if (sum(rem) + sum(g) < wt)
        next
      pk(rem, g, wt, len)
    }
    
  }
  pk(pkg, group, wt, len)
  grp
  
}

min_len <- function(pkg, wt = sum(pkg) / 3){
  n <- 1
  pkg <- sort(pkg, decreasing = TRUE)
  repeat{
    if (sum(pkg[1:n]) >= sum(pkg) / 3)
      break
    n <- n + 1
  }
  n
}

min_qe <- function(g)
  min(vapply(g, function(d) Reduce(`*`, d), numeric(1)) )

#part 1
g <- first_pkg(pkg, len = min_len(pkg))
min_qe(g)

#part 2
wt2 <- sum(pkg) / 4
g <- first_pkg(pkg, wt = wt2, len = min_len(pkg, wt2))
min_qe(g)