
gl <- grepl
f_ <- function(f, ...){
  if (any(vapply(list(...), is.null, logical(1))))
    return(NULL)
  do.call(f, list(...))
}
`%_%` <- function(l, x) 
  if (grepl("\\D", x)) l[[x]] else as.numeric(x)

assemble <- function(x, r = list()){
  
  for (d in x){
    
    ds <- strsplit(d, " ")[[1]]
    target <- ds[length(ds)]
    if (target %in% names(r))
      next
    a <- ds[1]
    b <- ds[3]

    if (gl("NOT", d))
      r[[target]] <- 
      f_(function(x) bitwAnd(65535, bitwNot(x)), r %_% ds[2])
    
    else if (gl("OR", d))
      r[[target]] <- f_(bitwOr, r %_% a, r %_% b)

    else if (gl("AND", d))
      r[[target]] <- f_(bitwAnd, r %_% a, r %_% b)
    
    else if (gl("LSHIFT", d))
      r[[target]] <- f_(bitwShiftL, r %_% a, as.numeric(b))
    
    else if (gl("RSHIFT", d))
      r[[target]] <- f_(bitwShiftR, r %_% a, as.numeric(b))
    
    else
      r[[target]] <- r %_% a

  }

  r
}

x <- c(
  "123 -> x",
  "456 -> y",
  "x AND y -> d",
  "x OR y -> e",
  "x LSHIFT 2 -> f",
  "y RSHIFT 2 -> g",
  "NOT x -> h",
  "NOT y -> i"
)
r <- assemble(x)

get_wire <- function(d, r = list(), wire = "a"){
  r <- assemble(d, r)
  repeat{
    r <- assemble(d, r)
    if (length(r) == length(d))
      break
  }
  r[[wire]]
}

d <- readLines("data/day7.txt", warn = FALSE)
a <- get_wire(d)
#3176

#part 2
get_wire(d, r = list(b = a))

