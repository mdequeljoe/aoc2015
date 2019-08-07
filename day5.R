


has_three_vowels <- function(x)
  length(x[x %in% c("a", "e", "i", "o", "u")]) >= 3

has_one_rep <- function(x)
  any(x[-length(x)] == x[-1])

is_nice_string <- function(x){
  if (grepl("ab|cd|pq|xy", x))
    return(FALSE)
  x <- strsplit(x, '')[[1]]
  has_three_vowels(x) &&
    has_one_rep(x) 
}

"ugknbfddgicrmopn"  %>% is_nice_string()
"aaa"               %>% is_nice_string()
"jchzalrnumimnmhp"  %>% is_nice_string()
"haegwjzuvuyypxyu"  %>% is_nice_string()
"dvszwmarrgswjxmb"  %>% is_nice_string()

d <- readLines("data/day5.txt", warn = FALSE)
s <- vapply(d, is_nice_string, logical(1))
length(s[s])

#part 2

has_pair_rep <- function(x) {
  for (i in seq_along(x)) {
    if (i == (length(x) - 1))
      break
    
    pair <- paste0(x[i:(i + 1)], collapse = "")
    m <- paste0(x[(i + 2):length(x)], collapse = "")
    
    if (grepl(pair, m))
      return(TRUE)
  }
  FALSE
}

has_submirror <- function(x){
  for (i in seq_along(x)){
    if (i == (length(x) - 1))
      break

    rng <- i:(i + 2)
    if (all(x[rng] == rev(x[rng])))
      return(TRUE)
  }
  FALSE
}

is_nice_string <- function(x){
  x <- strsplit(x, "")[[1]]
  has_pair_rep(x) && has_submirror(x)
}


"qjhvhtzxzqqjkmpb" %>% is_nice_string()
"xxyxx" %>% is_nice_string()
"uurcxstgmygtbstg" %>% is_nice_string()
"ieodomkazucvgmuy"%>% is_nice_string()

s <- vapply(d, is_nice_string, logical(1))
length(s[s])

