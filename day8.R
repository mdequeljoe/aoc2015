

d <- readLines("data/day8bis.txt", warn = FALSE)
d <- readLines("data/day8.txt", warn = FALSE)

# part 1
sum(nchar(d) - 
      (nchar(stringi::stri_unescape_unicode(d)) - 2))

#without stringi
parse_strings <- function(strings) {
  vapply(strings, function(s)
    eval(parse(text = s)), 
    character(1),
    USE.NAMES = FALSE)
}

sum(nchar(d) - nchar(parse_strings(d)))


#part 2
e <-
  vapply(d, function(k)
    nchar(deparse(k)), numeric(1), USE.NAMES = FALSE)
sum(e - nchar(d))
