
d <- readLines('data/day16.txt', warn = FALSE)
d <- regmatches(d, gregexpr("[a-z]+:\\s[0-9]+", d))
input <- "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
"
input <- strsplit(input, "\n")[[1]]
lapply(d, function(x) {
  all(x %in% input)
}) %>%
  unlist %>%
  which

#part 2
eqvar <- c("cats", 'trees', 'goldfish', 'pomeranians')
eq <- names(input)[!names(input) %in% eqvar]

as_vec <- function(x) {
  nm <- regmatches(x, gregexpr("[a-z]+", x))
  val <- regmatches(x, gregexpr("[0-9]+", x))
  val <- lapply(val, as.numeric)
  names(val) <- nm
  unlist(val)
}

input <- as_vec(input)
eqvar <- c("cats", 'trees', 'goldfish', 'pomeranians')
eq <- names(input)[!names(input) %in% eqvar]

`%na>%` <- function(a, b) if (is.na(a)) TRUE else a > b
`%na<%` <- function(a, b) if (is.na(a)) TRUE else a < b

lapply(d, function(x) {
  x <- as_vec(x)
  nm <- names(x)
  nm <- nm[!nm %in% eqvar]
  x['cats'] %na>% input['cats'] &&
    x['trees'] %na>% input['trees'] &&
    x['goldfish'] %na<% input['goldfish'] &&
    x['pomeranians'] %na<% input['pomeranians'] &&
    all(x[nm] == input[nm])
})  %>%
  unlist %>%
  which



