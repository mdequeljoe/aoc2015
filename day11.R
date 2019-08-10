
has_seq <- function(v){
  l <- paste(letters, collapse = "")
  for (i in 1:(length(v) - 2)) {
    m <- grepl(paste(v[i:(i + 2)], collapse = ""), l)
    if (m) return(TRUE)
  }
  FALSE
}

has_invalid_char <- function(v) any(v %in% c("i", "o", "l"))

has_pairs <- function(v, n = 2){
  m <- v == c(v[-1], "") & v != c("", v[-length(v)])
  length(m[m]) >= n
}

is_valid_pass <- function(v){
  if (has_invalid_char(v))
    return(FALSE)
  if (!has_pairs(v))
    return(FALSE)
  
  has_seq(v)
}

increment_letter <- function(l){
  l <- which(letters == l) %% 26 + 1
  letters[l]
}

# ... "xyz" "xza" ...
increment_pass <- function(v){
  n <- length(v)
  v[n] <- increment_letter(v[n])
  for (i in (n - 1):1){
    if (v[i + 1] == "a")
      v[i] <- increment_letter(v[i])
    else
      break
  }
  v
}

is_expired <- function(v, expired)
  any(vapply(expired, identical, logical(1), v))

get_new_pass <- function(s, expired = ""){
  v <- strsplit(s, "")[[1]]
  expired <- strsplit(expired, "")
  repeat{
    v <- increment_pass(v)
    if (is_valid_pass(v) && !is_expired(v, expired))
      break
  }
  paste(v, collapse = "")
}

# get_new_pass("abcdefgh") 

#part 1
get_new_pass("hepxcrrq")
#hepxxyzz

#part 2
get_new_pass("hepxcrrq", expired = "hepxxyzz")
#heqaabcc

