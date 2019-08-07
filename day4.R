

mine_coin <- function(key, rx = "^0{5, }"){
  i <- -1
  repeat{
    i <- i + 1
    d <- digest::digest(sprintf("%s%d", key, i), serialize = FALSE)
    m <- grepl(rx, d)
    if (m)
      break
  }
  c(i, d)
}

mine_coin("abcdef")
mine_coin('pqrstuv')
mine_coin('bgvyzdsv')
mine_coin('bgvyzdsv', "^0{6}")


