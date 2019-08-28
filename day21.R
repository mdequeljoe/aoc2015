
set_store <- function() {
  'Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3' -> store
  store <- strsplit(store, '\n')[[1]]
  store <- strsplit(store, "\\s+")
  store <- list(
    weapons = do.call(rbind, store[2:6]),
    armor = do.call(rbind, store[9:13]),
    rings = do.call(rbind, store[16:21])
  )
  store$rings[, 1] <- paste0(store$rings[, 1], store$rings[, 2])
  store$rings <- store$rings[, c(1, 3:5)]
  store$rings <- rbind(store$rings, c('none', 0, 0, 0))
  store$armor <- rbind(store$armor, c('none', 0, 0, 0))
  lapply(store, function(s) {
    s <- as.data.frame(s, stringsAsFactors = FALSE)
    s[, 2:4] <- vapply(s[, 2:4], as.numeric, numeric(nrow(s)))
    s
  })
}

set_boss <- function() {
  list(hp = 104,
       damage = 8,
       armor = 1)
}


play_turn <- function(u, b, i) {
  j <- if (i == 1)
    2
  else
    1
  o <- list(
    u = u,
    b = b,
    state = 'playing',
    next_turn = j
  )
  o[[j]]$hp <- o[[j]]$hp - max(1, o[[i]]$damage - o[[j]]$armor)
  if (o[[j]]$hp <= 0)
    o$state <- 'finished'
  o
}

play <- function(u, b = NULL) {
  i <- 1
  if (is.null(b))
    b <- set_boss()
  repeat {
    m <- play_turn(u, b, i)
    if (m$state == 'finished') {
      break
    }
    u <- m$u
    b <- m$b
    i <- m$next_turn
  }
  m
}


play(list(hp = 8, damage = 5,  armor = 5), list(hp = 12, damage = 7, armor = 2))

#part 1
s <- set_store()
g <-
  expand.grid(s$weapons$V1,
              s$armor$V1,
              s$rings$V1,
              s$rings$V1,
              stringsAsFactors = FALSE)
cost <- numeric(nrow(g))
win <- logical(nrow(g))
for (i in 1:nrow(g)) {
  w <- s$weapons[s$weapons$V1 == g[i, 1], ]
  a <- s$armor[s$armor$V1 == g[i, 2], ]
  rg <- s$rings[s$rings$V1 == g[i, 3], ]
  rg2 <- s$rings[s$rings$V1 == g[i, 4], ]
  if (rg2 == rg && rg != "none")
    next
  cost[i] <- w[2] + a[2] + rg[2] + rg2[2]
  u <- list(
    hp = 100,
    damage = as.numeric(w[3] + rg[3] + rg2[3]),
    armor = as.numeric(a[4] + rg[4] + rg2[4])
  )
  p <- play(u)
  if (p$b$hp <= 0)
    win[i] <- TRUE
}

k <- unlist(cost)
#part 1
min(k[win])

#part 2
max(k[!win])