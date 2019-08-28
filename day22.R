
set_boss <- function(){
  list(
    hp = 71,
    damage = 10
  )
}

set_player <- function(){
  list(
    hp = 50,
    mana = 500
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

init_round <- function(player = NULL, boss = NULL, mode = NULL){
  
  list(
       state = 'playing',
       mode = mode %||% 'easy',
       winner = NULL,
       player =  player %||% set_player(),
       player_action = NULL,
       total_cost = 0,
       boss =  boss %||% set_boss(),
       armor = 0,
       active = NULL,
       effects = list(
         shield = list(
           timer = 6,
           cost = 113,
           armor = 7
         ),
         poison = list(
           timer = 6,
           cost = 173,
           damage = 3
         ),
         recharge = list(
           timer = 5,
           cost = 229,
           mana = 101
         )
       ),
       actions = list(
         magic = list(
           cost = 53, 
           damage = 4
         ),
         drain = list(
           cost = 73, 
           damage = 2,
           hp = 2
         )
       ))
  
}

effects <- function() c("shield", "poison", "recharge")

update_effects <- function(round){
  g <- round
  if ('shield' %in% g$active){
    g$effects$shield$timer <- g$effects$shield$timer - 1
    g$armor <- g$effects$shield$armor
    if (g$effects$shield$timer == 0){
      g$effects$shield$timer <- 6
      g$active <- g$active[g$active != 'shield']
      g$armor <- 0
    } 
  }
  
  if ('poison' %in% g$active){
    g$effects$poison$timer <- g$effects$poison$timer - 1
    g$boss$hp <- g$boss$hp - g$effects$poison$damage
    
    if (g$effects$poison$timer == 0){
      g$effects$poison$timer <- 6
      g$active <- g$active[g$active != 'poison']
    } 
  }
  
  if ('recharge' %in% g$active){
    g$effects$recharge$timer <- g$effects$recharge$timer - 1
    g$player$mana <- g$player$mana + g$effects$recharge$mana
    
    if (g$effects$recharge$timer == 0){
      g$effects$recharge$timer <- 5
      g$active <- g$active[g$active != 'recharge']
    } 
  }
  g
}

#activate effect
activate_effects <- function(round){
  a <- round$player_action
  round$active <- c(round$active, a)
  k <- round$effects[[a]]$cost
  round$total_cost <- round$total_cost + k
  round$player$mana <- round$player$mana - k
  round
}

check_round <- function(r){
  if (r$player$mana <= 0 || r$player$hp <= 0){
    r$state <- "finished"
    r$winner <- "boss"
    return(r)
  }
  if (r$boss$hp <= 0){
    r$state <- "finished"
    r$winner <- "player"
    return(r)
  }
  r
}

boss_turn <- function(round){
  
  round <- update_effects(round)
  round <- check_round(round)
  if (round$state == 'finished')
    return(round)
  
  d <- max(1, round$boss$damage - round$armor)
  round$player$hp <- round$player$hp - d
  
  check_round(round)
}

player_turn <- function(round){
  
  if (round$mode == 'hard')
    round$player$hp <- round$player$hp - 1
  
  round <- check_round(round)
  if (round$state == 'finished')
    return(round)
  
  round <- update_effects(round)
  round <- check_round(round)
  if (round$state == 'finished')
    return(round)

  if (round$player_action %in% effects()){
    round <- activate_effects(round)
    return(check_round(round))
  }
    
  if (round$player_action == "magic"){
    
    k <- round$actions$magic$cost
    round$total_cost <- round$total_cost + k
    round$player$mana <- round$player$mana - k
    
    round <- check_round(round)
    if (round$state == 'finished')
      return(round)
    
    round$boss$hp <- round$boss$hp - round$actions$magic$damage 
    return(check_round(round))
  }
  
  if (round$player_action == "drain"){
    
    k <- round$actions$drain$cost
    round$total_cost <- round$total_cost + k
    round$player$mana <- round$player$mana - k
    
    round <- check_round(round)
    if (round$state == 'finished')
      return(round)

    round$boss$hp <- round$boss$hp - round$actions$drain$damage 
    round$player$hp <- round$player$hp + round$actions$drain$hp
    return(check_round(round))
  }  
}

play_round <- function(round, player_action){
  round$player_action <- player_action
  round <- player_turn(round)
  if (round$state == 'finished')
    return(round)
  boss_turn(round)
}

#example 1
# g <- init_round(player = list(hp = 10, mana = 250), boss = list(hp = 13, damage = 8))
# g <- play_round(g, 'poison')
# g <- play_round(g, 'magic')
# 
# #example2
# g <- init_round(player = list(hp = 10, mana = 250), boss = list(hp = 14, damage = 8))
# g <- play_round(g, 'recharge')
# g <- play_round(g, 'shield')
# g <- play_round(g, 'drain')
# g <- play_round(g, 'poison')
# g <- play_round(g, 'magic')
# 
# costs <- function() c(113, 173, 229, 73, 53)

costs <- function() setNames(
  c(113, 173, 229, 73, 53),
  c("shield", "poison", "recharge", "drain", "magic")
)

min_mana <- function(g = init_round()) {
  M <- numeric(0)
  find_min_mana <- function(g) {
    if (g$state == 'finished') {
      if (g$winner == 'player')
        M <<- min(M, g$total_cost)
      return(NULL)
    }
    if (isTRUE(g$total_cost > M))
      return(NULL)
    
    k <- costs()
    actions <- names(k[k <= g$player$mana])
    
    #can use effects that will end next
    ga <- g$active
    ga <-
      ga[!vapply(ga, function(a)
        g$effects[[a]]$timer == 1, logical(1))]
    actions <- actions[!actions %in% ga]
    for (a in actions)
      find_min_mana(play_round(g, a))
    
  }
  find_min_mana(g)
  M
}
#part 1
min_mana()

#part 2 
min_mana(init_round(mode = 'hard'))

