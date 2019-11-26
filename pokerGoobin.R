#Poker Game Simulator 0.1
#Matthew Monterosso 9/5/19



library(tidyverse)
library(merTools) # why?

suits <- c("h","s","d","c")
card <- c(2:10,"J","Q","K","A")
value <- 2:14

deck <- expand.grid(card, suits)

colnames(deck) <- c("card", "suit")

deck <- cbind(deck,value)




Deal_Players <- function(n){

  if (n %% 1 != 0)stop("Please Use Integer Between 1 and 9")
  if (n < 1 | n > 9)stop("Please Use Integer Between 1 and 9")


assign("Deals", sample_n(deck, 2 * n, replace = FALSE), envir = globalenv())



for (i in 1:n){

assign(paste0("P", i ), as_tibble(rbind(Deals[i,],Deals[i + n,])), envir = globalenv())


}
}
#####

Deal_Players(1)






#####
Deck_Pre <- setdiff(deck, Deals)
#Deck_Flop <- setdiff(Deck_Pre, Deals)


####Deal Logic
Deal_Board <- function(street){

  #if (street != 1 | street != 2 | street != 3)stop("Please Choose Appropriate Street Value")

  # assign("Burn", sample_n(deck, street, replace = FALSE), envir = globalenv())
  #
  # assign("deck", setdiff(deck, Burn))


  ##have to pick only 1 for now

   if (street == 1){
   assign("Flop", sample_n(deck, 3, replace = FALSE), envir = globalenv())
   }

   if (street == 2){
     assign("Turn", sample_n(deck, 4, replace = FALSE), envir = globalenv())
   }

   if (street == 3){
     assign("River", sample_n(deck, 5, replace = FALSE), envir = globalenv())
   }


}


Deal_Board(street = 1)



###Ideas as I make this

# 1. Drought-assist - model likihood of having consistently poor hands for how long - every hand
# indepenent event obvs but chances of not getting any playable hand diminish over sufficient sample.

# 2. M O I S T - examing board texture and how common wet vs dry boards occur (and quantify what this even means), and chance for dry to become moist on T/R ("Hottness" via flopzilla)

# 3. Re-calculate best hands via 1MM+ simluation with all hand combos
#######



##Start low - search toward high, wont overwrite better hands

## calc if pair before search high hand?


## All seven cards search for pair - n_distinct value or card >= 6 , must be pair,

# n_distinct == 5 must be two pair (or better but find this first)

# Could also do group by card , summarize n = n() for a lot of this logic

# straights - hardest lemme do some thinking

# group by suit if n >= 5 must be flush

# quads ez

# sf hmmmmm





HandWin <- function(P1,...){

HandRank <- c("HighHand","OnePair","TwoPair","Trips/Set","Straight","Flush","FullHouse","Quads","StraightFlush")
RankVal <- 1:9


player_hand <- rbind(Flop, Deals) %>% arrange(-value)




player_hand_reduce_card <- as.data.frame(player_hand) %>%
  group_by(card) %>%
  summarise(n = n()) %>% arrange(-n)

player_hand_reduce_suit <- player_hand %>%
  group_by(suit) %>%
  summarise(n = n()) %>% arrange(-n)


player_hand_rank <- case_when(max(player_hand_reduce_card$n) == 1 ~ "High Hand",
                              max(player_hand_reduce_card$n) == 2 ~ "One Pair",
                              player_hand_reduce_card[1:2] == 2 ~ "Two Pair", ## Does this work
                              max(player_hand_reduce_card$n) == 3 ~ "Trips/Set",

                              #bad straight logic surely can be improved ALSO no wheel logic yet
                                player_hand$value[1] - player_hand$value[2] == 1 &
                                player_hand$value[2] - player_hand$value[3] == 1 &
                                player_hand$value[3] - player_hand$value[4] == 1 &
                                player_hand$value[4] - player_hand$value[5] == 1 ~ "Straight",


                              player_hand_reduce_suit$n >= 5 ~ "Flush",
                              max(player_hand_reduce_card$n) == 3 & player_hand_reduce_card$n[2] == 2 ~ "Full House",
                              max(player_hand_reduce_card$n) == 4 ~ "Quads",

                                player_hand$value[1] - player_hand$value[2] == 1 &
                                player_hand$value[2] - player_hand$value[3] == 1 &
                                player_hand$value[3] - player_hand$value[4] == 1 &
                                player_hand$value[4] - player_hand$value[5] == 1 &
                                player_hand_reduce_suit$n >= 5 ~ "Straight Flush")



Win_df <- as_tibble(cbind(RankVal,HandRank))




}
