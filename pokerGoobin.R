#Poker Game Simulator 0.1
#Matthew Monterosso 9/5/19



library(tidyverse)
library(merTools)

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

Deal_Players(4)






#####
Deck_Pre <- setdiff(deck, Deals)



####Deal Logic
Deal_Board <- function(street){
  
  if (street %in% c(1,2,3) = FALSE)stop("Please Choose Appropriate Street Value")  
  
  assign("Burn", sample_n(deck, 2 * street, replace = FALSE), envir = globalenv())
  
  
  if (steet == 1){
  assign("Flop", sample_n(deck, 3, replace = FALSE), envir = globalenv())
  }
  
}


#######


HandWin <- function(P1,...){
  
HandRank <- c("HighHand","OnePair","TwoPair","Trips/Set","Straight","Flush","FullHouse","Quads","StraightFlush")
RankVal <- 1:9  

Win_df <- as_tibble(cbind(RankVal,HandRank))    
  

  
  
}
