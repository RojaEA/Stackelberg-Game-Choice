remove(list = ls())
library(tidyverse)
library(dplyr)
library(purrr)
library(pipeR)
library(data.table)
library(readr)

# Data --------------------------------------------------------------------
df <- read.table("Ham_utility_all.csv", dec = ".", header = TRUE, sep = ";")
# Validation on specific samples
#df <- read.table("UtilityallHamburg_Validation.csv", dec = ",", header = TRUE, sep = ";")

# Compute the utility with a set of estimates
theta = c(1.95,1.14,1.14,1.65,1.8,1.8, 1.8)
#theta optimised
#theta = c(2.2913632, 0.6736579, 1.5095670, 1.8612830, 1.4944622, 1.4327125, 1.1717086)
df <- df %>%
mutate(utility = exp(theta[1]*Safety + theta[2]*Time + theta[3]*Detour +
                     theta[4]*Dec + theta[5]*Group + theta[6]*Lane + theta[7]*Row))

# Game Strategy
Total_Strategy = c("contc_contp", "contc_decp", "contc_leftdevp", "contc_rightdevp",
                   "decc_contp", "decc_decp", "decc_leftdevp", "decc_rightdevp")

# Split df into separate conflict tibbles
conflict_list <- split(df, df$ID)

# Create an empty list
Result_List <- list()

# Iterate over conflict_list and add column Prob_User
 for (i in 1:length(conflict_list)) {

  conflict <- conflict_list[[i]]

  conflict$Prob_User <- as.numeric("")

  Total_U_carL_carP <- conflict %>%
    filter(leader == "car" & player == "car") %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_pedL_pedP <- conflict %>%
    filter(leader == "ped" & player == "ped") %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_carL_pedP_contc <- conflict %>%
    filter(leader == "car" & player == "ped" & Total_Strategy %in% c("contc_contp", "contc_decp", "contc_leftdevp",
                                                                     "contc_rightdevp")) %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_carL_pedP_decc <- conflict %>%
    filter(leader == "car" & player == "ped" & Total_Strategy %in% c("decc_contp", "decc_decp",
                                                                     "decc_leftdevp", "decc_rightdevp")) %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_pedL_carP_contp <- conflict %>%
    filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_contp", "decc_contp")) %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_pedL_carP_decp <- conflict %>%
    filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_decp", "decc_decp")) %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_pedL_carP_leftdevp <- conflict %>%
    filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_leftdevp", "decc_leftdevp")) %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  Total_U_pedL_carP_rightdevp <- conflict %>%
    filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_rightdevp", "decc_rightdevp")) %>%
    select(utility) %>%
    pull(.) %>%
    sum(.)

  for (j in 1:nrow(conflict)) {

    if (conflict$leader[j] == "car" & conflict$player[j] == "car") 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_carL_carP }

    else if (conflict$leader[j] == "car" & conflict$player[j] == "ped" & conflict$Total_Strategy[j] %in% c("contc_contp", "contc_decp",
                                                                                                        "contc_leftdevp", "contc_rightdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_carL_pedP_contc }

    else if (conflict$leader[j] == "car" & conflict$player[j] == "ped" & conflict$Total_Strategy[j] %in% c("decc_contp", "decc_decp",
                                                                                                        "decc_leftdevp", "decc_rightdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_carL_pedP_decc }

    else if (conflict$leader[j] == "ped" & conflict$player[j] == "ped") 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_pedP }

    else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_contp", "decc_contp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_contp }

    else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_decp", "decc_decp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_decp }

    else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_leftdevp", "decc_leftdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_leftdevp }

    else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_rightdevp", "decc_rightdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_rightdevp }

    
  }
# Compute the probability of strategy pairs in the game
  for (x in 1:length(conflict)) {

    conflict$Prob_User <- as.numeric(conflict$Prob_User)

    conflict$Prob_Game <- as.numeric("")

    Total_Prob_contc_contp <- conflict %>%
    filter(Total_Strategy == "contc_contp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_contc_decp <- conflict %>%
    filter(Total_Strategy == "contc_decp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_contc_leftdevp <- conflict %>%
    filter(Total_Strategy == "contc_leftdevp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_contc_rightdevp <- conflict %>%
    filter(Total_Strategy == "contc_rightdevp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_decc_contp <- conflict %>%
    filter(Total_Strategy == "decc_contp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_decc_decp <- conflict %>%
    filter(Total_Strategy == "decc_decp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_decc_leftdevp <- conflict %>%
    filter(Total_Strategy == "decc_leftdevp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    Total_Prob_decc_rightdevp <- conflict %>%
    filter(Total_Strategy == "decc_rightdevp") %>%
    select(Prob_User) %>%
    pull(.) %>%
    prod(.)

    for (k in 1:nrow(conflict)) {
      if (conflict$Total_Strategy[k] == "contc_contp") 
      {conflict$Prob_Game[k] = Total_Prob_contc_contp} 
      
      else if (conflict$Total_Strategy[k] == "contc_decp") 
      {conflict$Prob_Game[k] = Total_Prob_contc_decp}
      
      else if (conflict$Total_Strategy[k] == "contc_leftdevp") 
      {conflict$Prob_Game[k] = Total_Prob_contc_leftdevp}
      
      else if (conflict$Total_Strategy[k] == "contc_rightdevp") 
      {conflict$Prob_Game[k] = Total_Prob_contc_rightdevp}
      
      else if (conflict$Total_Strategy[k] == "decc_contp") 
      {conflict$Prob_Game[k] = Total_Prob_decc_contp}
      
      else if (conflict$Total_Strategy[k] == "decc_decp") 
      {conflict$Prob_Game[k] = Total_Prob_decc_decp}
      
      else if (conflict$Total_Strategy[k] == "decc_leftdevp") 
      {conflict$Prob_Game[k] = Total_Prob_decc_leftdevp}
      
      else if (conflict$Total_Strategy[k] == "decc_rightdevp") 
      {conflict$Prob_Game[k] = Total_Prob_decc_rightdevp}
      
      
    }
# Compute the Game outcome
    for (y in 1:length(conflict)){
      
      conflict$Prob_Game <- as.numeric(conflict$Prob_Game)
      
      conflict$Choice <- as.numeric("")
      
      Total_Prob_Outcome_choice  <-  conflict %>% 
        select(Prob_Game) %>% 
        pull(. ) %>% 
        max(.)
      
      for (n in 1:nrow(conflict)) {
        
        if (conflict$Prob_Game[n] >= Total_Prob_Outcome_choice) 
           {conflict$Choice[n] = 1} 
        
        else {conflict$Choice[n] = 0} 
      }
    }
  }
  Result_List[[i]] <- conflict
 }


alldataFrame <- bind_rows(Result_List)
write.csv(alldataFrame, file = "Game_Outcome.csv")



