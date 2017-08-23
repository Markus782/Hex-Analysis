#load json
library(jsonlite)
library(tidyr)

#load initial data set - path = json data
load_initial_data = function(pathZ=paste(Sys.Date()-1,".json", sep=""), with_archetypes = F) {
    
    hexData = fromJSON(pathZ)
    ladderData = hexData[hexData$TournamentType=="Ladder",]
    
    hexDataframe = data.frame(hero=character(),wins=integer(),loses=integer(),cards=character(), stringsAsFactors = F)
    mapping = stream_in(file("guid_name_mapping_array.json"))
    
    for(i in 1:length(ladderData[[1]])) {
        
        currGame = ladderData$Games[[i]]$Matches[[1]] 
    
        p1champ = mapping[mapping$id==currGame$PlayerOneDeck$Champion,2]
        p2champ = mapping[mapping$id==currGame$PlayerTwoDeck$Champion,2]
        p1wins = currGame$PlayerOneWins
        p2wins = currGame$PlayerTwoWins
        p1deck = currGame$PlayerOneDeck$Deck[[1]][[1]]
        p1deck = mapping[match(p1deck,mapping[[1]]),2]
        p2deck = currGame$PlayerTwoDeck$Deck[[1]][[1]]
        p2deck = mapping[match(p2deck,mapping[[1]]),2]
        if (length(currGame$PlayerOneDeck$Sideboard[[1]]) != 0) {
            p1reserves = currGame$PlayerOneDeck$Sideboard[[1]][[1]]
            p1reserves = mapping[match(p1reserves,mapping[[1]]),2]
        } else {
            p1reserves = 0
        }
        
        if(length(currGame$PlayerTwoDeck$Sideboard[[1]]) != 0) {
            p2reserves = currGame$PlayerTwoDeck$Sideboard[[1]][[1]]
            p2reserves = mapping[match(p2reserves,mapping[[1]]),2]
        } else {
            p2reserves = 0
        }
            
        if(length(p1deck)==60 && length(p1reserves)==15){
        hexDataframe = rbind(hexDataframe, data.frame(hero=p1champ, wins=p1wins, loses=p2wins, cards=p1deck, 
                                                      stringsAsFactors = F))
        hexDataframe = rbind(hexDataframe, data.frame(hero=p1champ, wins=p1wins, loses=p2wins, cards=p1reserves, 
                                                      stringsAsFactors = F))
        print(paste(mapping[mapping$id==currGame$PlayerOneDeck$Champion,2]," ",i))
        }
        
        if(length(p2deck)==60 && length(p2reserves)==15) {
        hexDataframe = rbind(hexDataframe, data.frame(hero=p2champ, wins=p2wins, loses=p1wins, cards=p2deck, 
                                                      stringsAsFactors = F))
        hexDataframe = rbind(hexDataframe, data.frame(hero=p2champ, wins=p2wins, loses=p1wins, cards=p2reserves, 
                                                      stringsAsFactors = F))
        print(paste(mapping[mapping$id==currGame$PlayerOneDeck$Champion,2]," ",i))
        }
    }
    if(with_archetypes == T){
        hexDataframe = create_archetypes(hexDataframe)
        hexDataframe$archetype = unlist(hexDataframe$archetype)
        return(hexDataframe)
    }
    else{
        return(hexDataframe)
    }
}

#create win overview by heroes
create_wins_overview_by_hero = function(inputDF, order=T) {
    matchResults = cbind(Hero=inputDF[,1],Result=paste(inputDF[,2],"-",inputDF[,3]))
    outputDF = as.data.frame(table(matchResults[,1],matchResults[,2])/75)
    outputDF = spread(outputDF, Var2, Freq)
    names(outputDF) = c("Hero", " 0-2", " 1-2", " 2-0", " 2-1")
    game_wins = outputDF$' 2-0'*2 + outputDF$' 2-1'*2 + outputDF$' 1-2'*1
    game_losses = outputDF$' 0-2'*2 + outputDF$' 1-2'*2 + outputDF$' 2-1' 
    match_wins = outputDF$' 2-0' + outputDF$' 2-1'
    match_losses = outputDF$' 1-2' + outputDF$' 0-2'
    game_win_perc = game_wins / (game_losses+game_wins)
    match_win_perc = match_wins / (match_losses+match_wins)
    outputDF = cbind(outputDF, 'Game Wins'=game_wins, 'Game Losses'=game_losses,'Game Win%'=game_win_perc,
                     'Match Wins'=match_wins, 'Match Losses'=match_losses, 'Match Win%'=match_win_perc)
    if(order == T) {
    outputDF = outputDF[order(-outputDF$`Match Win%`*outputDF$`Match Wins`),]
    }
    #ordered by match win% times match wins
    print(outputDF)
}

#create win overview by archetypes
create_wins_overview_by_archetype = function(inputDF, order=T) {
        matchResults = cbind(Archetype=inputDF[,5],Result=paste(inputDF[,2],"-",inputDF[,3]))
        outputDF = as.data.frame(table(unlist(matchResults[,1]),unlist(matchResults[,2]))/75)
        outputDF = spread(outputDF, Var2, Freq)
        names(outputDF) = c("Archetype", " 0-2", " 1-2", " 2-0", " 2-1")
        game_wins = outputDF$' 2-0'*2 + outputDF$' 2-1'*2 + outputDF$' 1-2'*1
        game_losses = outputDF$' 0-2'*2 + outputDF$' 1-2'*2 + outputDF$' 2-1' 
        match_wins = outputDF$' 2-0' + outputDF$' 2-1'
        match_losses = outputDF$' 1-2' + outputDF$' 0-2'
        game_win_perc = game_wins / (game_losses+game_wins)
        match_win_perc = match_wins / (match_losses+match_wins)
        outputDF = cbind(outputDF, 'Game Wins'=game_wins, 'Game Losses'=game_losses,'Game Win%'=game_win_perc,
                         'Match Wins'=match_wins, 'Match Losses'=match_losses, 'Match Win%'=match_win_perc)
        if(order == T) {
            outputDF = outputDF[order(-outputDF$`Match Win%`*outputDF$`Match Wins`),]
        }
        #ordered by match win% times match wins
        print(outputDF)
}

#list common cards for input dataframe
common_cards = function(inputDF, avg_freq = F) {
    outputDF = as.data.frame(table(inputDF[4]))
    outputDF = outputDF[order(-outputDF[,2]),]
    game_amount = length(inputDF[[4]])/75
    names(outputDF) = c("Card", "Occurrences")
    if(avg_freq){
        outputDF$AvgFreq = outputDF$Occurrences / game_amount
    }
    print(outputDF)
}

#list common cards for winning hero
common_cards_winning_decks = function(inputDF) {
    if(length(inputDF[inputDF$wins==2,1]) == 0) {
        print("No match wins for this input found")
        return(0)
    }
    outputDF = data.frame(table(inputDF[inputDF[2] == 2 ,4]))
    nr_match_wins = length(inputDF[inputDF[2] ==2,1])/75
    nr_games = length(inputDF[ ,1])/75
    match_win_percentage = (length(inputDF[inputDF[2] ==2,1])/75) / (length(inputDF[,1])/75)
    print(paste("Nr of match wins: ", nr_match_wins))
    print(paste("Nr of matches: ", nr_games))
    print(paste("Match win percentage: ", match_win_percentage))
    print("--------------------------")
    print("The following stats consider only cards included in match winning decks")
    outputDF = outputDF[order(-outputDF[2]),]
    outputDF = cbind(outputDF, "Average"=outputDF[2]/nr_match_wins)
    print(outputDF)
}

#create archetypes for decks
create_archetypes = function(inputDF) {
    
    inputDF[,"archetype"] = ""
    outputDF = inputDF
    
    shards_vec = c("Well of Cunning","Well of Retribution","Monsagi Lily Pad","Well of Instinct","Well of Purpose",
                   "Carloth Cobblestone","Well of Conquest","Well of Hatred","Well of Savagery","Well of Innovation",
                   "Well of Ancients","Well of Life","Sepulchra Crypt Dust","Zin'xith Silk","Quash Ridge Rubble",
                   "Scrios Limestone","Howling Plains Bluegrass","Permafrost","Primal Essence","Feralroot Acorn",
                   "Starsphere","Talysen's Memorial","Emperor's Shrine","Kismet's Curio","Shard Prism","Broomball",
                   "Shard of Ancients","Shard of Conquest","Shard of Cunning","Shard of Hatred","Shard of Innovation",
                   "Shard of Instinct","Shard of Life","Shard of Purpose","Shard of Retribution","Shard of Savagery",
                   "Necropolis Coins","Wakuna Coins","Ayotochi Coins","Sapphire Ice","Blood Ice","Scrios Coins",
                   "Diamond Ice","Monsagi Coins","Primal Prism","Ruby Ice","Wild Ice","Blood Shard","Diamond Shard",
                   "Ruby Shard","Sapphire Shard","Wild Shard" )
    
    archetype_list = list()
    archetype_key = list()
    
    for(i in 0:((length(inputDF[[1]])/75)-1)) {
        print(i)
        deck = inputDF[(i*75+1):((i+1)*75),]
        deckDF = data.frame(table(deck[[4]]), stringsAsFactors = F)
        names(deckDF) = c("card", "frequency")
        deckDF$card = as.character(deckDF$card)
        manabase = deckDF[deckDF$card %in% shards_vec,]
        if(is.na(match_every_element(manabase[1],archetype_list))){
            print("NEW")
            archetype_list = c(archetype_list,manabase)
            archetype_key = c(archetype_key, c(i,i))
            outputDF[(i*75+1):((i+1)*75),]$archetype = i
        } else {
            new = T
            for(j in match_every_element(manabase[1], archetype_list)){
                if(sum(archetype_list[j+1] == manabase[2]) == length(archetype_list[[j+1]])) {
                    print("FOUND")
                    outputDF[(i*75+1):((i+1)*75),]$archetype = archetype_key[j]
                    new = F
                }
            }
            if(new){
                print(match_every_element(manabase[1], archetype_list))
                archetype_list = c(archetype_list,manabase)
                archetype_key = c(archetype_key, c(i,i))
                outputDF[(i*75+1):((i+1)*75),]$archetype = i
            }
        }
    }
    outputDF
}


helpz_get_mana_base_list = function(inputDF){
    inputDF[,"archetype"] = ""
    outputDF = inputDF
    
    shards_vec = c("Well of Cunning","Well of Retribution","Monsagi Lily Pad","Well of Instinct","Well of Purpose",
                   "Carloth Cobblestone","Well of Conquest","Well of Hatred","Well of Savagery","Well of Innovation",
                   "Well of Ancients","Well of Life","Sepulchra Crypt Dust","Zin'xith Silk","Quash Ridge Rubble",
                   "Scrios Limestone","Howling Plains Bluegrass","Permafrost","Primal Essence","Feralroot Acorn",
                   "Starsphere","Talysen's Memorial","Emperor's Shrine","Kismet's Curio","Shard Prism","Broomball",
                   "Shard of Ancients","Shard of Conquest","Shard of Cunning","Shard of Hatred","Shard of Innovation",
                   "Shard of Instinct","Shard of Life","Shard of Purpose","Shard of Retribution","Shard of Savagery",
                   "Necropolis Coins","Wakuna Coins","Ayotochi Coins","Sapphire Ice","Blood Ice","Scrios Coins",
                   "Diamond Ice","Monsagi Coins","Primal Prism","Ruby Ice","Wild Ice","Blood Shard","Diamond Shard",
                   "Ruby Shard","Sapphire Shard","Wild Shard" )
    
    archetype_list = list()
    archetype_key = list()
    
    for(i in 0:((length(inputDF[[1]])/75)-1)) {
        print(i)
        deck = inputDF[(i*75+1):((i+1)*75),]
        deckDF = data.frame(table(deck[[4]]), stringsAsFactors = F)
        names(deckDF) = c("card", "frequency")
        deckDF$card = as.character(deckDF$card)
        manabase = deckDF[deckDF$card %in% shards_vec,]
        archetype_list= c(archetype_list, manabase)
    }
    archetype_list
}

#returns vector of all occurrences of input in inputList - NA if not found
match_every_element = function(input, inputList) {
    result = NULL
    if(length(inputList) ==0) {
        return(NA)
    }
    for(i in 1:length(inputList)) {
        if(sum(inputList[[i]] == input) == length(inputList[[i]]) & length(inputList[[i]]) == length(input[[1]]) ){
            result = c(result, i)
        }
    }
    if(length(result)>0){
        return(result)
    } else {
        return(NA)
    }
}
