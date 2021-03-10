#Bringing in packages
install.packages("rvest")
install.packages("plyr")
install.packages("dplyr")
library(rvest)
library(plyr)
library(dplyr)

#This is a new comment

#Trim function
trim <- function( x ) {gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)}

#Get years and positions
years <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
           "2011", "2012", "2013", "2014", "2015", "2016", "2017")

#Fill with the following loop. Data frame for all players.
all_players<-data.frame(Player=character(), Position=character(), Vertical=integer(),
                        Broad_Jump=integer(), Combine_Year=integer())

for(i in 1:length(years)){
  
  #NFL Combine website
  combineurl <- paste0("https://www.pro-football-reference.com/draft/",years[i],"-combine.htm")
  
  #Read NFL Combine website
  combinehtml <- read_html(combineurl)
  
  #Combine table from website
  combine_results <- html_nodes(combinehtml,"#combine")
  
  #Create a table
  combine_table <- html_table(combine_results)
  
  #Combine data into a data frame
  combine_data <- data.frame(combine_table[[1]])
  combine_data <- combine_data %>% dplyr::rename(Broad_Jump = 10)
  
  #Add Combine year to player
  combine_data$Combine_Year <- years[i]
  
  #Keep Player, Position, Vertical, Broad Jump, and Combine Year
  combine_data <- combine_data[, c(1,2,8,10,14)]
  
  #Fill with the following loop. Grabbing the links from all players.
  all_links <- data.frame(html=character()) 
  
  for(j in 1:nrow(combine_data)){
    
    get_links <- html_nodes(combinehtml, paste0("tr:nth-child(", j,") a")) %>% html_attr("href") 
    
    link <- data.frame(get_links[1])
    
    #Rename the link
    link <- link %>% dplyr::rename(Player_Link = 1)
    
    #Bind all links
    all_links <- rbind(all_links, link)
  }
  
  #Column bind all NFL Combine participants with their links
  combine_data <- cbind(combine_data, all_links)
  
  #Keep Centers, Offensive Guards, Offensive Tackles, and Offensive Linemen
  ol_data <- combine_data[combine_data$Pos %in% c("C", "OG", "OT", "OL"),] 
  
  #Remove players with no Vertical or Broad Jump
  ol_data <- ol_data[which(ol_data$Vertical != "" | ol_data$Broad_Jump != ""),]
  
  #Make 'Vertical' and 'Broad Jump' numeric
  ol_data$Vertical <- as.numeric(ol_data$Vertical)
  ol_data$Broad_Jump <- as.numeric(ol_data$Broad_Jump)
  
  #Putting all the data into all_players
  all_players <- rbind(all_players, ol_data)
}

#Add id to data frame
all_players <- all_players %>% mutate(id = row_number())

#Trimming original link to get to each player's game log.
#Game logs give the years the player played.
all_players$new_Player_Link <- trim(gsub(".htm", "", all_players$Player_Link))

#List of all players' links to their game log
new_link <- all_players$new_Player_Link

#List of only good links
#Players who attended the combine but don't have a link did not make the NFL
new_link <- new_link[grepl("^/p", new_link)]

#Indicating whether a player has valid link 
all_players$Good_Link <- grepl("^/p", all_players$new_Player_Link)

#Creating a data frame with just players who made the NFL                         
true_players <- all_players[which(all_players$Good_Link == "TRUE"),]
true_players <- true_players %>% mutate(i = row_number())

#Creating a data frame with just players who DID NOT make the NFL
false_players <- all_players[which(all_players$Good_Link == "FALSE"),]

#Setting years played for players who did not make the NFL to 0
false_players["Years_Played"] <- 0

#Empty table for getting years the player played
seasons <- data.frame(Year = integer())

#Game log url for all players
game_log_url <- paste0("https://www.pro-football-reference.com",new_link,"/gamelog/")

#Game log loop for players who made the NFL
for (i in 1:nrow(true_players)){
  tryCatch({
    
    #Just the players names
    player_names <- true_players$Player[[i]]
    
    #Player's count
    player_count <- true_players$i[[i]]
    
    #Read in html for game logs
    game_log_html <- read_html(game_log_url[i])
   
    #Direct to the game log
    log <- html_node(game_log_html,"#stats")
    
    #Some players links don't have a game log...they were in the NFL for 1 year
    if (length(log) == 0){
    years_played <- 1
    } else {
      
    #Game log table
    game_log <- html_table(log, fill=T)
    
    #Rename year column to "Years"
    names(game_log)[2]<-"Years"
    
    #Remove "Year" from data
    game_log <- game_log[which(game_log$Years != "Year"),]
    
    #Remove blanks in the "Years" column from data
    game_log <- game_log[which(game_log$Years != ""),]
    
    #Make "Years" numeric
    game_log$Years<-as.numeric(game_log$Years)
    
    #Keep just Years
    game_log <- game_log[, 2]
    
    #Grab first and last year the player was in the league
    first_year <- head(game_log, n = 1)
    last_year <- tail(game_log, n = 1)
    
    #Find total career length
    years_played <- (last_year + 1) - first_year
    }
    #Bind all years played into one data.frame w/ player name
    seasons <- rbind(seasons, data.frame( Years_Played = years_played, Name = player_names, Count = player_count))
    
  }, error=function(e){cat(conditionMessage(e))})
}

#Creating a data frame with the 2 players with an error.
#Viewing their 'log' I could not find a difference between theirs and a player without an error.
#Their web page did not have a link to their game log unlike every other player.
#However, their game log url did exist.
#I manually added them in since it was only 2 players.
years_played <- c(1, 1)
player_names <- c('Jarvis Harrison', 'Dorian Johnson')
player_count <- c(495,573)
error_players <- data.frame(Years_Played = years_played, Name = player_names, Count = player_count)

#Adding the 2 error players
seasons <- rbind(seasons, error_players)

#Sorting seasons by player's i
seasons <- seasons[order(seasons[,3]),]

#Binding to true_players
true_players <- cbind(true_players, seasons)

#Keeping same columns for both true_players and false_players 
true_players <- true_players[, c(1,2,3,4,5,7,11)]
false_players <- false_players[, c(1,2,3,4,5,7,10)]

#Binding the the two data frames
all_players <- rbind(true_players, false_players)

#Sorting all_players by player's i
all_players <- all_players[order(all_players[,6]),]

#Fill with the following loop
#Creates list of data frames for players by year of NFL Combine they participated in
players_by_year <- vector("list", length(years))

for (i in 1:length(years)){
  
  players_by_year[[i]] <- all_players[all_players$Combine_Year == years[i],]
  
}

#Fill with the following loop
#Creates list by NFL Combine year of the 75th percentile Vertical and Broad Jump values
vertical_75 <- vector("list", length(years))
broad_jump_75 <- vector("list", length(years))

for (i in 1:length(years)){
  for (j in 1:length(years)){

    vertical_75[[i]] <- quantile(players_by_year[[i]]$Vertical, probs = c(.75), na.rm = TRUE)
    broad_jump_75[[j]] <- quantile(players_by_year[[j]]$Broad_Jump, probs = c(.75), na.rm = TRUE)

  }
}

#Fill with the following loop
#Creates separate lists of data frames for players AT OR ABOVE the 75th percentile
#in Vertical, Broad Jump, and both by NFL Combine Year 
ol_vertical <- vector("list", length(years))
ol_broad_jump <- vector("list", length(years))
ol_vert_broad <- vector("list", length(years))

for (i in 1:length(years)){
  for (j in 1:length(years)){
    for (k in 1:length(years)){
      for (l in 1:length(years)){
    
    #Changing missing values to zero
    players_by_year[[i]][is.na(players_by_year[[i]])] <- 0
    ol_vertical[[j]] <- players_by_year[[j]][players_by_year[[j]]$Vertical >= vertical_75[j],]
    ol_broad_jump[[k]] <- players_by_year[[k]][players_by_year[[k]]$Broad_Jump >= broad_jump_75[k],]
    ol_vert_broad[[l]] <- players_by_year[[l]][players_by_year[[l]]$Vertical >= vertical_75[l] & players_by_year[[l]]$Broad_Jump >= broad_jump_75[l],]
    
      }
    }
  }
}

#Fill with the following loop
#Creates separate lists of data frames for players BELOW the 75th percentile
#in Vertical, Broad Jump, and both by NFL Combine Year 
no_ol_vertical <- vector("list", length(years))
no_ol_broad_jump <- vector("list", length(years))
no_ol_vert_broad <- vector("list", length(years))

for (i in 1:length(years)){
  for (j in 1:length(years)){
    for (k in 1:length(years)){
      for (l in 1:length(years)){
        
        #Changing missing values to zero
        players_by_year[[i]][is.na(players_by_year[[i]])] <- 0
        no_ol_vertical[[j]] <- players_by_year[[j]][players_by_year[[j]]$Vertical < vertical_75[j],]
        no_ol_broad_jump[[k]] <- players_by_year[[k]][players_by_year[[k]]$Broad_Jump < broad_jump_75[k],]
        no_ol_vert_broad[[l]] <- players_by_year[[l]][players_by_year[[l]]$Vertical < vertical_75[l] & players_by_year[[l]]$Broad_Jump < broad_jump_75[l],]
        
      }
    }
  }
}

#Data frame with all 75th percentile vertical players
vert_all <- data.frame()
for (i in 1:length(years)){
  
  all_vert <- ol_vertical[[i]]
  
  vert_all <- rbind(vert_all, all_vert)
}

#Average length of career
mean_va <- mean(vert_all$Years_Played)

#Data frame with all 75th percentile broad jump players
broad_all <- data.frame()
for (i in 1:length(years)){
  
  all_broad <- ol_broad_jump[[i]]
  
  broad_all <- rbind(broad_all, all_broad)
}

#Average length of career
mean_ba <- mean(broad_all$Years_Played)

#Data frame with all 75th percentile vertical and broad jump players
vert_broad_all <- data.frame()
for (i in 1:length(years)){
  
  all_vert_broad <- ol_vert_broad[[i]]
  
  vert_broad_all <- rbind(vert_broad_all, all_vert_broad)
}

#Average length of career
mean_vba <- mean(vert_broad_all$Years_Played)

#Data frame with all players below the 75th percentile in vertical
no_vert_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_vert <- no_ol_vertical[[i]]
  
  no_vert_all <- rbind(no_vert_all, all_no_vert)
}

#Average length of career
mean_no_va <- mean(no_vert_all$Years_Played)

#Data frame with all players below the 75th percentile in broad jump
no_broad_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_broad <- no_ol_broad_jump[[i]]
  
  no_broad_all <- rbind(no_broad_all, all_no_broad)
}

#Average length of career
mean_no_ba <- mean(no_broad_all$Years_Played)

#Data frame with all players below the 75th percentile in both vertical and broad jump
no_vert_broad_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_vert_broad <- no_ol_vert_broad[[i]]
  
  no_vert_broad_all <- rbind(no_vert_broad_all, all_no_vert_broad)
}

#Average length of career
mean_no_vba <- mean(no_vert_broad_all$Years_Played)
  
  
  
  
  