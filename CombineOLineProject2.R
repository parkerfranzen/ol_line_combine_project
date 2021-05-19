#### Bringing in packages ####
install.packages("rvest")
install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
library(rvest)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

#### Loop for all players ####

#Get years
years <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
           "2011", "2012", "2013", "2014", "2015", "2016", "2017")

#Fill with the following loop. Data frame for all players.
all_players<-data.frame(Player=character(), Position=character(), '40_Yard_Dash'=integer(), Vertical=integer(), Bench_Press=integer(),
                        Broad_Jump=integer(), '3_Cone'=integer(), Shuttle=integer(), Combine_Year=integer())

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
  combine_data <- combine_data %>% dplyr::rename('40_Yard_Dash' = 7, Broad_Jump = 10, '3_Cone' = 11)
  
  #Add Combine year to player
  combine_data$Combine_Year <- years[i]
  
  #Keep Player, Position, 40 Yard Dash, Vertical, Bench Press, Broad Jump, 3 Cone, Shuttle, and Combine Year
  combine_data <- combine_data[, c(1,2,7,8,9,10,11,12,14)]
  
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
  
  #Make all of the drills numeric
  ol_data$`40_Yard_Dash` <- as.numeric(ol_data$`40_Yard_Dash`)
  ol_data$Vertical <- as.numeric(ol_data$Vertical)
  ol_data$Bench <- as.numeric(ol_data$Bench)
  ol_data$Broad_Jump <- as.numeric(ol_data$Broad_Jump)
  ol_data$`3_Cone` <- as.numeric(ol_data$`3_Cone`)
  ol_data$Shuttle <- as.numeric(ol_data$Shuttle)
  
  #Putting all the data into all_players
  all_players <- rbind(all_players, ol_data)
}

#Add id to data frame
all_players <- all_players %>% mutate(id = row_number())

#### Determining good links from bad links ####

#Trimming original link to get to each player's game log.
#Game logs give the years the player played.
#Trim function
trim <- function( x ) {gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)}
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

#### Getting years played ####

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
player_count <- c(624,721)
error_players <- data.frame(Years_Played = years_played, Name = player_names, Count = player_count)

#Adding the 2 error players
seasons <- rbind(seasons, error_players)

#Sorting seasons by player's i
seasons <- seasons[order(seasons[,3]),]

#### Combining the years played into all players ####

#Binding to true_players
true_players <- cbind(true_players, seasons)

#Keeping same columns for both true_players and false_players 
true_players <- true_players[, c(1,2,3,4,5,6,7,8,9,11,15)]
false_players <- false_players[, c(1,2,3,4,5,6,7,8,9,11,14)]

#Binding the the two data frames
all_players <- rbind(true_players, false_players)

#Sorting all_players by player's i
all_players <- all_players[order(all_players[,10]),]

#### Calculating correlation ####

#Remove all missing values by column
d_players <- all_players[!is.na(all_players$`40_Yard_Dash`), ]
v_players <- all_players[!is.na(all_players$Vertical), ]
bp_players <- all_players[!is.na(all_players$Bench), ]
b_players <- all_players[!is.na(all_players$Broad_Jump), ]
c_players <- all_players[!is.na(all_players$`3_Cone`), ]
s_players <- all_players[!is.na(all_players$Shuttle), ]

#Correlation Tests
cor.test(d_players$Years_Played, d_players$`40_Yard_Dash`, method = c("pearson"))
cor.test(v_players$Years_Played, v_players$Vertical, method = c("pearson"))
cor.test(bp_players$Years_Played, bp_players$Bench, method = c("pearson"))
cor.test(b_players$Years_Played, b_players$Broad_Jump, method = c("pearson"))
cor.test(c_players$Years_Played, c_players$`3_Cone`, method = c("pearson"))
cor.test(s_players$Years_Played, s_players$Shuttle, method = c("pearson"))

#### Creating lists of 75th percentile values for all drills ####

#Fill with the following loop
#Creates list of data frames for players by year of NFL Combine they participated in
players_by_year <- vector("list", length(years))

for (i in 1:length(years)){
  
  players_by_year[[i]] <- all_players[all_players$Combine_Year == years[i],]
  
}

#Fill with the following loop
#Creates list by NFL Combine year of the 75th percentile for all drills
dash_75 <- vector("list", length(years))
vertical_75 <- vector("list", length(years))
bench_75 <- vector("list", length(years))
broad_jump_75 <- vector("list", length(years))
cone_75 <- vector("list", length(years))
shuttle_75 <- vector("list", length(years))

#Had to do multiple for loops, a nested for loop was taking too long
for (i in 1:length(years)){
            
    dash_75[[i]] <- quantile(players_by_year[[i]]$`40_Yard_Dash`, probs = c(.25), na.rm = TRUE)
    
}

for (j in 1:length(years)){
  
  vertical_75[[j]] <- quantile(players_by_year[[j]]$Vertical, probs = c(.75), na.rm = TRUE)

}

for (k in 1:length(years)){
  
  bench_75[[k]] <- quantile(players_by_year[[k]]$Bench, probs = c(.75), na.rm = TRUE)
  
}

for (l in 1:length(years)){
  
  broad_jump_75[[l]] <- quantile(players_by_year[[l]]$Broad_Jump, probs = c(.75), na.rm = TRUE)
  
}

for (m in 1:length(years)){
  
  cone_75[[m]] <- quantile(players_by_year[[m]]$`3_Cone`, probs = c(.25), na.rm = TRUE)
  
}

for (n in 1:length(years)){
          
  shuttle_75[[n]] <- quantile(players_by_year[[n]]$Shuttle, probs = c(.25), na.rm = TRUE)
  
}
#### Creating lists of data frames for all players AT OR ABOVE the 75th percentile ####

#Fill with the following loop
#Creates separate lists of data frames for players AT OR ABOVE the 75th percentile
#in all drills by NFL Combine Year
ol_dash <- vector("list", length(years))
ol_vertical <- vector("list", length(years))
ol_bench <- vector("list", length(years))
ol_broad_jump <- vector("list", length(years))
ol_cone <- vector("list", length(years))
ol_shuttle <- vector("list", length(years))

#Had to do multiple for loops, a nested for loop was taking too long
for (i in 1:length(years)){
  
  players_by_year[[i]][is.na(players_by_year[[i]])] <- 0
}

for (j in 1:length(years)){
  
  ol_dash[[j]] <- players_by_year[[j]][players_by_year[[j]]$`40_Yard_Dash` <= dash_75[j],]
  
}

for (k in 1:length(years)){
  
  ol_vertical[[k]] <- players_by_year[[k]][players_by_year[[k]]$Vertical >= vertical_75[k],]
  
}

for (l in 1:length(years)){
  
  ol_bench[[l]] <- players_by_year[[l]][players_by_year[[l]]$Bench >= bench_75[l],]
  
}

for (m in 1:length(years)){
  
  ol_broad_jump[[m]] <- players_by_year[[m]][players_by_year[[m]]$Broad_Jump >= broad_jump_75[m],]
  
}

for (n in 1:length(years)){
  
  ol_cone[[n]] <- players_by_year[[n]][players_by_year[[n]]$`3_Cone` <= cone_75[n],]
  
}

for (o in 1:length(years)){
  
  ol_shuttle[[o]] <- players_by_year[[o]][players_by_year[[o]]$Shuttle <= shuttle_75[o],]
  
}

#### Creating lists of data frames for players BELOW the 75th percentile ####

#Fill with the following loop
#Creates separate lists of data frames for players BELOW the 75th percentile
#in all drills by NFL Combine Year

no_ol_dash <- vector("list", length(years))
no_ol_vertical <- vector("list", length(years))
no_ol_bench <- vector("list", length(years))
no_ol_broad_jump <- vector("list", length(years))
no_ol_cone <- vector("list", length(years))
no_ol_shuttle <- vector("list", length(years))

for (j in 1:length(years)){
  
  no_ol_dash[[j]] <- players_by_year[[j]][players_by_year[[j]]$`40_Yard_Dash` > dash_75[j],]
  
}

for (k in 1:length(years)){
  
  no_ol_vertical[[k]] <- players_by_year[[k]][players_by_year[[k]]$Vertical < vertical_75[k],]
  
}

for (l in 1:length(years)){
  
  no_ol_bench[[l]] <- players_by_year[[l]][players_by_year[[l]]$Bench < bench_75[l],]
  
}

for (m in 1:length(years)){
  
  no_ol_broad_jump[[m]] <- players_by_year[[m]][players_by_year[[m]]$Broad_Jump < broad_jump_75[m],]
  
}

for (n in 1:length(years)){
  
  no_ol_cone[[n]] <- players_by_year[[n]][players_by_year[[n]]$`3_Cone` > cone_75[n],]
  
}

for (o in 1:length(years)){
  
  no_ol_shuttle[[o]] <- players_by_year[[o]][players_by_year[[o]]$Shuttle > shuttle_75[o],]
  
}

#### Data frames for AT OR ABOVE 75th percentile and average years played ####

#Data frame with all 75th percentile 40 yard dash players
dash_all <- data.frame()
for (i in 1:length(years)){
  
  all_dash <- ol_dash[[i]]
  
  dash_all <- rbind(dash_all, all_dash)
}

dash_all <- dash_all[dash_all$`40_Yard_Dash` != 0, ]

#Average length of career
mean_da <- mean(dash_all$Years_Played)

#Data frame with all 75th percentile vertical players
vert_all <- data.frame()
for (i in 1:length(years)){
  
  all_vert <- ol_vertical[[i]]
  
  vert_all <- rbind(vert_all, all_vert)
}

#Average length of career
mean_va <- mean(vert_all$Years_Played)

#Data frame with all 75th percentile bench press players
bench_all <- data.frame()
for (i in 1:length(years)){
  
  all_bench <- ol_bench[[i]]
  
  bench_all <- rbind(bench_all, all_bench)
}

#Average length of career
mean_bpa <- mean(bench_all$Years_Played)

#Data frame with all 75th percentile broad jump players
broad_all <- data.frame()
for (i in 1:length(years)){
  
  all_broad <- ol_broad_jump[[i]]
  
  broad_all <- rbind(broad_all, all_broad)
}

#Average length of career
mean_ba <- mean(broad_all$Years_Played)

#Data frame with all 75th percentile 3 cone players
cone_all <- data.frame()
for (i in 1:length(years)){
  
  all_cone <- ol_cone[[i]]
  
  cone_all <- rbind(cone_all, all_cone)
}

cone_all <- cone_all[cone_all$`3_Cone` != 0, ]

#Average length of career
mean_ca <- mean(cone_all$Years_Played)

#Data frame with all 75th percentile shuttle players
shuttle_all <- data.frame()
for (i in 1:length(years)){
  
  all_shuttle <- ol_shuttle[[i]]
  
  shuttle_all <- rbind(shuttle_all, all_shuttle)
}

shuttle_all <- shuttle_all[shuttle_all$Shuttle != 0, ]

#Average length of career
mean_sa <- mean(shuttle_all$Years_Played)

#### Data frames for BELOW 75th percentile and average years played ####

#Data frame with all players below the 75th percentile in 40 yard dash
no_dash_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_dash <- no_ol_dash[[i]]
  
  no_dash_all <- rbind(no_dash_all, all_no_dash)
}

#Average length of career
no_mean_da <- mean(no_dash_all$Years_Played)

#Data frame with all players below the 75th percentile in vertical
no_vert_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_vert <- no_ol_vertical[[i]]
  
  no_vert_all <- rbind(no_vert_all, all_no_vert)
}

no_vert_all <- no_vert_all[no_vert_all$Vertical != 0, ]

#Average length of career
no_mean_va <- mean(no_vert_all$Years_Played)

#Data frame with all players below the 75th percentile in bench press
no_bench_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_bench <- no_ol_bench[[i]]
  
  no_bench_all <- rbind(no_bench_all, all_no_bench)
}

no_bench_all <- no_bench_all[no_bench_all$Bench != 0, ]

#Average length of career
no_mean_bpa <- mean(no_bench_all$Years_Played)

#Data frame with all players below the 75th percentile in broad jump
no_broad_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_broad <- no_ol_broad_jump[[i]]
  
  no_broad_all <- rbind(no_broad_all, all_no_broad)
}

no_broad_all <- no_broad_all[no_broad_all$Broad_Jump != 0, ]

#Average length of career
no_mean_ba <- mean(no_broad_all$Years_Played)

#Data frame with all players below the 75th percentile in 3 cone
no_cone_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_cone <- no_ol_cone[[i]]
  
  no_cone_all <- rbind(no_cone_all, all_no_cone)
}

#Average length of career
no_mean_ca <- mean(no_cone_all$Years_Played)

#Data frame with all players below the 75th percentile in shuttle
no_shuttle_all <- data.frame()
for (i in 1:length(years)){
  
  all_no_shuttle <- no_ol_shuttle[[i]]
  
  no_shuttle_all <- rbind(no_shuttle_all, all_no_shuttle)
}

#Average length of career
no_mean_sa <- mean(no_shuttle_all$Years_Played)

#Save function
save(shuttle_all, file = "Shuttle_At_Or_Above_75.RData")