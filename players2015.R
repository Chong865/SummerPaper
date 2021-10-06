
#rm(list = ls())
code.dir <- "./code"
data.dir <- "./data"
#players15<-jsonlite::fromJSON("players.json")
#colnames(players15) <- c("firstname", "lastname","player_id_15","team_id")
#players_join <- merge(x = players, y = players15, by = c("firstname","lastname"), all = TRUE)
#length(players_join$player_id[!is.na(players_join$player_id)])
players <- read.csv(sprintf("%s/players2015.csv", data.dir))
#write.csv(players_join,"./players_join.csv", row.names = FALSE)