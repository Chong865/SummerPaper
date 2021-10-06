library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)
library(data.table)
#rm(list = ls())
library(doParallel)
registerDoParallel(16)
#raw_game  <- fromJSON(file=sprintf("%s/0021500492.json", data.dir))
start_time <- Sys.time()

raw_game<-jsonlite::fromJSON("0021500506.json")
game_id <- raw_game$gameid
cat("...... restructuring game info.\n")
var1 <- NULL
var2 <- NULL
var3 <- NULL
var4 <- NULL
var5 <- NULL
var6 <- NULL
var7 <- NULL
for (i in 1:length(raw_game$events$moments)) {
  var1 <- append(var1, sapply(raw_game$events$moments[[i]], "[[", 1))
  var2 <- append(var2, sapply(raw_game$events$moments[[i]], "[[", 2))
  var3 <- append(var3, sapply(raw_game$events$moments[[i]], "[[", 3))
  var4 <- append(var4, sapply(raw_game$events$moments[[i]], "[[", 4))
  var5 <- append(var5, sapply(raw_game$events$moments[[i]], "[[", 5))
  var6 <- append(var6, sapply(raw_game$events$moments[[i]], "[", 6))
  var7 <- append(var7, rep(raw_game$events$eventId[i], length(raw_game$events$moments[[i]])))
}
var4 <- lapply(var4, function(x) {ifelse(is.null(x), NA, x)})
var5 <- lapply(var5, function(x) {ifelse(is.null(x), NA, x)})
var1 <- unlist(var1)
var2 <- unlist(var2)
var3 <- unlist(var3)
var4 <- unlist(var4)
var5 <- unlist(var5)

df <- data.frame(var1,var2,var3,var4,var5,var7,
                 stringsAsFactors = FALSE)

cat("...... restructuring player info.\n")
format_player_info <- function(player_moment_matrix, home_id, away_id) {
  b_sel <- which(player_moment_matrix[,1] == -1)
  h_sel <- which(player_moment_matrix[,1] == home_id)
  a_sel <- which(player_moment_matrix[,1] == away_id)
  b_out <- player_moment_matrix[b_sel,]
  h_out <- c(t(player_moment_matrix[h_sel,]))
  a_out <- c(t(player_moment_matrix[a_sel,]))
  if (length(b_out) < 5)
    b_out <- c(b_out, rep(NA, 5 - length(b_out)))
  if (length(h_out) < 25)
    h_out <- c(h_out, rep(NA, 25 - length(h_out)))
  if (length(a_out) < 25)
    a_out <- c(a_out, rep(NA, 25 - length(a_out)))
  out <- c(b_out, a_out, h_out)
  return(out)
}

home_id <- raw_game$events$home$teamid[1]
away_id <- raw_game$events$visitor$teamid[1]

players_df <- lapply(var6, format_player_info, home_id = home_id, away_id = away_id)
players_df <- do.call("rbind", players_df)

cat("...... combining game/player info.\n")

ranges <- matrix(NA, nrow = 12, ncol = 2)
ranges[1,] <- range(players_df[,1], na.rm = T) == c(-1,-1)
ranges[2,] <- range(players_df[,2], na.rm = T) == c(-1,-1)
for (i in 1:10)
  ranges[i+2,] <- range(players_df[,(10 + (i - 1) * 5)], na.rm = T) == c(0,0)

if (all(ranges)) {
  col_nums <- c(1,2,seq(10,55,by=5))
  players_df <- players_df[,-col_nums]
} else {
  stop("Problem when dropping columns out of 'players_df' matrix.")
}
header <- c("quarter", "unknown1", "game_clock", "shot_clock", "unknown2", "event_id",
            "x", "y", "z")

for (i in 1:5)
  header <- append(header, c(paste0("a", i, "_team"), paste0("a", i, "_ent"), paste0("a", i, "_x"), paste0("a", i, "_y")))
for (i in 1:5)
  header <- append(header, c(paste0("h", i, "_team"), paste0("h", i, "_ent"), paste0("h", i, "_x"), paste0("h", i, "_y")))
full_df <- data.frame(df, players_df, stringsAsFactors = FALSE)
colnames(full_df) <- header

# restructuring data set to match the paper
full_df$event_id <- as.numeric(full_df$event_id)
full_df <- distinct(full_df, game_clock,x,y,z,.keep_all = TRUE)
full_df <- subset(full_df, select=-c(unknown1,unknown2))
full_df <- subset(full_df, select=-c(a2_team,a3_team,a4_team,a5_team,h2_team,h3_team,h4_team,h5_team))
full_df <- full_df %>% mutate(possID = NA,a1_event = NA,a2_event = NA,a3_event = NA,a4_event = NA,a5_event = NA,h1_event = NA,h2_event = NA,h3_event = NA,h4_event = NA,h5_event = NA)
for (i in 1:5) full_df <- full_df %>% relocate(paste0("a", i, "_event"), .after = paste0("a", i, "_y"))
for (i in 1:5) full_df <- full_df %>% relocate(paste0("h", i, "_event"), .after = paste0("h", i, "_y"))

#Adding player distance from the ball
full_df <- full_df %>% mutate(a1_dis = NA,a2_dis = NA,a3_dis = NA,a4_dis = NA,a5_dis = NA,h1_dis = NA,h2_dis = NA,h3_dis = NA,h4_dis = NA,h5_dis = NA)
for (i in 1:5) full_df <- full_df %>% relocate(paste0("a", i, "_dis"), .after = paste0("a", i, "_y"))
for (i in 1:5) full_df <- full_df %>% relocate(paste0("h", i, "_dis"), .after = paste0("h", i, "_y"))

for (i in 1:5) full_df[[paste0("h", i, "_dis")]]<- sqrt((full_df$x-full_df[[paste0("h", i, "_x")]])^2+(full_df$y-full_df[[paste0("h", i, "_y")]])^2)
for (i in 1:5) full_df[[paste0("a", i, "_dis")]]<- sqrt((full_df$x-full_df[[paste0("a", i, "_x")]])^2+(full_df$y-full_df[[paste0("a", i, "_y")]])^2)


#add an identifying number

full_df$time <- 1:nrow(full_df)
full_df <- full_df %>% relocate(time)

full_df$time1 <- (1:nrow(full_df)-1)*40
full_df <- full_df %>% relocate(time1)

#removing event_id
full_df <- subset(full_df, select=-c(event_id))

#add an possession home/visitor
full_df$pos <- NA
full_df <- full_df %>% relocate(pos)

#add home/visitor
full_df$tea <- NA
full_df <- full_df %>% relocate(tea)

#add home/visitor
full_df$score <- NA
full_df <- full_df %>% relocate(score)


end_time <- Sys.time()
print(end_time - start_time)
