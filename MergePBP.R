# get_pbp <- function(gameid){
#   #Grabs the play by play data from the NBA site
#   #URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=0",sep = "")
#   URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=0",sep = "")
#   
#   the.data.file<-fromJSON(URL1)
#   test <-the.data.file$resultSets$rowSet
#   test2 <- test[[1]]
#   test3 <- data.frame(test2)
#   coltest <- the.data.file$resultSets$headers
#   colnames(test3) <- coltest[[1]]
#   return (test3)}


########## players merge
#getwd()
#setwd("C:\Users\Dell\Documents\R-test data") 
players15<-jsonlite::fromJSON("players.json")
#colnames(players15) <- c("firstname", "lastname","player_id_15","team_id")
#players_join <- merge(x = players, y = players15, by = c("firstname","lastname"), all = TRUE)
#length(players_join$player_id[!is.na(players_join$player_id)])


######Clear event
#full_df <- full_df %>% mutate(possID = NA,a1_event = NA,a2_event = NA,a3_event = NA,a4_event = NA,a5_event = NA,h1_event = NA,h2_event = NA,h3_event = NA,h4_event = NA,h5_event = NA)

#########play by play
pbp <- read.csv("0021500506.csv")

pbp <- pbp[-1,]
#colnames(pbp)[2] <- c('event_id')
#Trying to limit the fiels to join to keep the overall size manageable
#pbp <- pbp %>% select (event_id,EVENTMSGTYPE,EVENTMSGACTIONTYPE,SCORE)
#pbp$event_id <- sapply(pbp$event_id, as.numeric)
pbp$game_clock <- as.numeric(lubridate::seconds(lubridate::ms(pbp$PCTIMESTRING)))
pbp <- pbp %>% relocate(game_clock, .after = PCTIMESTRING)
#write.csv(full_df,"./full_df_F1.csv", row.names = FALSE)


#Adding PBP to Spatial-temporal data
for (i in 1:length(unique(full_df$quarter))){
  
  filter_t1 <- full_df %>% filter(full_df$quarter == i) 
  temp_last <- filter(filter_t1,filter_t1$game_clock == min(filter_t1$game_clock))
  if (temp_last$game_clock <0.1){
    #Adding end of Quarter
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==4 & game_clock==0)
    if (length(temp$PLAYER1_ID)!=0){
      if (temp$PLAYER1_ID == unique(full_df$h1_team)){
        temp_last_dis <- temp_last %>% select(h1_dis,h2_dis,h3_dis,h4_dis,h5_dis)
        full_df[[paste0(substr(colnames(temp_last_dis)[apply(temp_last_dis,1,which.min)], 1, 2) ,"_event")]][full_df$time==temp_last$time] <- 15
        #print(i)
        #print("Knicks")
      } else if (temp$PLAYER1_ID == unique(full_df$a1_team)){
        temp_last_dis <- temp_last %>% select(a1_dis,a2_dis,a3_dis,a4_dis,a5_dis)
        full_df[[paste0(substr(colnames(temp_last_dis)[apply(temp_last_dis,1,which.min)], 1, 2) ,"_event")]][full_df$time==temp_last$time] <- 15
        
        #print(i)
        #print("Hawks")
      } else{print("Nobody")}
    } else {cat("No end quarter", i,"\n")}
    
    #Adding Time out
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==9)
    for (j in 1:nrow(temp)){
      #print(j)
      if(temp[j,"PLAYER1_ID"]==0){
        full_df$h2_event[which.min(abs(full_df$game_clock - temp[j,"game_clock"]))] <- 11
      }else if (temp[j,"PLAYER1_ID"] == unique(full_df$h1_team)){
        #full_df$h1_event[full_df$game_clock==temp[j,"game_clock"]] <- 11
        full_df$h1_event[which.min(abs(full_df$game_clock - temp[j,"game_clock"]))] <- 11

      } else if (temp[j,"PLAYER1_ID"] == unique(full_df$a1_team)){
        #full_df$a1_event[full_df$game_clock==temp[1,"game_clock"]] <- 11
        full_df$a1_event[which.min(abs(full_df$game_clock - temp[j,"game_clock"]))] <- 11
      } else{print("NOOOO")}
    }
    
    #Adding Foul
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==6)
    for (j in 1:nrow(temp)){
      #cat("J: ", j,"\n")
      temp_sec <- temp[j,"game_clock"]
      temp_typ <- temp[j,"EVENTMSGACTIONTYPE"]
      temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_player <- temp[j,"PLAYER1_ID"]
      temp_event <- which(filter_t1[temp_order,] ==temp_player)+4
      #cat("temp_event: ", temp_event,"\n")
      temp_dis <- temp_event-1
      
      temp_order <-temp_order +25
      while (filter_t1[temp_order,"game_clock"]-temp_sec<2){
        #cat("distance: ", filter_t1[temp_order,temp_dis],"\n")
        #cat("clock on",filter_t1[temp_order,"game_clock"],"\n")
        if(filter_t1[temp_order,temp_dis]<4 & temp_typ ==2){
          temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
          full_df[[temp_event]][full_df$time ==temp_time] <-8
          temp_order <- 2
          #cat("Foul Player S: ", temp_player, "\n")
        }else if(temp_typ !=2 & (filter_t1[temp_order,"game_clock"]-temp_sec>0.1)){
          temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
          full_df[[temp_event]][full_df$time ==temp_time] <-8
          #cat("Foul Player 8: ", temp_player, "\n")
          temp_order <- 2
          
          if(temp_typ==4){
            full_df[[temp_event]][which(full_df$time ==temp_time)-1] <-7
            #cat("Foul Player 7: ", temp_player, "\n")
          }
          #cat("Other Foul: ", j,"\n")
        } else{}
        temp_order <- temp_order-1
      }
    }
    
    #Adding Turnover
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==5 & EVENTMSGACTIONTYPE !=5)
    for (j in 1:nrow(temp)){
      #cat("J: ", j,"\n")
      temp_sec <- temp[j,"game_clock"]
      temp_order <- which.min(abs(filter_t1$game_clock - temp_sec-0.1))
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_player1 <- temp[j,"PLAYER1_ID"]
      temp_player2 <- temp[j,"PLAYER2_ID"]
      temp_event1 <- which(filter_t1[temp_order,] ==temp_player1)+4
      #cat("temp_event: ", temp_event1,"\n")
      temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
      full_df[[temp_event1]][full_df$time ==temp_time] <-7
      if(temp_player2!=0){
        temp_event2 <- which(filter_t1[temp_order,] ==temp_player2)+4
        full_df[[temp_event2]][full_df$time ==temp_time] <-24
      }
    }
    
    #Adding Rebound
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==4)


    for (j in 1:nrow(temp)){
      temp_sec <- temp[j,"game_clock"]
      temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
      temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_player1 <- temp[j,"PLAYER1_ID"]
      
      if (temp_player1 == unique(full_df$h1_team)){
        #full_df$h1_event[full_df$game_clock==temp[j,"game_clock"]] <- 11
        full_df$h1_event[full_df$time ==temp_time] <- 6
        #cat("Home Team Rebound ","\n")
        
      } else if (temp_player1 == unique(full_df$a1_team)){
        #full_df$a1_event[full_df$game_clock==temp[1,"game_clock"]] <- 11
        full_df$a1_event[full_df$time ==temp_time] <- 6
        #cat("Away Team Rebound ","\n")
      } else{
        temp_reb_type = 6
        temp_event <- which(filter_t1[temp_order,] ==temp_player1)+4
        #cat("temp_event: ", temp_event,"\n")
        temp_dis <- temp_event-1
        if(temp[j,"HOMEDESCRIPTION"]==""){
          temp_desc = temp[j,"VISITORDESCRIPTION"]
          #cat(temp_desc,"\n")
          temp_off = substring(temp_desc, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+5)
          temp_off = as.numeric(sub(" ", "", temp_off))
          temp_def = substring(temp_desc, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+5)
          temp_def = as.numeric(sub(")", "", temp_def))
          if(temp_off+temp_def<=1){
            if(temp_off==1){temp_reb_type=5}
          } else{
            
            temp_total <- filter(pbp, PERIOD < i & EVENTMSGTYPE==4)
            temp_count <-  nrow(temp_total)
            temp_total <- rbind(temp_total,temp)
            temp_j = temp_count+j-1
            while (temp_total[temp_j,"PLAYER1_ID"]!=temp_player1){

              temp_j = temp_j-1
            }
            jtemp_desc = temp_total[temp_j,"VISITORDESCRIPTION"]
            jtemp_off = substring(jtemp_desc, gregexpr(pattern ="Off:",jtemp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",jtemp_desc)[[1]][1]+5)
            jtemp_off = as.numeric(sub(" ", "", jtemp_off))
            jtemp_def = substring(jtemp_desc, gregexpr(pattern ="Def:",jtemp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",jtemp_desc)[[1]][1]+5)
            jtemp_def = as.numeric(sub(")", "", jtemp_def))
            if (jtemp_off!=temp_off) {temp_reb_type=5}
            
          }
          
        } else {
          temp_desc =temp[j,"HOMEDESCRIPTION"]
          #cat(temp_desc,"\n")
          temp_off = substring(temp_desc, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+5)
          temp_off = as.numeric(sub(" ", "", temp_off))
          temp_def = substring(temp_desc, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+5)
          temp_def = as.numeric(sub(")", "", temp_def))
          if(temp_off+temp_def<=1){
            if(temp_off==1){temp_reb_type=5}
          }else{
            temp_total <- filter(pbp, PERIOD < i & EVENTMSGTYPE==4)
            temp_count <-  nrow(temp_total)
            temp_total <- rbind(temp_total,temp)
            temp_j = temp_count+j-1
            while (temp_total[temp_j,"PLAYER1_ID"]!=temp_player1){
              temp_j = temp_j-1
            }
            jtemp_desc = temp_total[temp_j,"HOMEDESCRIPTION"]
            jtemp_off = substring(jtemp_desc, gregexpr(pattern ="Off:",jtemp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",jtemp_desc)[[1]][1]+5)
            jtemp_off = as.numeric(sub(" ", "", jtemp_off))
            jtemp_def = substring(jtemp_desc, gregexpr(pattern ="Def:",jtemp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",jtemp_desc)[[1]][1]+5)
            jtemp_def = as.numeric(sub(")", "", jtemp_def))
            if (jtemp_off!=temp_off) {temp_reb_type=5}
            
          }
        }
        temp_order <-temp_order +45
        while (filter_t1[temp_order,"game_clock"]-temp_sec<2){

          if(!is.na(filter_t1[temp_order,temp_dis]) & filter_t1[temp_order,temp_dis]<2.5 ){
            temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
            full_df[[temp_event]][full_df$time ==temp_time] <-temp_reb_type
            temp_order <- 2
          }
          temp_order <- temp_order-1
        }
      }
    }
    
    #Add free throw
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==3)
    for (j in 1:nrow(temp)){
      temp_sec <- temp[j,"game_clock"]
      #temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
      #temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_set <- filter(filter_t1, filter_t1$game_clock==temp_sec)
      temp_player1 <- temp[j,"PLAYER1_ID"]
      temp_event <- which(temp_set[1,] ==temp_player1)+4
      temp_event1 <- which(temp_set[nrow(temp_set),] ==temp_player1)+4
      #cat("temp_event: ", temp_event,"\n")
      temp_dis <- temp_event-1
      temp_dis1 <- temp_event1-1
      if(temp[j,"EVENTMSGACTIONTYPE"]==11){
        for(k in 1:nrow(temp_set)){
          if(temp_set[k,temp_dis]<3){
            if(temp[j,"SCORE"]!=""){
              full_df[[temp_event]][full_df$time ==temp_set[k,"time"]] <-1
            } else{
              full_df[[temp_event]][full_df$time ==temp_set[k,"time"]] <-2
            }
            break
          }
        }
        
      }else if(temp[j,"EVENTMSGACTIONTYPE"]==12){
        for(k in nrow(temp_set):1){
          if(temp_set[k,temp_dis1]<3){
            if(temp[j,"SCORE"]!=""){
              full_df[[temp_event1]][full_df$time ==temp_set[k,"time"]] <-1
            } else{
              full_df[[temp_event1]][full_df$time ==temp_set[k,"time"]] <-2
            }
            break
          }
        }
      }else{
        for(k in 1:nrow(temp_set)){
          if(temp_set[k,temp_dis]<3){
            if(temp[j,"SCORE"]!=""){
              full_df[[temp_event]][full_df$time ==temp_set[k,"time"]] <-1
            } else{
              full_df[[temp_event]][full_df$time ==temp_set[k,"time"]] <-2
            }
            break
          }
        }
       }
    }
    
    #Add Made Shot
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==1)
    for (j in 1:nrow(temp)){
      temp_sec <- temp[j,"game_clock"]
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
      temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_player1 <- temp[j,"PLAYER1_ID"]
      temp_event <- which(filter_t1[temp_order,] ==temp_player1)+4
      #cat("temp_event: ", temp_event,"\n")
      temp_dis <- temp_event-1
      temp_order <-temp_order +50
      while (filter_t1[temp_order,"game_clock"]-temp_sec<4){
        
        if(!is.na(filter_t1[temp_order,temp_dis]) & filter_t1[temp_order,temp_dis]<1.5 & filter_t1[temp_order,"game_clock"]!=filter_t1[(temp_order-1),"game_clock"]){
          #temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
          temp_time <- filter_t1[temp_order,]$time
          full_df[[temp_event]][full_df$time ==temp_time] <- 3
          temp_order <- 2
        }
        temp_order <- temp_order-1
      }
      
    }
    
    #Add Miss Shot
    temp <- filter(pbp, PERIOD == i & EVENTMSGTYPE==2)
    for (j in 1:nrow(temp)){
      temp_sec <- temp[j,"game_clock"]
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
      temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
      #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
      temp_player1 <- temp[j,"PLAYER1_ID"]
      temp_event <- which(filter_t1[temp_order,] ==temp_player1)+4
      #cat("temp_event: ", temp_event,"\n")
      temp_dis <- temp_event-1
      temp_order <-temp_order +50
      while (filter_t1[temp_order,"game_clock"]-temp_sec<4){
        
        if(!is.na(filter_t1[temp_order,temp_dis]) & filter_t1[temp_order,temp_dis]<1.5 & filter_t1[temp_order,"game_clock"]!=filter_t1[(temp_order-1),"game_clock"]){
          #temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
          temp_time <- filter_t1[temp_order,]$time
          full_df[[temp_event]][full_df$time ==temp_time] <- 4
          temp_order <- 2
        }
        temp_order <- temp_order-1
      }
      
    }
    
    
  }
  else{
    #no quarter data
    cat("No quarter ", i,"\n")
  } 
}








data_merged <- merge(x = full_df, y = pbp, by = "event_id", all.x = TRUE)
data_merged <- data_merged[order(data_merged$quarter, -data_merged$game_clock),]

gameid = "0021500506"