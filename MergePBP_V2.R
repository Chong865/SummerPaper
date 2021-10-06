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
#full_df <- full_df %>% mutate(pos = NA,pos = NA, possID = NA,a1_event = NA,a2_event = NA,a3_event = NA,a4_event = NA,a5_event = NA,h1_event = NA,h2_event = NA,h3_event = NA,h4_event = NA,h5_event = NA)

#########play by play
pbp <- read.csv("0021500506.csv")

pbp <- pbp[-1,]
#colnames(pbp)[2] <- c('event_id')
#Trying to limit the fiels to join to keep the overall size manageable
#pbp <- pbp %>% select (event_id,EVENTMSGTYPE,EVENTMSGACTIONTYPE,SCORE)
#pbp$event_id <- sapply(pbp$event_id, as.numeric)
pbp$game_clock <- as.numeric(lubridate::seconds(lubridate::ms(pbp$PCTIMESTRING)))
pbp <- pbp %>% relocate(game_clock, .after = PCTIMESTRING)
#write.csv(tdat1,"./tdat_V1.csv", row.names = FALSE)


start_time <- Sys.time()
#poss of shot and turnover
ii <- 1

#Adding PBP to Spatial-temporal data
for (i in 1:length(unique(full_df$quarter))){
  cat("Period: ", i,"\n")
  filter_t1 <- full_df %>% filter(full_df$quarter == i) 
  temp_last <- filter(filter_t1,filter_t1$game_clock == min(filter_t1$game_clock))
  temp_bound <- 1
  
  if (temp_last$game_clock <0.1){
    temp <- filter(pbp, PERIOD == i)
    for(k in 1: nrow(temp)){
      curr_time <- temp[k,"game_clock"]
      #cat(curr_time,": ", abs(filter_t1$game_clock[which.min(abs(filter_t1$game_clock - temp[k,"game_clock"]))]-temp[k,"game_clock"]),"\n")
      #Adding end of quarter
      if(temp[k,"EVENTMSGTYPE"]==4 & curr_time==0){
        if (length(temp[k,"PLAYER1_ID"])!=0){
          if (temp[k,"PLAYER1_ID"] == unique(full_df$h1_team)){
            temp_last_dis <- temp_last %>% select(h1_dis,h2_dis,h3_dis,h4_dis,h5_dis)
            full_df[[paste0(substr(colnames(temp_last_dis)[apply(temp_last_dis,1,which.min)], 1, 2) ,"_event")]][full_df$time==temp_last$time] <- 15
            #print(i)
            #print("Knicks")
          } else if (temp[k,"PLAYER1_ID"] == unique(full_df$a1_team)){
            temp_last_dis <- temp_last %>% select(a1_dis,a2_dis,a3_dis,a4_dis,a5_dis)
            full_df[[paste0(substr(colnames(temp_last_dis)[apply(temp_last_dis,1,which.min)], 1, 2) ,"_event")]][full_df$time==temp_last$time] <- 15
            
            #print(i)
            #print("Hawks")
          } else{print("Nobody")}
        } else {cat("No end quarter", i,"\n")}
      }
      
      #if data is missing
      else if(abs(filter_t1$game_clock[which.min(abs(filter_t1$game_clock - temp[k,"game_clock"]))]-temp[k,"game_clock"])>1.8){
        cat("No data for time clock for: ", temp[k,"game_clock"],"\n")
      }
      
      
      #Adding time out
      else if(temp[k,"EVENTMSGTYPE"]==9){
        temp_bound <-filter_t1$time[which.min(abs(filter_t1$game_clock - temp[k,"game_clock"]))]
        #print(temp_bound)
        if(temp[k,"PLAYER1_ID"]==0){
          #full_df$h2_event[which.min(abs(full_df$time - temp_bound))] <- 11
          full_df$h2_event[full_df$time== temp_bound] <- 11
        }else if (temp[k,"PLAYER1_ID"] == unique(full_df$h1_team)){
          #full_df$h1_event[full_df$game_clock==temp[j,"game_clock"]] <- 11
          #full_df$h1_event[which.min(abs(full_df$time - temp_bound))]<- 11
          full_df$h1_event[full_df$time == temp_bound]<- 11
          
          #full_df$pos[full_df$time<temp_bound & is.na(full_df$pos) & full_df$quarter==i] <-k
          
        } else if (temp[k,"PLAYER1_ID"] == unique(full_df$a1_team)){
          #full_df$a1_event[full_df$game_clock==temp[1,"game_clock"]] <- 11
          full_df$a1_event[full_df$time == temp_bound] <- 11
          #full_df$pos[full_df$time <= temp_bound & is.na(full_df$pos) & full_df$quarter==i] <-"A"
          
        } else{print("Time out NOOOO")}
      }
      
      #Adding foul
      else if(temp[k,"EVENTMSGTYPE"]==6){
        #cat(curr_time, ": ", temp[k,"HOMEDESCRIPTION"],temp[k,"VISITORDESCRIPTION"],"\n")
        #cat("J: ", j,"\n")
        temp_sec <- curr_time
        temp_typ <- temp[k,"EVENTMSGACTIONTYPE"]
        temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_player <- temp[k,"PLAYER1_ID"]
        temp_event <- which(filter_t1[temp_order,] ==temp_player)+4
        #cat("temp_event: ", temp_event,"\n")
        temp_dis <- temp_event-1
        
        temp_order <-temp_order +25
        while (filter_t1[temp_order,"time"]> temp_bound){
          #cat("distance: ", filter_t1[temp_order,temp_dis],"\n")
          #cat("clock on",filter_t1[temp_order,"game_clock"],"\n")
          if(filter_t1[temp_order,temp_dis]<4 & temp_typ ==2){
            temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
            full_df[[temp_event]][full_df$time ==temp_time] <-8
            #if (grepl("h",colnames(full_df)[temp_event])){
              #full_df$pos[full_df$time <= temp_time& is.na(full_df$pos) & full_df$quarter==i]<-"A"
            #}
            colnames(full_df)[temp_event]
            temp_order <- 2
            temp_bound <- temp_time
            #cat("Foul Player S: ", temp_player, "\n")
          }else if(temp_typ !=2 & (filter_t1[temp_order,"game_clock"]-temp_sec>0.1)){
            temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
            full_df[[temp_event]][full_df$time ==temp_time] <-8
            #cat("Foul Player 8: ", temp_player, "\n")
            temp_order <- 2
            temp_bound <- temp_time
            
            if(temp_typ==4){
              full_df[[temp_event]][which(full_df$time ==temp_time)-1] <-7
              #cat("Foul Player 7: ", temp_player, "\n")
            }
            #cat("Other Foul: ", j,"\n")
          } else{

          }
          temp_order <- temp_order-1
          if(temp_order==0){break}
        }
      }
      
      
      #Adding turnover
      else if(temp[k,"EVENTMSGTYPE"]==5 & temp[k,"EVENTMSGACTIONTYPE"]!=5){
        #cat(curr_time, ": ", temp[k,"HOMEDESCRIPTION"],temp[k,"VISITORDESCRIPTION"],"\n")
        temp_sec <- temp[k,"game_clock"]
        temp_order <- which.min(abs(filter_t1$game_clock - temp_sec-0.5))
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_player1 <- temp[k,"PLAYER1_ID"]
        temp_player2 <- temp[k,"PLAYER2_ID"]
        temp_event1 <- which(filter_t1[temp_order,] ==temp_player1)+4
        #cat("temp_event: ", temp_event1,"\n")
        temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
        while(temp_time<=temp_bound){
          temp_time <- temp_time+1
        }
        full_df[[temp_event1]][full_df$time ==temp_time] <-7
        temp_scor <- k
        while(temp[temp_scor,"SCORE"]==""){
          temp_scor <- temp_scor-1
          
        }
        full_df$score[full_df$time ==temp_time] <- temp[temp_scor,"SCORE"]
        temp_tea <- "h"
        if (grepl("a",colnames(full_df)[temp_event1])){
          temp_tea <-"a"
        }
        
        #Add possession, dribble and pass
        poss_time <- temp_time
        while(poss_time>temp_bound){
          if(full_df$game_clock[full_df$time==poss_time]!=full_df$game_clock[full_df$time==(poss_time-1)]){
            if(full_df$game_clock[full_df$time==poss_time]-full_df$game_clock[full_df$time==(poss_time-1)]>-1.5 & full_df$game_clock[full_df$time==poss_time]-full_df$game_clock[full_df$time==(poss_time-1)]<0){    
              full_df$pos[full_df$time==poss_time] <- ii
              full_df$tea[full_df$time==poss_time] <- temp_tea
            }else{poss_time <- temp_bound}
          }else{poss_time <- temp_bound}
          poss_time <- poss_time-1
        }
        ii <- ii+1
        
        if(temp_player2!=0){
          temp_event2 <- which(filter_t1[temp_order,] ==temp_player2)+4
          full_df[[temp_event2]][full_df$time ==temp_time] <-24

        }else{
        }
        temp_bound <- temp_time
      }
      
      #Adding Rebound
      else if(temp[k,"EVENTMSGTYPE"]==4){
        #cat(curr_time, ": ", temp[k,"HOMEDESCRIPTION"],temp[k,"VISITORDESCRIPTION"],"\n")
        temp_sec <- temp[k,"game_clock"]
        temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
        temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_player1 <- temp[k,"PLAYER1_ID"]
        
        if (temp_player1 == unique(full_df$h1_team)){
          #full_df$h1_event[full_df$game_clock==temp[j,"game_clock"]] <- 11
          while(temp_time<=temp_bound){
            temp_time <- temp_time+1
          }
          full_df$h1_event[full_df$time ==temp_time] <- 6
          temp_bound <- temp_time
   
          #cat("Home Team Rebound ","\n")
          
        } else if (temp_player1 == unique(full_df$a1_team)){
          #full_df$a1_event[full_df$game_clock==temp[1,"game_clock"]] <- 11
          while(temp_time<=temp_bound){
            temp_time <- temp_time+1
          }
          full_df$a1_event[full_df$time ==temp_time] <- 6
          temp_bound <- temp_time
          #cat("Away Team Rebound ","\n")
        } else{
          temp_reb_type = 6
          temp_event <- which(filter_t1[temp_order,] ==temp_player1)+4
          #cat("temp_event: ", temp_event,"\n")
          temp_dis <- temp_event-1
          if(temp[k,"HOMEDESCRIPTION"]==""){
            temp_desc = temp[k,"VISITORDESCRIPTION"]
            #cat(temp_desc,"\n")
            temp_off = substring(temp_desc, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+5)
            temp_off = as.numeric(sub(" ", "", temp_off))
            temp_def = substring(temp_desc, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+5)
            temp_def = as.numeric(sub(")", "", temp_def))
            if(temp_off+temp_def<=1){
              if(temp_off==1){temp_reb_type=5}
            } else{
              temp_c <- filter(temp, game_clock >= temp[k,"game_clock"] & EVENTMSGTYPE==4)
              temp_c_count <- nrow(temp_c)
              temp_total <- filter(pbp, PERIOD < i & EVENTMSGTYPE==4)
              
              temp_count <-  nrow(temp_total)
              temp_total <- rbind(temp_total,temp_c)
              temp_k <- temp_count+temp_c_count-1
              while (temp_total[temp_k,"PLAYER1_ID"]!=temp_player1){
                
                temp_k = temp_k-1
              }
              ktemp_desc = temp_total[temp_k,"VISITORDESCRIPTION"]
              ktemp_off = substring(ktemp_desc, gregexpr(pattern ="Off:",ktemp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",ktemp_desc)[[1]][1]+5)
              ktemp_off = as.numeric(sub(" ", "", ktemp_off))
              ktemp_def = substring(ktemp_desc, gregexpr(pattern ="Def:",ktemp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",ktemp_desc)[[1]][1]+5)
              ktemp_def = as.numeric(sub(")", "", ktemp_def))
              if (ktemp_off!=temp_off) {temp_reb_type=5}
              
            }
            
          } else {
            temp_desc =temp[k,"HOMEDESCRIPTION"]
            #cat(temp_desc,"\n")
            temp_off = substring(temp_desc, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",temp_desc)[[1]][1]+5)
            temp_off = as.numeric(sub(" ", "", temp_off))
            temp_def = substring(temp_desc, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",temp_desc)[[1]][1]+5)
            temp_def = as.numeric(sub(")", "", temp_def))
            if(temp_off+temp_def<=1){
              if(temp_off==1){temp_reb_type=5}
            }else{
              temp_c <- filter(temp, game_clock >= temp[k,"game_clock"] & EVENTMSGTYPE==4)
              temp_c_count <- nrow(temp_c)
              temp_total <- filter(pbp, PERIOD < i & EVENTMSGTYPE==4)
              temp_count <-  nrow(temp_total)
              temp_total <- rbind(temp_total,temp_c)
              temp_k = temp_count+temp_c_count-1
              while (temp_total[temp_k,"PLAYER1_ID"]!=temp_player1){
                temp_k = temp_k-1
              }
              ktemp_desc = temp_total[temp_k,"HOMEDESCRIPTION"]
              ktemp_off = substring(ktemp_desc, gregexpr(pattern ="Off:",ktemp_desc)[[1]][1]+4, gregexpr(pattern ="Off:",ktemp_desc)[[1]][1]+5)
              ktemp_off = as.numeric(sub(" ", "", ktemp_off))
              ktemp_def = substring(ktemp_desc, gregexpr(pattern ="Def:",ktemp_desc)[[1]][1]+4, gregexpr(pattern ="Def:",ktemp_desc)[[1]][1]+5)
              ktemp_def = as.numeric(sub(")", "", ktemp_def))
              if (ktemp_off!=temp_off) {temp_reb_type=5}
              
            }
          }
          temp_order <-temp_order +20
          while (filter_t1[temp_order,"time"]>temp_bound){
            
          
            if(!is.na(filter_t1[temp_order,temp_dis]) & filter_t1[temp_order,temp_dis]<3 & filter_t1[temp_order,"game_clock"]!=filter_t1[(temp_order+1),"game_clock"]){
       
              temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
              full_df[[temp_event]][full_df$time ==temp_time] <-temp_reb_type
              temp_bound <- temp_time
              temp_order <- 2
            }
            temp_order <- temp_order-1
            if(temp_order==0){break}
          }
        }
      }
      
      
      #Adding free throw
      else if(temp[k,"EVENTMSGTYPE"]==3){
        #cat(curr_time, ": ", temp[k,"HOMEDESCRIPTION"],temp[k,"VISITORDESCRIPTION"],"\n")
        temp_sec <- temp[k,"game_clock"]
        #temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
        #temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_set <- filter(filter_t1, filter_t1$game_clock==temp_sec)
        temp_player1 <- temp[k,"PLAYER1_ID"]
        temp_event <- which(temp_set[1,] ==temp_player1)+4
        temp_event1 <- which(temp_set[nrow(temp_set),] ==temp_player1)+4
        #cat("temp_event: ", temp_event,"\n")
        temp_dis <- temp_event-1
        temp_dis1 <- temp_event1-1
        if(temp[k,"EVENTMSGACTIONTYPE"]==11){
          for(n in 1:nrow(temp_set)){
            if(temp_set[n,temp_dis]<3){
              if(temp[k,"SCORE"]!=""){
                full_df[[temp_event]][full_df$time ==temp_set[n,"time"]] <-1
                temp_bound <- temp_set[n,"time"]
              } else{
                full_df[[temp_event]][full_df$time ==temp_set[n,"time"]] <-2
                temp_bound <- temp_set[n,"time"]
              }
              break
            }
          }
          
        }else if(temp[k,"EVENTMSGACTIONTYPE"]==12){
          for(n in nrow(temp_set):1){
            if(temp_set[n,temp_dis1]<3){
              if(temp[k,"SCORE"]!=""){
                full_df[[temp_event1]][full_df$time ==temp_set[n,"time"]] <-1
                temp_bound <- temp_set[n,"time"]
              } else{
                full_df[[temp_event1]][full_df$time ==temp_set[n,"time"]] <-2
                temp_bound <- temp_set[n,"time"]
              }
              break
            }
          }
        }else{
          for(n in 1:nrow(temp_set)){
            if(temp_set[n,temp_dis]<3){
              if(temp[k,"SCORE"]!=""){
                full_df[[temp_event]][full_df$time ==temp_set[n,"time"]] <-1
                temp_bound <- temp_set[n,"time"]
              } else{
                full_df[[temp_event]][full_df$time ==temp_set[n,"time"]] <-2
                temp_bound <- temp_set[n,"time"]
              }
              break
            }
          }
        }
      }
      
      
      #Adding made shot
      else if(temp[k,"EVENTMSGTYPE"]==1){
        #cat(curr_time, ": ", temp[k,"HOMEDESCRIPTION"],temp[k,"VISITORDESCRIPTION"],"\n")
        temp_sec <- temp[k,"game_clock"]
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
        temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_player1 <- temp[k,"PLAYER1_ID"]
        temp_event <- which(filter_t1[temp_order,] ==temp_player1)+4
        #cat("temp_event: ", temp_event,"\n")
        temp_dis <- temp_event-1
        temp_order <-temp_order +20
        while (filter_t1[temp_order,"time"]>temp_bound){
          
          if(!is.na(filter_t1[temp_order,temp_dis]) & filter_t1[temp_order,temp_dis]<4 & filter_t1[temp_order,"game_clock"]!=filter_t1[(temp_order+1),"game_clock"]){
            #temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
          
            temp_time <- filter_t1[temp_order,]$time
            full_df[[temp_event]][full_df$time ==temp_time] <- 3
            full_df$score[full_df$time ==temp_time] <- temp[k,"SCORE"]
            
            temp_tea <- "h"
            if (grepl("a",colnames(full_df)[temp_event])){
              temp_tea <-"a"
            }
            #Add possession, dribble and pass
            poss_time <- temp_time
            while(poss_time>temp_bound){
              if(full_df$game_clock[full_df$time==poss_time]!=full_df$game_clock[full_df$time==(poss_time-1)]){
                if(full_df$game_clock[full_df$time==poss_time]-full_df$game_clock[full_df$time==(poss_time-1)]>-1.5 & full_df$game_clock[full_df$time==poss_time]-full_df$game_clock[full_df$time==(poss_time-1)]<0){    
                  full_df$pos[full_df$time==poss_time] <- ii
                  full_df$tea[full_df$time==poss_time] <- temp_tea
                }else{poss_time <- temp_bound}
              }else{poss_time <- temp_bound}
              poss_time <- poss_time-1
            }
            ii <- ii+1
            
            temp_bound <- temp_time
            temp_order <- 2
          }
          temp_order <- temp_order-1
          if(temp_order==0){break}
        }
        
      }
      
      #Adding miss shot
      else if(temp[k,"EVENTMSGTYPE"]==2){
        #cat(curr_time, ": ", temp[k,"HOMEDESCRIPTION"],temp[k,"VISITORDESCRIPTION"],"\n")
        temp_sec <- temp[k,"game_clock"]
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_order <- which.min(abs(filter_t1$game_clock - temp_sec))
        temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
        #cat("temp_order: ", temp_order, " temp_sec: ", temp_sec,"\n")
        temp_player1 <- temp[k,"PLAYER1_ID"]
        temp_event <- which(filter_t1[temp_order,] ==temp_player1)+4
        #cat("temp_event: ", temp_event,"\n")
        temp_dis <- temp_event-1
        temp_order <-temp_order+20
        while (filter_t1[temp_order,"time"]>temp_bound){
       
          if(!is.na(filter_t1[temp_order,temp_dis]) & filter_t1[temp_order,temp_dis]<4 & filter_t1[temp_order,"game_clock"]!=filter_t1[(temp_order+1),"game_clock"]){
            #temp_time <-full_df$time[full_df$time == filter_t1[temp_order,]$time]
            
            temp_time <- filter_t1[temp_order,]$time
            full_df[[temp_event]][full_df$time ==temp_time] <- 4
            temp_scor <- k
            while(temp[temp_scor,"SCORE"]==""){
              temp_scor <- temp_scor-1
              
            }
            full_df$score[full_df$time ==temp_time] <- temp[temp_scor,"SCORE"]
            
            temp_tea <- "h"
            if (grepl("a",colnames(full_df)[temp_event])){
              temp_tea <-"a"
            }
            #Add possession, dribble and pass
            poss_time <- temp_time
            while(poss_time>temp_bound){
              if(full_df$game_clock[full_df$time==poss_time]!=full_df$game_clock[full_df$time==(poss_time-1)]){
                if(full_df$game_clock[full_df$time==poss_time]-full_df$game_clock[full_df$time==(poss_time-1)]>-1.5 & full_df$game_clock[full_df$time==poss_time]-full_df$game_clock[full_df$time==(poss_time-1)]<0){    
                  full_df$pos[full_df$time==poss_time] <- ii
                  full_df$tea[full_df$time==poss_time] <- temp_tea
                }else{poss_time <- temp_bound}
              }else{poss_time <- temp_bound}
              poss_time <- poss_time-1
            }
            ii <- ii+1
            
            temp_bound <- temp_time
            temp_order <- 2
          }
          temp_order <- temp_order-1
          if(temp_order==0){break}
        }
      }
      else{
        print("hah")
      }
      
    }

    
    
  }else{
    #no quarter data
    cat("No quarter ", i,"\n")
  } 
}

#Add pocession, passing and dribbling

for (i in unique(full_df$pos)){
  if(!is.na(i)){
    temp_pos <- subset(full_df, pos == i)

    if(nrow(temp_pos)>600){
      temp_pos <- tail(temp_pos,n=650)
    }
    
    if(temp_pos[1,"tea"]=="a"){
      temp_side <- which(colnames(temp_pos) =="a1_dis")
      dis_rank <- apply(temp_pos[,c(temp_side,temp_side+5,temp_side+10,temp_side+15,temp_side+20)], 1, which.min)
      dis_dis <- apply(temp_pos[,c(temp_side,temp_side+5,temp_side+10,temp_side+15,temp_side+20)], 1, min)
    }else{
      temp_side <- which(colnames(temp_pos) =="h1_dis")
      dis_rank <- apply(temp_pos[,c(temp_side,temp_side+5,temp_side+10,temp_side+15,temp_side+20)], 1, which.min)
      dis_dis <- apply(temp_pos[,c(temp_side,temp_side+5,temp_side+10,temp_side+15,temp_side+20)], 1, min)
    }
    
    curr_baller <-10
    candid_player <-20
    delay <- 8
    flag <- 1
    flag2 <- 1
    if(nrow(temp_pos)>delay){
      for (k in 1:(nrow(temp_pos)-delay)){
        temp_baller <- dis_rank[[k]]
        #print(temp_baller)
        #print(dis_dis[[k]])
        
        if(temp_baller!=candid_player){
          flag <- 1
        }
        if (curr_baller!= temp_baller & flag==1){
          candid <- k
          candid_player <- temp_baller
          flag <- 0
        }
        #Add dribbling
        if(flag2==0 & temp_pos[k,"z"]>1 & temp_pos[(k+1),"z"]<1){
          col_d <- paste0(temp_pos[1,"tea"],curr_baller,"_event")
          full_df[,col_d][full_df$time==temp_pos[k+1,"time"]] <- 21
        }
        
        if(dis_dis[[k]]<3.5 & curr_baller!= temp_baller){
          if(dis_rank[[k+delay]]==temp_baller &dis_dis[[k+delay]]<3.5){
            col_b <- paste0(temp_pos[1,"tea"],dis_rank[[k]],"_event")
            full_df[,col_b][full_df$time==temp_pos[k,"time"]] <- 23
            temp_curr_baller <- curr_baller
            curr_baller <- temp_baller
            if(dis_rank[[candid]]==curr_baller & flag2==0){
              col_p <- paste0(temp_pos[1,"tea"],temp_curr_baller,"_event")
              m <-0
              while(dis_dis[[candid-1-m]]>3.5){
                m <- m+1
              }
              full_df[,col_p][full_df$time==temp_pos[candid-1-m,"time"]] <- 22
              #clean between pass dribbles
              full_df[,col_p][full_df$time>temp_pos[candid-1-m,"time"] &full_df$time<=temp_pos[k,"time"]] <-NA
            }
            flag <- 1
            flag2 <- 0
            #print(curr_baller)
            
          }
        }
      }
     }
    #print(i)
    #print(nrow(temp_pos))
    
  }
}

full_df$possID <- full_df$pos
full_df$game <- gameid
full_df$time <- full_df$time1

full_df <- select(full_df, -contains("_dis"))
full_df <- select(full_df, -contains("_team"))
full_df <- select(full_df, -contains("time1"))


full_df <- full_df %>%filter(!is.na(possID))


#data_merged <- merge(x = full_df, y = pbp, by = "event_id", all.x = TRUE)
#data_merged <- data_merged[order(data_merged$quarter, -data_merged$game_clock),]

#gameid = "0021500506"

#Manipulate data to hald court, etc.
poss1 <- possession.indicator(full_df) # infer ballcarrier... takes about a minute
tdat1 <- rearrange.data(full_df, poss1) # re-shuffle columns by to ballcarrier... (2 min)

tdat1[,"event_id"] <- as.numeric(tdat1[,"event_id"])
for (i in 1:4){
  tdat1[,paste0("off", i, "_ent")] <- as.numeric(tdat1[,paste0("off", i, "_ent")])
  tdat1[,paste0("off", i, "_event")] <- as.numeric(tdat1[,paste0("off", i, "_event")])
  #full_df[,paste0("a", i, "_ent")] <- as.integer(full_df[,paste0("a", i, "_ent")])
  #full_df[,paste0("h", 1, "_ent")] <- as.integer(full_df[,paste0("h", i, "_ent")])
}

tdat1 <- offensive.halfcourt(tdat1) # transforming to offensive halfcourt
tdat1 <- offensive.ballcarrier(tdat1)
touchID1 <- get.touchID(tdat1)
covariates1 <- getAllCovars(tdat1) # get covariates... (3 min)
tdat1 <- data.frame(tdat1, touchID=touchID1, covariates1)
# save(tdat, file=sprintf("%s/tdat.Rdata", data.dir))

end_time <- Sys.time()
print(end_time - start_time)
