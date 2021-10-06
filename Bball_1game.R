library(sp)
library(RCurl)
library(knitr)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(xtable)
library(data.table)
library(matrixStats)
library(RColorBrewer)
library(fields)
library(magick)
library(foreach)
library(doParallel)
library(doMC)
library(progress)
library(INLA)
library(ggplot2)
library(jsonlite)
#install.packages("vctrs")
#library(tidyverse)
library(vctrs)
#install.packages("rjson")
library("rjson")

#Load the existing code and data
code.dir <- "./code"
data.dir <- "./data"

#load the one game from 2013
dat <- read.csv(file=sprintf("%s/2013_11_01_MIA_BKN.csv", data.dir))

#dat_jason <- read.csv(file=sprintf("%s/0021500506.json", data.dir))
dat_jason  <- fromJSON(file=sprintf("%s/0021500506.json", data.dir))
print(dat_jason)
raw_game <- jsonlite::fromJSON(file=sprintf("%s/0021500506.json", data.dir))
game <- sportvu_df("/0021500336.json")



source(sprintf("%s/constants.R", code.dir)) # loads libraries and constants used throughout code
source(sprintf("%s/graphics.R", code.dir))  # graphics/plotting functions
par(mar=c(0, 0, 0, 0))
data.plotter(dat, 1600)

##Transforming Data
source(sprintf("%s/data_formatting.R", code.dir))
source(sprintf("%s/covariates.R", code.dir))
poss <- possession.indicator(dat) # infer ballcarrier... takes about a minute
tdat <- rearrange.data(dat, poss) # re-shuffle columns by to ballcarrier... (2 min)
tdat <- offensive.halfcourt(tdat) # transforming to offensive halfcourt
tdat <- offensive.ballcarrier(tdat) #subset to only plays where ballcarrier in offensive halfcourt and moving
touchID <- get.touchID(tdat) #ID for each player-touch sequence
covariates <- getAllCovars(tdat) # get covariates... (3 min)
tdat <- data.frame(tdat, touchID=touchID, covariates)


###Player similarity adjacency matrix, H
load(sprintf("%s/playerbases.Rdata", data.dir)) # court occupancy patterns
players <- read.csv(sprintf("%s/players2013.csv", data.dir))
par(mfrow=c(1,5))
for(i in 200:204)
  spatialPlot0(df[i, ], legend=F) #G
#players[which(players$lastname =="Bryant"),]

par(mfrow=c(1,5))
for(i in 1:5)
  spatialPlot0(nmf.basis[i, ], legend=F) #non-negative matrix factorization to obtain a rank 5 


df.lowrank <- nmf.coef %*% nmf.basis
par(mfrow=c(1,5))
for(i in 1:5)
  spatialPlot0(df.lowrank[i, ], legend=F) #Low rank court occupancy distributions


K <- matrix(NA, nrow=nrow(df), ncol=nrow(df))
for(i in 1:nrow(K)){
  this.coef <- nmf.coef[i, ] / sum(nmf.coef[i, ])
  K[i, ] <- apply(nmf.coef, 1, function(r) sum((r / sum(r) - this.coef)^2))
}
H <- 0 * K
for(i in 1:nrow(H)){
  inds <- order(K[i, ])[1:8 + 1]
  H[i,inds] <- H[inds, i] <- 1
}

this.player <- grep("Allen", players$lastname)
paste(players$firstname, players$lastname)[which(H[this.player, ] == 1)]

######Spatial effect basis functions
par(mfrow = c(2,5))
for(i in 1:10)
  spatialPlot1(take.basis[i, ], legend=F) ###players' macrotransition entry 

######Microtransition model
player.id <- players$player_id[which(players$firstname == "LeBron")]
load(sprintf("%s/micros/%s.Rdata", data.dir, player.id))
# x component of LeBron James' micro model during ball possession
#xtable(with.ball$io.x$summary.fixed[, 1:5]) 

par(mfrow=c(1,2), mar=c(0,0,0,0))
vectorPlot(with.ball)
vectorPlot(without.ball)

#Defence
source(sprintf("%s/parameters.R", code.dir)) # loads many modeling functions
def.micro <- microDefModel(tdat)
# coefficients are a_x, c_x, and b_x from Equation 6 in paper
#xtable(summary(def.micro$mod.x)$coef[, 1:3])

######Macrotransition model
load(sprintf("%s/INLA_TAKE.Rdata", data.dir))
# coefficients for time-varying covariates in shot-taking hazard model
#xtable(inla.out$summary.fixed[, 1:2])

#this.player <- grep("Bryant", players$lastname)
#xtable(inla.out$summary.random$p.dribble[this.player, 2:6])

n.player <- nrow(players)
# inference for Chris Bosh's intercept and first basis coefficient
#xtable(inla.out$summary.random$p.int[this.player + 0:1, 2:6])
#xtable(inla.out$summary.random$p.b1[this.player + 0:1, 2:6]) # identical

param.names <- row.names(inla.out$summary.fixed)
n <- nrow(players)
player.params <- matrix(NA, nrow=n, ncol=length(param.names))
y.fix <- inla.out$summary.fixed[, "mean"] # fixed effects
temp <- names(inla.out$summary.random)
basis.inds <- c(which(temp == "p.int"), grep("p.b[0-9][0-9]*", temp))
cov.inds <- setdiff(seq(length(inla.out$summary.random)), basis.inds)
for(pl in 1:n) {
  # add players' random effects to fixed effects
  y.rand <- c(inla.out$summary.random$p.int[pl, "mean"], 
              sapply(cov.inds, 
                     function(k) inla.out$summary.random[[k]][pl, "mean"]),
              inla.out$summary.random$p.b1[pl + n * (1:n.basis), "mean"])
  player.params[pl, ] <- y.fix + y.rand
}

values <- player.params[this.player, ]
ranks <- apply(player.params, 2, function(col) rank(col)[this.player]) # increasing order
#xtable(data.frame(param.names, values, ranks), digits=c(0,0,2,0))


vars <- paste0("b", seq(n.basis))
spat.fixed <- as.numeric(inla.out$summary.fixed["(Intercept)", "mean"] + 
                           t(take.basis) %*% inla.out$summary.fixed[vars, "mean"])
spat.random <- as.numeric(inla.out$summary.random$p.int[this.player, "mean"] + 
                            t(take.basis) %*% inla.out$summary.random$p.int[this.player + n * (1:n.basis), "mean"])

par(mfrow=c(1,2), mar=c(1,4,1,6))
spatialPlot1(spat.fixed + spat.random, axis.args=list(cex.axis=0.75))
spatialPlot1(spat.random, axis.args=list(cex.axis=0.75))


#Shooting
load(sprintf("%s/INLA_PASS1.Rdata", data.dir))
vars <- paste0("b", seq(n.basis))
spat.fixed <- as.numeric(inla.out$summary.fixed["(Intercept)", "mean"] + 
                           t(pass1.basis) %*% inla.out$summary.fixed[vars, "mean"])
spat.random <- as.numeric(inla.out$summary.random$p.int[this.player, "mean"] + 
                            t(pass1.basis) %*% inla.out$summary.random$p.int[this.player + n * (1:n.basis), "mean"])

par(mfrow=c(1,2), mar=c(1,4,1,6))
spatialPlot2(head(spat.fixed + spat.random, mesh$n),
             tail(spat.fixed + spat.random, mesh$n),
             axis.args=list(cex.axis=0.75))

#Transition probability matrix
player.id <- players$player_id[grep("Wade", players$lastname)]
load(sprintf("%s/tmats/%s.Rdata", data.dir, player.id))
tmat.ind$absorbs

###Coarsened state expected point values
source(sprintf("%s/parameters.R", code.dir))
hyper <- getHyperParams(tdat) # makes sure all parameter inference is loaded
ev.out <- evLineups(tdat) # coarsened state EVs for each offensive lineup in tdat

lineup.ids <- ev.out$teammates.all[2, ]
this.lineup <- players[match(lineup.ids, players$player_id), ]
this.lineup[, 2:4]

lineup.states <- paste(rep(this.lineup$lastname, each=14), state_nms) # state names
#xtable(data.frame(state=lineup.states, EV=ev.out$evs[[2]])[grep("Haslem", lineup.states), ], digits=2)

#EPV curves
#source(sprintf("%s/EPV_calcs.R", code.dir))
#draw.raw <- multiresDraw(tdat, hyper, def.micro, ev.out, nmic=50, save.positions=F)
#draw <- compressEPV(tdat, draw.raw$fv.epv.list)

#load(sprintf("%s/draw.Rdata", data.dir))
#names(draw)

#transformed.data.plotter(tdat, 200)
#draw$probs[200, ]
#draw$vals[200, ]

#plot(720 - tdat$game_clock[1:200], draw$epv[1:200], xlab="game clock", ylab="EPV")


#load(sprintf("%s/combined.epv.draws.Rdata", data.dir))
#e.dat <- combineDatEPV(dat, epv.table)

#par(xpd=NA, bty="n", mfrow=c(1, 2))
#poss.1 <- which(e.dat$possID == 1)
#plot(720 - e.dat$game_clock[poss.1], e.dat$epv.smooth[poss.1], 
#     xlab="game clock", ylab="EPV", type="l", lwd=2, ylim=c(.5, 1.5))
#points(720 - e.dat$game_clock[poss.1], e.dat$epv[poss.1], pch=20, cex=0.5)
#
#poss.90 <- which(e.dat$possID == 90) # possession shown in paper
#plot(720 - e.dat$game_clock[poss.90], e.dat$epv.smooth[poss.90], 
#     xlab="game clock", ylab="EPV", type="l", lwd=2, ylim=c(.5, 1.8))
#points(720 - e.dat$game_clock[poss.90], e.dat$epv[poss.90], pch=20, cex=0.5)

#makeGIF(e.dat, which(e.dat$possID == 90), "poss_90") # takes a few minutes
#makeGIF(e.dat, which(e.dat$possID == 24), "poss_24") # takes a few minutes

#earth <- image_read("./gifs/poss_90.gif") 


source(sprintf("%s/EPV_calcs.R", code.dir))
#dat_backup <- dat
#tdat_backup <- tdat
BB <-73
FF <-60
dis_cou <-3
counter <- cbind(c(0,0),c(dis_cou,0),c(dis_cou*2,0),c(0,dis_cou),c(0,2*dis_cou),
                 c(-dis_cou,0),c(-dis_cou*2,0),c(0,-dis_cou),c(0,-2*dis_cou),
                 c(dis_cou,dis_cou),c(-dis_cou,-dis_cou),
                 c(dis_cou,-dis_cou),c(-dis_cou,dis_cou),
                 c(2*dis_cou,dis_cou),c(2*dis_cou,-dis_cou),
                 c(2*dis_cou,2*dis_cou),c(2*dis_cou,-2*dis_cou),
                 c(-2*dis_cou,dis_cou),c(-2*dis_cou,-dis_cou),
                 c(-2*dis_cou,2*dis_cou),c(-2*dis_cou,-2*dis_cou),
                 c(-dis_cou,2*dis_cou),c(dis_cou,2*dis_cou),
                 c(dis_cou,-2*dis_cou),c(-dis_cou,-2*dis_cou))

PosA <-cbind(c("a2_x","a2_y"),c("a3_x","a3_y"),c("a4_x","a4_y"),c("a5_x","a5_y"),c("a1_x","a1_y"))
PosH <-cbind(c("h2_x","h2_y"),c("h3_x","h3_y"),c("h4_x","h4_y"),c("h5_x","h5_y"),c("h1_x","h1_y"))
dat_4q <- list()
for(j in 1:4) {            
  game_clock <- seq(720, 0, by = -0.04)
  dat_q <- dat[which(dat$quarter ==j),1:4]
  
  dat_B <-dat[which(dat$possID ==BB),]#Lebron drive in
  dat_B <-dat_B[1:(nrow(dat_B)),]
  dat_F <-dat[which(dat$possID ==FF),]#Pierce jump shot
  dat_F <-dat_F[1:nrow(dat_F),]
  fakeq <- list()
  for(i in 1:25){
    dat_B_temp <-cbind(dat_B[,6:(ncol(dat_B)-1)], possID=((i*2-1)+(50*(j-1))))
    dat_B_temp[,PosH[1,j]] <-sapply(dat_B_temp[,PosH[1,j]],function(x) x+counter[1,i])
    dat_B_temp[,PosH[2,j]] <-sapply(dat_B_temp[,PosH[2,j]],function(x) x+counter[2,i])
    dat_F_temp <-cbind(dat_F[,6:(ncol(dat_F)-1)], possID=((i*2)+(50*(j-1))))
    dat_F_temp[,PosA[1,j]] <-sapply(dat_F_temp[,PosA[1,j]],function(x) x+counter[1,i])
    dat_F_temp[,PosA[2,j]] <-sapply(dat_F_temp[,PosA[2,j]],function(x) x+counter[2,i])
    temp <- rbind(dat_B_temp,dat_F_temp)
    fakeq <-rbind(fakeq,temp)
  }
  
  dat_q <- cbind(dat_q[1:length(game_clock),], game_clock)
  dat_q <- cbind(dat_q[1:nrow(fakeq),],fakeq)
  dat_4q <-rbind(dat_4q,dat_q)
  #dat_4q[nrow(dat_4q),"a1_event"] <- 15
}

dat_4q[which(dat_4q$quarter >=3),"x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"a1_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"a1_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"a2_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"a2_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"a3_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"a3_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"a4_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"a4_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"a5_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"a5_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"h1_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"h1_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"h2_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"h2_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"h3_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"h3_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"h4_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"h4_x"],function(x) 94-x)
dat_4q[which(dat_4q$quarter >=3),"h5_x"] <-sapply(dat_4q[which(dat_4q$quarter >=3),"h5_x"],function(x) 94-x)


poss <- possession.indicator(dat_4q) # infer ballcarrier... takes about a minute
tdat <- rearrange.data(dat_4q, poss) # re-shuffle columns by to ballcarrier... (2 min)
tdat <- offensive.halfcourt(tdat) # transforming to offensive halfcourt
tdat <- offensive.ballcarrier(tdat) #subset to only plays where ballcarrier in offensive halfcourt and moving
touchID <- get.touchID(tdat) #ID for each player-touch sequence
covariates <- getAllCovars(tdat) # get covariates... (3 min)
tdat <- data.frame(tdat, touchID=touchID, covariates)


iterations <- 8
numCores <- 2
registerDoMC(cores=numCores)
#getDoParRegistered()
#getDoParWorkers()
#registerDoParallel(2) # use multicore, set to the number of our cores

foreach (i=1:8) %dopar% {
  draw.raw <- multiresDraw(tdat, hyper, def.micro, ev.out, nmic=50, save.positions=F)
  draw <- compressEPV(tdat, draw.raw$fv.epv.list)
  save(draw, file=sprintf("%s/EPVdraws/%03.f.Rdata", data.dir, i))
}
#plot(720 - tdat$game_clock[30000:31800], draw$epv[30000:31800], xlab="game clock", ylab="EPV")

source(sprintf("%s/combine_draws.R", code.dir))
load(sprintf("%s/combined.epv.draws.Rdata", data.dir))
e.dat7360 <-combineDatEPV(dat_4q, epv.table)
save(e.dat7360, file=sprintf("%s/combined.epv7360_2to5.Rdata", data.dir))

#remove(e.dat7360)
#e.dat9041 <- e.dat
#save(e.dat9041, file=sprintf("%s/combined.epv9041_1to4.Rdata", data.dir))
load(haha, file=sprintf("%s/INLA_PASS2.Rdata", data.dir))
##################Data Analysis#######################

e.dat <- e.dat1224##2
e.dat <- e.dat164165
e.dat <- e.dat9041
e.dat <- e.dat7360
e.dat <- e.dat231230


poss.1 <- which(e.dat$possID == 1)
poss.2 <- which(e.dat$possID == 2)
par(xpd=NA, bty="n", mfrow=c(1, 2))
plot(720 - e.dat$game_clock[poss.1], e.dat$epv.smooth[poss.1], 
     xlab="game clock", ylab="EPV", type="l", lwd=2, ylim=c(.8, 1.8),col="blue")
#points(720 - e.dat$game_clock[poss.2], e.dat$epv[poss.2], pch=20, cex=0.5)
plot(720 - e.dat$game_clock[poss.2], e.dat$epv.smooth[poss.2], 
     xlab="game clock", ylab="EPV", type="l", lwd=2, ylim=c(.8, 1.8),col="red")

poss.1 <- which(e.dat$possID == 1)
poss.2 <- which(e.dat$possID == 2)
poss.5 <- which(e.dat$possID == 5)
poss.7 <- which(e.dat$possID == 7)
poss.9 <- which(e.dat$possID == 9)

par(xpd=NA, bty="n", mfrow=c(1, 1))
plot(720 - e.dat$game_clock[poss.1], e.dat$epv.smooth[poss.1], 
     xlab="game clock", ylab="EPV", type="l", lwd=2, ylim=c(.8, 1.8),col="blue")
#points(720 - e.dat$game_clock[poss.1], e.dat$epv[poss.1], pch=20, cex=0.5)

lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 109)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 5)],col="pink")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 7)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 9)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 11)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 13)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 15)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 17)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 19)],col="red")
lines(720 -e.dat$game_clock[poss.1], e.dat$epv.smooth[which(e.dat$possID == 21)],col="red")
#plot(720 -e.dat$game_clock[poss.3], e.dat$epv.smooth[poss.3], 
#     xlab="game clock", ylab="EPV", type="l", lwd=2, ylim=c(.8, 1.8))
#points(720 - e.dat$game_clock[poss.5], e.dat$epv[poss.5], pch=20, cex=0.5)



par(xpd=NA, bty="n", mfrow=c(1, 2))
#data.plotter(e.dat, 1)
#data.plotter(e.dat,  1+(sec+fir)*4)
data.plotter(e.dat_calc, 150)
data.plotter(e.dat_calc, 14358)


e.dat <- e.dat1224##2
q_row <-nrow(e.dat[which(e.dat$quarter==1),])
fir <- nrow(e.dat[which(e.dat$possID==1),])
sec <- nrow(e.dat[which(e.dat$possID==2),])
e.dat_calc <- cbind(mini_evp=c(-1),mini_evp_index=c(-1),mini_s=c(-1),mini_s_index=c(-1),e.dat)
for (k in 1:4){
    e.dat <-  e.dat1224
    e.dat <-e.dat[which(e.dat$quarter==k),]
    
    mini_evp1 <- matrix(0, nrow = fir, ncol = 2)
    mini_evp1[,2] <- 1
    mini_s_evp1 <- matrix(0, nrow = fir, ncol = 2)
    mini_s_evp1[,2] <- 1
    for(j in 1:fir){
      evp_p1 <- e.dat[j,"epv"]
      evp_s_p1 <- e.dat[j,"epv.smooth"]
      mini_evp1[j,] <-c(evp_p1,1)
      mini_s_evp1[j,] <-c(evp_s_p1,1)
      for (i in 1:25){
        #if(evp_p2[i]=="NULL" || is.na(evp_p2[i])) evp_p2[i] = 50
        #if(evp_s_p2[i]=="NULL" || is.na(evp_s_p2[i])) evp_s_p2[i] = 50
        #print(i)
        #print(evp_p2)
        if(!is.na(evp_p1)){
            ee <-e.dat[j+(sec+fir)*(i-1),"epv"]
            ee_s <- e.dat[j+(sec+fir)*(i-1),"epv.smooth"]
            if (evp_p1 > ee){
              
              mini_evp1[j,] <- c(ee,i)
              evp_p1 <- ee
            }
            if (evp_s_p1 > ee_s){
              mini_s_evp1[j,] <- c(ee_s,i)
              evp_s_p1 <- ee_s
            }
        }
      }
    }
    mini_evp2 <- matrix(0, nrow = sec, ncol = 2)
    mini_evp2[,2] <- 1
    mini_s_evp2 <- matrix(0, nrow = sec, ncol = 2)
    mini_s_evp2[,2] <- 1
    for(j in 1:sec){
      evp_p2 <- e.dat[fir+j,"epv"]
      evp_s_p2 <- e.dat[fir+j,"epv.smooth"]
      mini_evp2[j,] <-c(evp_p2,1)
      mini_s_evp2[j,] <-c(evp_s_p2,1)
      for (i in 1:25){
        #if(evp_p2[i]=="NULL" || is.na(evp_p2[i])) evp_p2[i] = 50
        #if(evp_s_p2[i]=="NULL" || is.na(evp_s_p2[i])) evp_s_p2[i] = 50
        #print(i)
        #print(evp_p2)
        if(!is.na(evp_p2)){
          ee <-e.dat[fir+j+(sec+fir)*(i-1),"epv"]
          ee_s <- e.dat[fir+j+(sec+fir)*(i-1),"epv.smooth"]
          if (evp_p2 > ee){
            
            mini_evp2[j,] <- c(ee,i)
            evp_p2 <- ee
          }
          if (evp_s_p2 > ee_s){
            mini_s_evp2[j,] <- c(ee_s,i)
            evp_s_p2 <- ee_s
          }
        }
      }
    }

    e.dat_calc[(q_row*(k-1)+1):(q_row*(k-1)+fir+sec),1:4] <-rbind(cbind(mini_evp1,mini_s_evp1),cbind(mini_evp2,mini_s_evp2))

}
#1s player
pos_H_ID <-c(1,51,101,151)
pos_A_ID <-c(2,52,102,152)
H_dev <- matrix(0, nrow = 4, ncol = 2)
for(i in pos_H_ID){
  fir_p <-e.dat_calc[which(e.dat_calc$possID==i),]
  fir_p <-fir_p[which(!is.na(fir_p$mini_evp)),]
  fir_p_evp <- fir_p$epv - fir_p$mini_evp
  fir_p_evp <- mean(fir_p_evp)
  fir_p_evp_s <-fir_p$epv.smooth - fir_p$mini_s
  fir_p_evp_s <-mean(fir_p_evp_s)
  H_dev[which(pos_H_ID %in% i),1] <- fir_p_evp
  H_dev[which(pos_H_ID %in% i),2]  <- fir_p_evp_s
}

A_dev <- matrix(0, nrow = 4, ncol = 2)
for(j in pos_A_ID){
  fir_p <-e.dat_calc[which(e.dat_calc$possID==j),]
  fir_p <-fir_p[which(!is.na(fir_p$mini_evp)),]
  fir_p_evp <- fir_p$epv - fir_p$mini_evp
  fir_p_evp <- mean(fir_p_evp)
  fir_p_evp_s <-fir_p$epv.smooth - fir_p$mini_s
  fir_p_evp_s <-mean(fir_p_evp_s)
  A_dev[which(pos_A_ID %in% j),1] <- fir_p_evp
  A_dev[which(pos_A_ID %in% j),2]  <- fir_p_evp_s
}

play_sum <- rbind(cbind(231,H_dev),cbind(230,A_dev))

#play_sum_all <- list()
play_sum_all <- rbind(play_sum_all,play_sum)
#play_sum_all <- rbind(play_sum_all[1:8,],play_sum_all[17:24,])

quat <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4)
play_sum_all <- play_sum_all[2:nrow(play_sum_all),2:4]
play_sum_all <-cbind(quat,play_sum_all)

a <-c(3235, 3501, 2989, 330047,61849,214152,226806,172631,3315,3501,2989,330047,61849,3062,3497,75344,172537,3315,3501,292404,296572,61849,214152,226806,3235,3501,173147,292404,3062,3497,214152,172631,3235, 3501,66858,2989,61849,3062,214152,172631)
play_sum_all <- cbind(a, play_sum_all)
play_sum_all  <-cbind(play_sum_all, Player = 1)
temp <- play_sum_all
play_sum_all[,"Player"] <-sapply(play_sum_all[,"a"],function(x) players$lastname[players$player_id==x])

#temp <- cbind(temp,position = 1,height =1,weight=1,byear=1,rookie=1)
#temp[,"position"] <-sapply(play_sum_all[,"a"],function(x) players$position[players$player_id==x])
#temp[,"height"] <-sapply(play_sum_all[,"a"],function(x) players$height[players$player_id==x])
#temp[,"weight"] <-sapply(play_sum_all[,"a"],function(x) players$weight[players$player_id==x])
#temp[,"byear"] <-sapply(play_sum_all[,"a"],function(x) players$byear[players$player_id==x])
#temp[,"rookie"] <-sapply(play_sum_all[,"a"],function(x) players$rookie[players$player_id==x])
#play_sum_all <-temp

#play_sum_all <-data.frame(play_sum_all)
#names(play_sum_all)[names(play_sum_all) == "V5"] <- "epv_smooth"

play_sum_all$up <-vec
vec <-c(4,4,4,4,-8,-8,-8,-8,5,5,5,5,-9,-9,-9,-9,5,5,5,5,-1,-1,-1,-1,14,14,14,14,-14,-14,-14,-14,5,5,5,5,-5,-5,-5,-5)
vec2 <- c(656,656,656,656,453,453,453,453,522,522,522,522,705,705,705,705,296,296,296,296,266,266,266,266,199,199,199,199,180,180,180,180,18,18,18,18,34,34,34,34)
vec3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0)
play_sum_all$made <-vec3
#play_sum_all$clock <-vec2
#save(play_sum_all, file=sprintf("%s/play_sum_all.Rdata", data.dir))
dd  <-  as.data.frame(matrix(unlist(play_sum_all), nrow=length(unlist(play_sum_all[1]))))

# Grouped Bar Plot
p<-ggplot(data=dd, aes(x=V14, y=V4, fill=V14)) 
p + geom_boxplot()+ ggtitle("Avg Points Above the Optimal EPV for Made(1) and Miss(0)") +
  ylab("Avg Above Optimal EPV(pts)") + xlab("Made/Miss") + labs(fill = "Made/Miss")

p<-ggplot(data=dd, aes(x=V14, y=V5, fill=V14)) 
p + geom_boxplot()+ ggtitle("Avg Points Above the Optimal EPV Smooth for Made(1) and Miss(0)") +
  ylab("Avg Above Optimal EPV Smooth (pts)") + xlab("Made/Miss") + labs(fill = "Made/Miss")

dd_m <- dd[which(dd$V3!=231),]

p<-ggplot(data=dd, aes(x=factor(V6), y=V4, fill=V2)) 
p + geom_boxplot()+ ggtitle("Avg Points Above the Optimal EPV vs Time left") +
  ylab("Avg Above Optimal EPV(pts)") + xlab("Time Left") + labs(fill = "Quarter")

p<-ggplot(data=dd, aes(x=factor(V6), y=V4, shape = factor(V2))) 
p + geom_point(aes(color=factor(V2),size =factor(V2)))+ ggtitle("Avg Points Above the Optimal EPV vs Players") +
  ylab("Pts Above Optimal EPV") + xlab("Player") + labs(color = "Quarter")+
  scale_shape_manual(values=c(15, 16, 17,18))+ scale_size_manual(values=c(3,3,3,5))+ theme_minimal()

######################taking out ability#####################
pos_H_ID <-c(1,51,101,151)
pos_A_ID <-c(2,52,102,152)
H_dev <- matrix(0, nrow = 4, ncol = 2)
x_loc <-c("h2_x","h3_x","h4_x","h5_x")
y_loc <-c("h2_y","h3_y","h4_y","h5_y")
#x_loc <-c("h1_x","h2_x","h3_x","h4_x")
#y_loc <-c("h1_y","h2_y","h3_y","h4_y")
for(i in pos_H_ID){
  fir_p <-e.dat_calc[which(e.dat_calc$possID==i),]
  fir_p <-fir_p[which(!is.na(fir_p$mini_evp)),]
  print("nkaka")
  for(m in 1:(nrow(fir_p)-1)){
    print(m)
    next_dis <-sqrt((fir_p[m+1,x_loc[which(pos_H_ID %in% i)]]-counter[1,fir_p[m,"mini_evp_index"]]-fir_p[m,x_loc[which(pos_H_ID %in% i)]])^2+(fir_p[m+1,y_loc[which(pos_H_ID %in% i)]]-counter[2,fir_p[m,"mini_evp_index"]]-fir_p[m,y_loc[which(pos_H_ID %in% i)]])^2)
    current_dis <- sqrt((counter[1,fir_p[m,"mini_evp_index"]])^2+(counter[2,fir_p[m,"mini_evp_index"]])^2)
    cat("next: ",next_dis," current: ",current_dis)
    print(next_dis)
    if(next_dis<=current_dis){
      #print("innnnn")
      #print(fir_p[m+1,"mini_evp"])
      #print(fir_p[m+1,"epv"])
      fir_p[m+1,"mini_evp"] <- fir_p[m+1,"epv"]}
  }
  fir_p_evp <- fir_p$epv - fir_p$mini_evp
  fir_p_evp <- mean(fir_p_evp)
  fir_p_evp_s <-fir_p$epv.smooth - fir_p$mini_s
  fir_p_evp_s <-mean(fir_p_evp_s)
  H_dev[which(pos_H_ID %in% i),1] <- fir_p_evp
  H_dev[which(pos_H_ID %in% i),2]  <- fir_p_evp_s
}


#x_loc <-c("a1_x","a2_x","a3_x","a4_x")
#y_loc <-c("a1_y","a2_y","a3_y","a4_y")
x_loc <-c("a2_x","a3_x","a4_x","a5_x")
y_loc <-c("a2_y","a3_y","a4_y","a5_y")

A_dev <- matrix(0, nrow = 4, ncol = 2)
for(j in pos_A_ID){
  print("n")
  fir_p <-e.dat_calc[which(e.dat_calc$possID==j),]
  fir_p <-fir_p[which(!is.na(fir_p$mini_evp)),]
  for(m in 1:(nrow(fir_p)-1)){
    next_dis <-sqrt((fir_p[m+1,x_loc[which(pos_A_ID %in% j)]]-counter[1,fir_p[m,"mini_evp_index"]]-fir_p[m,x_loc[which(pos_A_ID %in% j)]])^2+(fir_p[m+1,y_loc[which(pos_A_ID %in% j)]]-counter[2,fir_p[m,"mini_evp_index"]]-fir_p[m,y_loc[which(pos_A_ID %in% j)]])^2)
    current_dis <- sqrt((counter[1,fir_p[m,"mini_evp_index"]])^2+(counter[2,fir_p[m,"mini_evp_index"]])^2)
    print(m)
    print(next_dis)
    print(current_dis)
    if(next_dis<=current_dis){fir_p[m+1,"mini_evp"]<- fir_p[m+1,"epv"]}
  }
  fir_p_evp <- fir_p$epv - fir_p$mini_evp
  fir_p_evp <- mean(fir_p_evp)
  fir_p_evp_s <-fir_p$epv.smooth - fir_p$mini_s
  fir_p_evp_s <-mean(fir_p_evp_s)
  A_dev[which(pos_A_ID %in% j),1] <- fir_p_evp
  A_dev[which(pos_A_ID %in% j),2]  <- fir_p_evp_s
}


play_sum_f <- rbind(cbind(231,H_dev),cbind(230,A_dev))
#play_sum_all_f <- list()
play_sum_all_f <- rbind(play_sum_all_f,play_sum_f)
#dd_f <-as.data.frame(matrix(unlist(play_sum_all_f), nrow=40))
#dd_f <- cbind(dd_f,dd[,6:14])
#dd_f <- cbind(dd[,1:2],dd_f)
#colnames(dd_f)[5] <- "V5"
p<-ggplot(data=dd_f, aes(x=V14, y=V4, fill=V14)) 
p + geom_boxplot()+ ggtitle("[MERCY]Avg Points Above the Optimal EPV for Made(1) and Miss(0)") +
  ylab("Avg Above Optimal EPV(pts)") + xlab("Made/Miss") + labs(fill = "Made/Miss")

p<-ggplot(data=dd_f, aes(x=factor(V13), y=V4, fill=V2)) 
p + geom_boxplot()+ ggtitle("[MERCY]Avg Points Above the Optimal EPV vs Time left") +
  ylab("Avg Above Optimal EPV(pts)") + xlab("Time Left") + labs(fill = "Quarter")

p<-ggplot(data=dd_f, aes(x=factor(V12), y=V4,fill=V12)) 
p + geom_boxplot()+ ggtitle("[MERCY]Avg Points Above the Optimal EPV vs Point Lead") +
  ylab("Avg Above Optimal EPV(pts)") + xlab("Time Left") + labs(fill = "Point Lead")

p<-ggplot(data=dd_f, aes(x=factor(V6), y=V4, shape = factor(V2))) 
p + geom_point(aes(color=factor(V2),size =factor(V2)))+ ggtitle("[MERCY]Avg Points Above the Optimal EPV vs Players") +
  ylab("Pts Above Optimal EPV") + xlab("Player") + labs(color = "Quarter")+
  scale_shape_manual(values=c(15, 16, 17,18))+ scale_size_manual(values=c(3,3,3,5))+ theme_minimal()
