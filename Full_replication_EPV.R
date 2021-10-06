#rm(list = ls())
library(matlib)

getwd()
############### Loading the data ########################################

code.dir <- "./code"
data.dir <- "./data"
dat <- read.csv(file=sprintf("%s/2013_11_01_MIA_BKN.csv", data.dir))
source(sprintf("%s/constants.R", code.dir)) # loads libraries and constants used throughout code
source(sprintf("%s/graphics.R", code.dir))  # graphics/plotting functions
par(mar=c(0, 0, 0, 0))
data.plotter(dat, 1800)
#plot(mesh,asp = 1, main = "")

source(sprintf("%s/data_formatting.R", code.dir))
source(sprintf("%s/covariates.R", code.dir))

# poss <- possession.indicator(dat) # infer ballcarrier... takes about a minute
# tdat <- rearrange.data(dat, poss) # re-shuffle columns by to ballcarrier... (2 min)
# tdat <- offensive.halfcourt(tdat) # transforming to offensive halfcourt
# tdat <- offensive.ballcarrier(tdat)
# touchID <- get.touchID(tdat)
# covariates <- getAllCovars(tdat) # get covariates... (3 min)
# tdat <- data.frame(tdat, touchID=touchID, covariates)
# save(tdat, file=sprintf("%s/tdat.Rdata", data.dir))

load(sprintf("%s/tdat.Rdata", data.dir))

############### Components of hierarchical models ########################################
#players' court occupancy distributions
load(sprintf("%s/playerbases.Rdata", data.dir))
players <- read.csv(sprintf("%s/players2013.csv", data.dir))
head(players)

par(mfrow=c(1,4))
for(i in 1:4)
  spatialPlot0(df[i, ], legend=F)


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

this.player <- grep("Hickson", players$lastname)

paste(players$firstname, players$lastname)[which(H[,this.player] == 1)]

par(mfrow = c(2,5))
for(i in 1:10)
  spatialPlot1(take.basis[i, ], legend=F)

############### Loading parameters and model estimates ########################################
#############Microtransition model
#####offence
player.id <- players$player_id[which(players$firstname == "LeBron")]
load(sprintf("%s/micros/%s.Rdata", data.dir, player.id))
# x component of LeBron James' micro model during ball possession
xtable(with.ball$io.x$summary.fixed[, 1:7]) 

par(mfrow=c(1,2), mar=c(0,0,0,0))
vectorPlot(with.ball)
vectorPlot(without.ball)

#####defence
source(sprintf("%s/parameters.R", code.dir)) # loads many modeling functions
def.micro <- microDefModel(tdat)
# coefficients are a_x, c_x, and b_x from Equation 6 in paper
xtable(summary(def.micro$mod.x)$coef[, 1:4])

#############Macrotransition model

load(sprintf("%s/INLA_TAKE.Rdata", data.dir))
# coefficients for time-varying covariates in shot-taking hazard model
xtable(inla.out$summary.fixed[, 1:2])


this.player <- grep("Bosh", players$lastname)
xtable(inla.out$summary.random$p.dribble[this.player, 2:6])

n.player <- nrow(players)
xtable(inla.out$summary.random$p.int[this.player + 0:1, 2:6])

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
xtable(data.frame(param.names, values), digits=c(0,0,2))


load(sprintf("%s/INLA_TAKE.Rdata", data.dir))
vars <- paste0("b", seq(n.basis))
spat.fixed <- as.numeric(inla.out$summary.fixed["(Intercept)", "mean"] + 
                           t(take.basis) %*% inla.out$summary.fixed[vars, "mean"])
spat.random <- as.numeric(inla.out$summary.random$p.int[this.player, "mean"] + 
                            t(take.basis) %*% inla.out$summary.random$p.int[this.player + n * (1:n.basis), "mean"])

in1 <-t(take.basis)
in2 <- inla.out$summary.fixed[vars, "mean"]


load(sprintf("%s/INLA_PASS1.Rdata", data.dir))
vars <- paste0("b", seq(n.basis))
spat.fixed <- as.numeric(inla.out$summary.fixed["(Intercept)", "mean"] + 
                           t(pass1.basis) %*% inla.out$summary.fixed[vars, "mean"])
spat.random <- as.numeric(inla.out$summary.random$p.int[this.player, "mean"] + 
                            t(pass1.basis) %*% inla.out$summary.random$p.int[this.player + n * (1:n.basis), "mean"])
in1 <-t(pass1.basis)
in11 <-inla.out$summary.random$p.int[this.player + n * (1:n.basis), "mean"]
in111 <-t(pass1.basis) %*% inla.out$summary.random$p.int[this.player + n * (1:n.basis), "mean"]
in22 <- inla.out$summary.fixed[vars, "mean"]

par(mfrow=c(1,2), mar=c(1,4,1,6))
spatialPlot2(head(spat.fixed + spat.random, mesh$n),
             tail(spat.fixed + spat.random, mesh$n),
             axis.args=list(cex.axis=0.75))

#############Transition probability matrices

player.id <- players$player_id[grep("Wade", players$lastname)]
load(sprintf("%s/tmats/%s.Rdata", data.dir, player.id))
names(tmat.ind)

#############EPV
source(sprintf("%s/parameters.R", code.dir))
hyper <- getHyperParams(tdat) # makes sure all parameter inference is loaded
ev.out <- evLineups(tdat) # coarsened state EVs for each offensive lineup in tdat

lineup.ids <- ev.out$teammates.all[1, ]
this.lineup <- players[match(lineup.ids, players$player_id), ]
this.lineup[, 2:4]

lineup.states <- paste(rep(this.lineup$lastname, each=14), state_nms) # state names
xtable(data.frame(state=lineup.states, EV=ev.out$evs[[2]])[grep("James", lineup.states), ], digits=2)

source(sprintf("%s/EPV_calcs.R", code.dir))
draw.raw <- multiresDraw(tdat, hyper, def.micro, ev.out, nmic=25, save.positions=F)
draw <- compressEPV(tdat, draw.raw$fv.epv.list)
load(sprintf("%s/draw.Rdata", data.dir))





