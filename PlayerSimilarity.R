rm(list = ls())

code.dir <- "./code"
data.dir <- "./data"

#load(sprintf("%s/playerbases.Rdata", data.dir))

source(sprintf("%s/data_formatting.R", code.dir))
source(sprintf("%s/covariates.R", code.dir))

poss <- possession.indicator(full_df) # infer ballcarrier... takes about a minute

 haha <- data.frame((sapply(poss,c)))
# tdat <- rearrange.data(dat, poss) # re-shuffle columns by to ballcarrier... (2 min)
# tdat <- offensive.halfcourt(tdat) # transforming to offensive halfcourt
# tdat <- offensive.ballcarrier(tdat)
# touchID <- get.touchID(tdat)
# covariates <- getAllCovars(tdat) # get covariates... (3 min)
# tdat <- data.frame(tdat, touchID=touchID, covariates)