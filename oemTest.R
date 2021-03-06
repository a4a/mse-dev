# example.R - DESC
# mse/tests/example.R

# Copyright European Union, 2018
# Authors: Finlay Scott (EC JRC)
#          Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# ==============================================================================
# SETUP
# ==============================================================================

# THIS WAS RAN WITH BRANCH INSTALL TO GENERATE THE OBJECT: AFTER REINSTALLED FROM 
# FLR REPO AND RAN AGAIN AND COMPARE


# LOAD packages

library(mse)
library(FLa4a)
library(ggplotFL)
library(doParallel)

# LOAD data

data(ple4)
data(ple4.indices)

stk <- ple4
idx <- ple4.indices["BTS-Combined (all)"]

# VARIABLES

it <- 250 # iterations
fy <- 2030 # final year
y0 <- range(stk)["minyear"] # initial OM year
dy <- range(stk)["maxyear"] # final OM year
iy <- dy # initial year of projection (also intermediate)
#ny <- fy - iy + 1 # number of years to project from initial year
nsqy <- 3 # number of years to compute status quo metrics
vy <- ac(iy:fy) # vector of years to be projected

mpargs <- list(fy=fy, y0=y0, iy=iy, nsqy=nsqy)

# ==============================================================================
# OM conditioning
# ==============================================================================

# - Two SRRs: geomean and Bevholt

mcsave <- 500
mcmc <- mcsave*it

fit <- sca(stk, idx, fit="MCMC", mcmc = SCAMCMC(mcmc = mcmc, mcsave = mcsave, mcprobe = 0.4))

stk <- stk + fit

# skin to keep one iteration
stk0 <- qapply(stk, iterMedians)

# Fit a4a model to replicate official assessment w/MCMC

# average recruitment estimation sd
rv1 <- sqrt(mean(c(iterVars(log(rec(stk)))), na.rm=TRUE))

# average autocor lag1
# TODO acf(residuals)
ac1 <- mean(apply(window(rec(stk), end=2008)@.Data, 6, function(x)
  c(acf(c(x), plot=FALSE, lag.max=1)$acf[2])))

# BevHolt
srbh <- fmle(as.FLSR(stk0, model="bevholt"), method="L-BFGS-B", lower=c(1e-6, 1e-6), upper=c(max(rec(stk)) * 3, Inf))

# Residuals
resbh <- ar1rlnorm(rho=ac1, years=dy:fy, iters=it, margSD=rv1*2)
residuals(srbh) <- resbh

# ==============================================================================
# Refpts
# ==============================================================================

brp <- brp(FLBRP(stk0, srbh))

# ==============================================================================
# Set up operating model
# ==============================================================================

# Set up future assumptions - means of 3 years
stk <- fwdWindow(stk, brp, end=2030)

#==============================================================================
# Fleet behaviour
#==============================================================================

fb <- mseCtrl(method=hyperstability.fb, args=list(beta=0.8))

#==============================================================================
# OM projection method
#==============================================================================

proj <- mseCtrl(method=fwd.om, args=list(maxF=2))

#==============================================================================
# OM object
#==============================================================================
om <- FLom(stock=stk, sr=srbh, refpts=refpts(brp), projection=proj)#, fleetBehaviour=fb)

###############################################################################
# OEM settings
###############################################################################

#==============================================================================
# prepare objects
#==============================================================================

stk <- stock(om)

#==============================================================================
# Estimate the indices catchability from the a4a fit (without simulation)
#==============================================================================

set.seed(0)

# Use all indices
idcs <- FLIndices()
for (i in 1:length(idx)){
	# this is a simplification as if index reflects 01 January abundances
	lst <- mcf(list(idx[[i]]@index, stock.n(stk0)))
	# log catchability of index 
	idx.lq <- log(lst[[1]]/lst[[2]]) 
	# empty quant
	idx.qmu <- idx.qsig <- stock.n(iter(stk,1)) 
	# Every year has the same mean catchability
	idx.qmu[] <- yearMeans(idx.lq) 
	idx.qsig[] <- sqrt(yearVars(idx.lq))
	idx.q <- FLQuant(NA, dimnames=dimnames(stock.n(stk)))
	# Build FLQ of index catchability based on lognormal distribution with mean and sd calculated above
	idx.q <- rlnorm(it, idx.qmu, idx.qsig) 
	#idx.q[,ac(y0:iy)] <- idx.q[,ac(y0:iy)]
	idx_temp <- idx.q * stock.n(stk)
	# generate initial index
	idx_temp <- FLIndex(index=idx_temp, index.q=idx.q) 
	range(idx_temp)[c("startf", "endf")] <- c(0, 0)
	idcs[[i]] <- idx_temp
}
names(idcs) <- names(idx)

#==============================================================================
# Deviances for catch.n
#==============================================================================

set.seed(0)

catch.dev <- log(catch.n(stk))
catch.dev <- catch.dev-iterMeans(catch.dev)
Sig <- apply(catch.dev[,ac(y0:dy),1,1,,drop=TRUE], 3, function(x) cov(t(x)))
Sig <- apply(Sig, 1, mean)
Sig <- matrix(Sig, ncol=dim(catch.dev)[1])
catch.dev[,ac(vy)][] <- t(mvrnorm(it * length(vy), rep(0, nrow(Sig)), Sig))
catch.dev <- exp(catch.dev)

#==============================================================================
# OEM object
#==============================================================================

idxDev <- lapply(idcs, index.q)
names(idxDev) <- "index.q"
stkDev <- FLQuants(catch.n=catch.dev)
dev <- list(idx=idxDev, stk=stkDev)
obs <- list(idx=idcs[1], stk=stk)

oem <- FLoem(method=sampling.oem, observations=obs, deviances=dev)
#save(oem, file="oem.RData")

###############################################################################
# Implementation error
###############################################################################

iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0.1, multiplicative=TRUE))

###############################################################################
# Management procedure
###############################################################################

# general pars, add seed
mpargs$seed <- 1234

##==============================================================================
## Scenarios
##==============================================================================

##------------------------------------------------------------------------------
## base with TAC and SA
##------------------------------------------------------------------------------
#ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
#	ctrl.is = mseCtrl(method=tac.is),
#	ctrl.est = mseCtrl(method=sca.sa)))

## run new method in 2 cores with foreach
#registerDoParallel(3)
#mpargs$nblocks <- 3
#resp3b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)


##------------------------------------------------------------------------------
## base with TAC and SA
##------------------------------------------------------------------------------
##install.packages("mse", repos="http://flr-project.org/R")

#registerDoParallel(3)
#mpargs$nblocks <- 3
#resp3b0 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

#> all.equal(resp3b0, resp3b)
#[1] TRUE

##------------------------------------------------------------------------------
## base with TAC and SA
##------------------------------------------------------------------------------
##load_all("/home/gamitjo/devel/FLR/pkgs/mse")
#registerDoParallel(3)
#mpargs$nblocks <- 3
#resp3b2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

##==============================================================================
## IEM tests
##==============================================================================

#install.packages("mse", repos="http://flr-project.org/R")
#registerDoParallel(3)
#mpargs$nblocks <- 3
#set.seed(1234)
#resp3biem0 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

#load_all("/home/gamitjo/devel/FLR/pkgs/mse")
#registerDoParallel(3)
#mpargs$nblocks <- 3
#set.seed(1234)
#resp3biem1b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

#iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0, multiplicative=TRUE))

#mpargs$management_lag <- 2
#resp3biem2 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

#==============================================================================
# effort is
#==============================================================================
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=effort.is),
	ctrl.est = mseCtrl(method=perfect.sa)))

registerDoParallel(8)
mpargs$nblocks <- 250
set.seed(1234)
mpargs$management_lag <- 2
resp3beffc <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=effort.is),
	ctrl.est = mseCtrl(method=sca.sa)))

registerDoParallel(8)
mpargs$nblocks <- 250
set.seed(1234)
mpargs$management_lag <- 2
date()
resp3beffb <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
date()

# took about 15 minutes each iter cycle

#==============================================================================
# ewg 19 02 hcr
#==============================================================================
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=ewg1902.hcr, args=list(ftrg=0.3, ytrg=2024)),
	ctrl.is = mseCtrl(method=effort.is),
	ctrl.est = mseCtrl(method=perfect.sa)))

registerDoParallel(3)
mpargs$nblocks <- 3
set.seed(1234)
mpargs$management_lag <- 2
resp3beffd <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

