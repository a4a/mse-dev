###############################################################################
# EJ(20150519)
# MSE for EWG 16-02
# NOTE: The first intermediate year must be the last on the assessment so that
#	the OM has information for the MPs assessment/intermediate year.
# alt run: 1507054.26
###############################################################################

#==============================================================================
# libraries and auxiliary functions
#==============================================================================
rm(list=ls())
library(FLa4a)
library(FLash)
library(FLAssess)
library(ggplotFL)
library(FLBRP)
library(parallel)
source("funs_hcrtotest.R")
set.seed(0)

#==============================================================================
# Read data
#==============================================================================

stock_name <- "hake_091011"

stock_file <- paste(stock_name,"_50it.RData",sep="")
load(stock_file)
# Unpack it
stk20 <- stock_data$stk20
bstk <- stock_data$bstk
idx <- stock_data$idx
# Skinny the FLIndices to just the first one to speed up
idx <- idx[1]
srbh <- stock_data$srbh
srbh.res <- stock_data$srbh.res
srgm <- stock_data$srgm
srgm.res <- stock_data$srgm.res
refpts <- stock_data$refpts

# Check for decrease in F in 2017
#fbar(bstk)
#fbar(stk20) # yes
# Check SRR are different
#summary(srbh)
#summary(srgm) # yes
# residuals
#srgm.res # yes
#srbh.res

#==============================================================================
# Variables and settings
#==============================================================================

it <- dim(stock(stk20))[6] # iterations
#iy <- 2017 # initial year - any way of scraping this out of the stk object?
iy <- an(dimnames(catch(stk20))$year[which(is.na(iter(catch(stk20),1)))[1]]) - 1 # last year of data before NA in catch - dodgy
fy <- range(stk20)["maxyear"] # final year to project to
vy <- ac(iy:fy)
fmsy_year <- 2020
y0 <- range(stk20)["minyear"] # initial data year 
nsqy <- 3

# Implementation error function
iemset <- list(fun="rlnorm", mean=0, sd=0.05, multiplicative=TRUE)

#scenarios <- get_scenarios()
source("make_scenarios.R")

#==============================================================================
# Run simulations
#==============================================================================

dti <- date()
res <- mclapply(split(scenarios, 1:nrow(scenarios)), function(scn){
#res <- lapply(split(scenarios, 1:nrow(scenarios)), function(scn){
    set.seed(0)
    cat(stock_name, "\n")
    cat(paste(scn[,c(22, 1,2,3,4,7,11,13,15,18,20)]), "\n")

    # Get the stock and sr objects
    stk <- get(scn$stk)
    name(stk) <- scn$id
    sr <- get(scn$sr)
    sr.res <- get(scn$sr.res)

    # Fixed objects with intermediate year included
    TAC <- FLQuant(NA, dimnames=list(TAC="all", year=vy, iter=1:it))
    TAC[,ac(iy)] <- catch(stk)[,ac(iy)]
    # Target F
    EFF <- FLQuant(NA, dimnames=list(EFF="all", year=vy, iter=1:it))
    EFF[,ac(iy)] <- fbar(stk)[,ac(iy)]
    EFF0 <- EFF[rep(1,6)]

	# go fish
	for(i in vy[-length(vy)]){
		gc()
		ay <- an(i)
		cat(i, " > ")
		vy0 <- 1:(ay-y0) # data years (positions vector) - one less than current year
		sqy <- (ay-y0-nsqy+1):(ay-y0) # status quo years (positions vector) - one less than current year

		#----- OEM ------
		# Generating the 'perceived' stock from the 'true' stock
		stk0 <- stk[,vy0] # Only data years
		# *** Add underreporting here if necessary ***
        # Here, assumes perfect knowledge of removals
		catch.n(stk0) <- (catch.n(stk0) + 1) # avoid zeros
		# Generate the indices - Just data years
		idx0 <- lapply(idx, function(x) x[,vy0])
        # Generate full index up to projection year
        for (idx_count in 1:length(idx)){
            index(idx[[idx_count]])[,i] <- stock.n(stk)[,i]*index.q(idx[[idx_count]])[,i]
        }

		#----- Management ------
        # Is perceived stock based on an assessment?

        if (scn$runAssessment){
            #fmod <- getFmod(stk0, dfm=c(2/3, 2/3))
            #fmod <- ~factor(age) + breakpts(year, ay-4)
            #qmod <- getQmod(idx0)
            #fit0 <- sca(stk0, idx0, fmodel=fmod, qmodel=qmod)
            fit0 <- sca(stk0, idx0) # default settings
            stk0 <- stk0 + fit0
        }
        #plot(FLStocks(perc = stk0[,vy0], true = pstk[,vy0]))
		fdy <- fbar(stk0)[,ac(ay-1)] # Perceived F is final perceived data year
		#fdy <- yearMeans(fbar(stk0)[,sqy]) # Perceived F is average of last perceived years
		EFF0[1,ac(ay)] <- fdy

        # Ref pts error - only in first year of projection
        # Calc new ref pts based on first perceived stock
		if(ac(ay)==vy[1] & scn$refptsError){
			stk00 <- qapply(stk0, iterMedians)
			sr0 <- fmle(as.FLSR(stk00, model="segreg")) # Or use alternative model?
			brp0 <- brp(FLBRP(stk00, sr0))
			scn$ftrg <- refpts(brp0)["msy","harvest"]
			scn$blim <- min(iterMedians(ssb(stk0)))
			scn$bsafe <- scn$blim*1.4
            # Update FSQ too
            dim2 <- dim(harvest(stk00))[2]
            scn$fsq <- c(yearMeans(fbar(stk00)[,(dim2-nsqy+1):dim2]))
		}

        #------ HCR ------
        EFF <- getF(scn$ftrg, fdy, scn$mxy, ay, EFF, multiplicative=scn$multiplicative, correction=scn$correction)
        EFF0[2,ac(ay+1)] <- EFF[,ac(ay+1)]
        # If Baseline - EFF = Fsq, ignore biomass safeguard?

		# biomass safeguard
        if(!is.na(scn$biomassRecovery)){
            v0 <- c(ssb(stk0)[,ac(ay-1)]<=scn$bsafe) # which iters in last data year are below bsafe
            ry <- fdy
            ry[] <- 1-1/scn$biomassRecovery 
            EFF[,ac(ay+1),,,,v0] <- EFF[,ac(ay),,,,v0]*ry[,,,,,v0] # Apply multiplier to target Fs for the recovery iters
            # Alternative - take the minimum
            # EFF[,ac(ay+1),,,,v0] <- pmin(c(EFF[,ac(ay),,,,v0]), c(EFF[,ac(ay),,,,v0]*ry[,,,,,v0])) # If less than current Ftrgt, Apply multiplier to target Fs for the recovery iters
        }
		EFF0[3,ac(ay+1)] <- EFF[,ac(ay+1)]

        # Make the control for ay+1 (f based) from the F trajectory values
		ctrl <- getCtrl(c(EFF[,ac(ay+1)]), "f", ay+1, it)

        # Currently not implemented
		# biomass limit
		#v0 <- c(ssb(stk0)[,ac(ay-1)]<=scn$blim)
		#ctrl@trgtArray[,"val",v0] <- 0

        # Effort correction
		if(scn$managementType=="effortCorrection" & ay>vy[1]){
			fperc <- fbar(stk0)[,ac(vy[1]:(ay-1))]
			fobj <- EFF[,ac(vy[1]:(ay-1))]
			ry1 <- median(fobj/fperc)
			EFF[,ac(ay+1)] <- EFF[,ac(ay+1)]*ry1
			ctrl@trgtArray[,"val",] <- c(EFF[,ac(ay+1)])
		}
		EFF0[4,ac(ay+1)] <- EFF[,ac(ay+1)]

		if(scn$managementType=="tac"){
            fsq0 <- yearMeans(fbar(stk0)[,sqy]) # Use status quo years defined above
			ctrl <- getCtrl(c(fsq0, ctrl@trgtArray[,"val",]), "f", c(ay, ay+1), it)
			stkTmp <- stf(stk0, 2)
            gmean_rec <- c(exp(yearMeans(log(rec(stk0)[,sqy]))))
			stkTmp <- fwd(stkTmp, ctrl=ctrl, sr=list(model="mean", params = FLPar(gmean_rec,iter=it)))
			# Get TAC for following year that results from hitting the F in STF
			TAC[,ac(ay+1)] <- catch(stkTmp)[,ac(ay+1)]
			ctrl <- getCtrl(c(TAC[,ac(ay+1)]), "catch", ay+1, it)
		}

        # Technical measures
		if(scn$techMeasures & ac(ay)==vy[1]){
			py <- ac((ay+1):fy)
			sold <- snew <- harvest(stk)[,ac(ay+1)]
            snew[1] <- sold[1] * rlnorm(it, log(scn$techMeasuresEffect), scn$techMeasuresEffectSD)
			#snew[-1] <- (sum(sold)-sum(snew[1]))/sum(snew[-1])*snew[-1] # orig
            dif <- quantSums(sold) - quantSums(snew) # difference in total area of original and new with adjusted first age
            snew[-1,] <- snew[-1,] %*% ((quantSums(sold[-1,]) + dif) / quantSums(sold[-1.]))
            #quantSums(sold) # check they are the same
            #quantSums(snew)
			harvest(stk)[,py] <- snew
		} 

		#--------- IEM --------------
        # iem parameters set at top: iemset
        # Puts random noise on top - mult or additive
		if(scn$iem) ctrl <- iem(iemset, ctrl)
		EFF0[5,ac(ay+1)] <- ctrl@trgtArray[,"val",]

		#--------- OM --------------
		# hyperstability or hyperdepletion
        # Only operates on F targets - so nothing happens to TAC control
		ctrl <- e2f(ctrl, beta = scn$beta.om)
		EFF0[6,ac(ay+1)] <- ctrl@trgtArray[,"val",]

        # Project the OM
        if (scn$useSRResiduals){
            stk <- fwd(stk, ctrl=ctrl, sr=sr, sr.residuals = sr.res[,ac(ay+1)], sr.residuals.mult = TRUE)
        }
        else {
            stk <- fwd(stk, ctrl=ctrl, sr=sr)
        }
	}
    cat("\n")
    attr(stk, "EFF") <- EFF0
    stk
}
, mc.cores=30

)
dtf <- date()


save.image(paste(stock_name,"_hcrtotest.RData",sep=""))

