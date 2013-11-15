#====================================================
# tests to speed up l2a
#====================================================

#----------------------------------------------------
# S3 function for compilation with bytecode compiler
#----------------------------------------------------

l2ac <- function(object, model, stat="sum", weights=FLQuant(1, dimnames=dimnames(object)), ...){
	# constants
	cat("Converting lengths to ages ...\n")
	dnms <- dimnames(object)
	if(!all.equal(dnms, dimnames(weights))) stop("Weights must have the same dimensions as the data.")
	len <- as.numeric(dnms[[1]])
	mit <- niters(model)
	qit <- length(dnms[[6]]) 
	if(mit>1 & qit>1 & mit!=qit) stop("Can not operate with diferent iterations in each object.")

	# model
	mod <- FLModelSim(model=grInvMod(model), params=params(model), vcov=vcov(model), distr=distr(model))
	age <- floor(predict(mod, len=len))

	# aggregates all lengths above linf
	age <- apply(age, 2, function(x){
		x[which.max(x[!is.infinite(x)]):length(x)] <- max(x[!is.infinite(x)], na.rm=T)
		x
	})
	# slicing and aggregating

	# new flq
	dnms[[1]] <- as.character(sort(unique(c(age))))
	if(mit>qit) dnms[[6]] <- as.character(1:mit)
	flq <- FLQuant(NA, dimnames=dnms, quant="age")

	# loop :(

	# both have one iter
	if(mit>qit){
		object <- object[,,,,,rep(1, mit)]		
		dimnames(object)$iter <- 1:mit
		weights <- weights[,,,,,rep(1, mit)]
		dimnames(weights)$iter <- 1:mit
		}
	if(mit<qit){
		age <- age[,rep(1,qit)]
		dimnames(age)$iter <- 1:qit
		}
	
	lst <- apply(age, 2, split, x=len)

	if(stat=="sum"){
		for(j in names(lst)){
			for(i in dnms[[1]]){
				flq[i,,,,,j] <- quantSums(object[as.character(lst[[j]][[i]]),,,,,j])
			}	 
		}
	} else if(stat=="mean"){
		for(j in names(lst)){
			for(i in dnms[[1]]){
				wts <- weights[as.character(lst[[j]][[i]]),,,,,j]
				if(dim(wts)[1]!=0){
					wts <- apply(wts, 2:6, function(x) x/sum(x))
					flq[i,,,,,j] <- quantSums(object[as.character(lst[[j]][[i]]),,,,,j]*FLQuant(wts))
				}
			}
		}
	}
	units(flq) <- units(object)
	flq
}

l2abc <- cmpfun(l2ac)

system.time(l2abc(propagate(cth,10), mvrnorm(10, vbObj)))
system.time(l2a(propagate(cth,10), mvrnorm(10, vbObj)))


deadloop <- function(object, idx1, idx2, flq){
		for(j in idx1){
			for(i in idx2){
				flq[i,,,,,j] <- apply(object[i,,,,,j], 2:6, sum)
			}
		}
		flq
}


