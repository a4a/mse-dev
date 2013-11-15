# validity 1 or N; params with 3 dims params/year/iter

setClass("a4aRule",
        representation(
				timeOrder = "numeric",
				obsVar = "character",
				refVal = "FLQuant",                
				refMin = "FLQuant",
				refMax = "FLQuant",
				actVar = "character",
				model = "formula",
				params = "FLPar",
				refYr = "numeric"),
        prototype = prototype(
				timeOrder = as.numeric(NA),
				obsVar = as.character(NA),
				refVal = new("FLQuant"),                
				refMin = new("FLQuant"),
				refMax = new("FLQuant"),
				actVar = as.character(NA),
				model = NA,
				params = new("FLPar"),
				refYr = as.numeric(NA))
)

setGeneric("a4aRule", function(object, ...) standardGeneric("a4aRule"))

setMethod("a4aRule", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("a4aRule")
    # or not
  	} else {
      args <- list(...)
	  args$Class <- 'a4aRule'
      do.call("new", args)
	  }
  }
)


# accessors and replacements
genAR(a4aRule())

# predict
setMethod("predict", signature(object="a4aRule"), function (object, ...){
    args <- list(...)
    pr <- params(object)
	mod <- model(object)
	ryr <- refYr(object) 
	cvar <- match(all.vars(mod), names(args), nomatch=0)
	cvar <- cvar[cvar!=0]
	if(length(cvar)>0 & !is.na(ryr)){
		lst <- lapply(split(cvar, cvar), function(x){
			if(is(args[[x]], "FLQuant")) args[[x]][,getYidx(args[[x]], ryr)] 
		})
		args[cvar] <- lst
	} 

    res <- apply(pr, 2, function(x) {
        lst <- as.list(x)
        eval(as.list(mod)[[2]], envir = c(args, lst))
    })
    res <- matrix(res, ncol = dim(pr)[2])
    dimnames(res) <- list(pred = 1:nrow(res), iter = 1:dim(pr)[2])
    return(res)
})

# example
o1 <- a4aRule(obsVar="f", timeOrder=1, model=~Ftrg, params=FLPar(Ftrg=0.35))
predict(o1)
# it's all the same
data(ple4)
o1 <- a4aRule(obsVar="c", model=~mean(c), refYr=2006:2008)
predict(o1, c=catch(ple4))
o1 <- a4aRule(obsVar="c", model=~mean(c), refYr=-1:-3)
predict(o1, c=catch(ple4))
o1 <- a4aRule(obsVar="c", model=~mean(c), refYr=50:52)
predict(o1, c=catch(ple4))

o1 <- a4aRule(obsVar="f", timeOrder=1, model=~ifelse(c1>quantile(c,a), 1, 2), params=FLPar(a=0.9))














