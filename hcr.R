# validity
#	type must match fwd "quantity"

setClass("a4aHCRvar",
        representation(
				type = "character",
				timeOrder = "numeric",
                model = "formula", 
				# if no model, params used in fwdControl and refYr used to set rel.year !?
				params = "FLPar",
				refYr = "numeric"),
        prototype = prototype(
				type = vector(mode="character"),
				timeOrder = 1,
#                model = ~a,
#				params = new("FLPar", array(1, dim=c(1,1), dimnames=list(params="a", iter="1"))),
                model = ~1,
				params = new("FLPar"),
				refYr = vector(mode="numeric"))
)

setGeneric("a4aHCRvar", function(object, ...) standardGeneric("a4aHCRvar"))

setMethod("a4aHCRvar", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("a4aHCRvar")
    # or not
  	} else {
      args <- list(...)
	  args$Class <- 'a4aHCRvar'
      do.call("new", args)
	  }
  }
)

setMethod("params", "a4aHCRvar", function(object) object@params)
setMethod("model", "a4aHCRvar", function(object) object@model)
setMethod("type", "a4aHCRvar", function(object) object@type)
setGeneric("refYr", function(object, ...) standardGeneric("refYr"))
setMethod("refYr", "a4aHCRvar", function(object) object@refYr)
setGeneric("timeOrder", function(object, ...) standardGeneric("timeOrder"))
setMethod("timeOrder", "a4aHCRvar", function(object) object@timeOrder)

setMethod("predict", signature(object="a4aHCRvar"),	function (object, ...){
    args <- list(...)
    pr <- params(object)
	mod <- model(object)
	ryr <- refYr(object) 
	cvar <- match(all.vars(mod), names(args), nomatch=0)
	if(sum(cvar)>0){
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
o1 <- a4aHCRvar(type="f", timeOrder=1, model=~Ftrg, params=FLPar(Ftrg=0.35))
predict(o1)
# it's all the same
data(ple4)
o1 <- a4aHCRvar(type="c", model=~mean(c), refYr=2005:2007)
predict(o1, c=catch(ple4))
o1 <- a4aHCRvar(type="c", model=~mean(c), refYr=-1:-3)
predict(o1, c=catch(ple4))
o1 <- a4aHCRvar(type="c", model=~mean(c), refYr=49:51)
predict(o1, c=catch(ple4))


# validity
#	iter = 1 or N

setClass("a4aHCR",
        representation(
				objective = "a4aRule",
				constraints = "a4aRule",
				softLimit = "a4aRule",
				hardLimit = "a4aRule"),
        prototype = prototype(
				objective = a4aRule(),
				constraints = a4aRule(),
				softLimit = a4aRule(),
				hardLimit = a4aRule())
)

setGeneric("a4aHCR", function(object, ...) standardGeneric("a4aHCR"))

setMethod("a4aHCR", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("a4aHCR")
    # or not
  	} else {
      args <- list(...)
	  args$Class <- 'a4aHCR'
      do.call("new", args)
	  }
  }
)

# accessors
genAR(a4aHCR())


setMethod("fwdControl", signature(object="a4aHCR"),
  function(object, yrs, ...) {
	args <- list(...)
	args$yrs <- yrs
	obj <- objective(object)
	# iters ??
	trg <- data.frame(quantity=obsVar(obj), val=c(predict(obj)), year=args$yrs, timeOrder=timeOrder(obj))
	trg <- fwdControl(trg)
	trg
  }
)

# examples

o1 <- a4aHCRvar(type="f", model=~0.35)

hcr1 <- a4aHCR(objective=a4aRule(obsVar="f", refVal=FLQuant(0.35)))
fwdControl(hcr1, yrs=2000:2001)

# mean catch from the last 3 years

o1 <- a4aHCRvar(type="c", model=~mean(c), refYr=0:-2)
hcr1 <- a4aHCR(objective=o1)
fwdControl(hcr1, yrs=2000:2001)


predict(o1, c=catch(ple4))





