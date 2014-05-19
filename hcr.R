# validity
#	type must match fwd "quantity"

setClass("a4aHCRvar",
        representation(
				quantity = "character", # change name to "quantity"
                model = "formula", 
				params = "FLPar",
				refYr = "numeric",
				range = "numeric"),
        prototype = prototype(
				quantity = vector(mode="character"),
                model = ~1,
				params = new("FLPar"),
				refYr = -1,
				range = NaN)
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
setGeneric("quantity", function(object, ...) standardGeneric("quantity"))
setMethod("quantity", "a4aHCRvar", function(object) object@quantity)
setGeneric("refYr", function(object, ...) standardGeneric("refYr"))
setMethod("refYr", "a4aHCRvar", function(object) object@refYr)
setMethod("range", "a4aHCRvar", function(x) x@range)

setMethod("predict", signature(object="a4aHCRvar"),	function (object, ...){
    args <- list(...)
    pr <- params(object)
	mod <- model(object)
	ryr <- refYr(object) 
	cvar <- match(all.vars(mod), names(args), nomatch=0)
	if(sum(cvar)>0){
		cvar <- names(args)[cvar]
		lst <- lapply(split(cvar, cvar), function(x){
			if(is(args[[x]], "FLQuant")){
				args[[x]][,getYidx(args[[x]], ryr)]
			} else {
				args[[x]]
			}
		})
		args[cvar] <- lapply(lst, c)
	} 

    res <- apply(pr, 2, function(x) {
        lst <- as.list(x)
        eval(as.list(mod)[[2]], envir = c(args, lst))
    })
    res <- matrix(res, ncol = dim(pr)[2])
    dimnames(res) <- list(pred = 1:nrow(res), iter = 1:dim(pr)[2])
    return(res)
})

# examples
o1 <- a4aHCRvar(quantity="f", model=~Ftrg, params=FLPar(Ftrg=0.35))
predict(o1)
# it's all the same
data(ple4)
o1 <- a4aHCRvar(quantity="catch", model=~mean(catch), refYr=2005:2007)
predict(o1, catch=catch(ple4))
o1 <- a4aHCRvar(quantity="catch", model=~mean(catch), refYr=-2:-4)
predict(o1, catch=catch(ple4))
o1 <- a4aHCRvar(quantity="catch", model=~mean(catch), refYr=49:51)
predict(o1, catch=catch(ple4))

# using ssb to get catch
o1 <- a4aHCRvar(quantity="catch", model=~f*SSB, params=FLPar(f=0.35), refYr=-1)
predict(o1, SSB=ssb(ple4))

# a more complex one
o1 <- a4aHCRvar(quantity="catch", model=~sum(f/(m+f)*(1-exp(-(m+f)))*N*w), params=FLPar(f=0.35), refYr=-1)
predict(o1, N=stock.n(ple4), m=m(ple4), w=stock.wt(ple4))

# in this case we wouldn't need limits, they're defined by the range
f0 <- a4aHCRvar(quantity="catch", model=~0, refYr=-1, range=c(0,10))
f0.35 <- a4aHCRvar(quantity="catch", model=~f*SSB, params=FLPar(f=0.35), refYr=-1, range=c(10,20))
fmax <- a4aHCRvar(quantity="catch", model=~10, refYr=-1, range=c(20,Inf))

# constraints
cnst <- a4aHCRvar(quantity="catch", model=~if(C >= mult * c) mult * c else C , params=FLPar(mult=1.1), refYr=-1, range=c(20,Inf))

predict(cnst, C=predict(f0.35, SSB=ssb(ple4)), c=catch(ple4))














# validity
#	iter = 1 or N

setClass("a4aHCR",
        representation(
				objective = "a4aHCRvar",
				constraints = "a4aHCRvar",
				limit = "a4aHCRvar",
				precedence = "numeric",
				range = "numeric"),
        prototype = prototype(
				objective = a4aHCRvar(),
				constraints = a4aHCRvar(),
				limit = a4aHCRvar(),
				precedence = NA,
				range = NA)
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
setGeneric("objective", function(object, ...) standardGeneric("objective"))
setMethod("objective", "a4aHCR", function(object) object@objective)
setGeneric("constraints", function(object, ...) standardGeneric("constraints"))
setMethod("constraints", "a4aHCR", function(object) object@constraints)
setGeneric("softLimit", function(object, ...) standardGeneric("softLimit"))
setMethod("softLimit", "a4aHCR", function(object) object@softLimit)
setGeneric("hardLimit", function(object, ...) standardGeneric("hardLimit"))
setMethod("hardLimit", "a4aHCR", function(object) object@hardLimit)

# method to create fwdControl object
setMethod("fwdControl", signature(object="a4aHCR"),
  function(object, yrs, ...) {
	args <- list(...)
	args$yrs <- yrs
	obj <- objective(object)
	# iters ??
	trg <- data.frame(quantity=type(obj), val=c(predict(obj)), year=args$yrs, timeOrder=timeOrder(obj))
	trg <- fwdControl(trg)
	trg
  }
)

# examples

o1 <- a4aHCRvar(type="f", model=~0.35)

hcr1 <- a4aHCR(objective=o1)
fwdControl(hcr1, yrs=2000:2001)

# mean catch from the last 3 years

o1 <- a4aHCRvar(model=~mean(c), refYr=-1:-3, FUN="harvest")
hcr1 <- a4aHCR(objective=o1)
fwdControl(hcr1, yrs=2000:2001)


predict(o1, c=catch(ple4))





