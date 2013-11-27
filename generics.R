#' Generates accessores and replacements for classes
#'
#' @author EJ \email{ernesto.jardim@@jrc.ec.europa.eu}
#' @param object a class object
#' @return generates the accessors and replacement methods
#' @export

setGeneric("genAR", function(object, ...) standardGeneric("genAR"))
setMethod("genAR", "ANY", function(object, ...) {
	slts <- slotNames(object)

	for(i in slts){
		if(!isGeneric(i)){
			do.call("setGeneric", list(name=i, def=eval(parse(text=paste("function(object, ...) standardGeneric(\"", i, "\")", sep="")))))
		}
		do.call("setMethod", list(f=i, signature=is(object)[1], definition=eval(parse(text=paste("function(object) object@", i, sep="")))))
		
		if(!isGeneric(paste(i, "<-", sep=""))){
			do.call("setGeneric", list(name=paste(i, "<-", sep=""), def=eval(parse(text=paste("function(object, value) standardGeneric(\"", paste(i, "<-", sep=""), "\")", sep="")))))
		}

		do.call("setReplaceMethod", list(f=i, signature=is(object)[1], def=eval(parse(text=paste("function(object, value){if(all.equal(is(value), is(object@", i, "))) object@", i, "<- value; object}", sep="")))))
	}

})

