
# red fish
# M=0.05; Linf=58.5, k=0.086

cth <- stock.n(rfLen.stk)[,1:2,,1,]
dnms <- dimnames(cth)
invvb <- FLModelSim(model=~t0-1/k*log(1-len/linf), params=FLPar(linf=40, k=0.086, t0=0, units=c("cm","ano-1","ano")), vcov=mm, distr="norm")
len <- as.numeric(dimnames(cth)[[1]])
age <- floor(predict(invvb, len=as.numeric(dimnames(cth)[[1]])))
age[which.max(age[!is.infinite(age)]):length(age)] <- max(age[!is.infinite(age)], na.rm=T)
dnms[[1]] <- as.character(c(unique(age)))
cthA <- FLQuant(NA, dimnames=dnms, quant="age")
lst <- split(len, age)
for(i in dnms[[1]]) cthA[as.character(i)] <- quantSums(cth[as.character(lst[[as.character(i)]])])


mm <- matrix(NA, ncol=3, nrow=3)
diag(mm) <- c(100, 0.001,0.001)
mm[upper.tri(mm)] <- mm[lower.tri(mm)] <- c(0.1,0.1,0.0003)

vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), grInvMod=~t0-1/k*log(1-len/linf), params=FLPar(linf=40, k=0.086, t0=0.001, units=c("cm","ano-1","ano")), vcov=mm, distr="norm")

vbObj <- mvrlnorm(100, vbObj)




setMethod("mvrlnorm", signature("missing", "a4aGr"), function(n=missing, object, which.model="grInvMod", ...) {

	args <- list(...)	
	args$object <- FLModelSim(model=grInvMod(object), params=params(object), vcov=vcov(object), distr=distr(object))
	if(model=="grMod") model(args$object) <- grMod(object)
	args$n <- n
	res <- do.call("mvrlnorm", args)	
	params(object) <- params(res)
	object	
	
})



