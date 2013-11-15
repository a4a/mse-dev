# example

mm <- matrix(NA, ncol=3, nrow=3)
diag(mm) <- c(100, 0.001,0.001)
mm[upper.tri(mm)] <- mm[lower.tri(mm)] <- c(0.1,0.1,0.0003)

vb <- FLModelSim(model=~linf*(1-exp(-k*(t-t0))), params=FLPar(linf=120, k=0.3, t0=0.1, units=c("cm","ano-1","ano")), vcov=mm, distr="norm")

mmm <- predict(mvrlnorm(100, vb), t=0:20)
boxplot(as.data.frame(t(mmm)))

invvb <- FLModelSim(model=~t0-1/k*log(1-len/linf), params=FLPar(linf=120, k=0.3, t0=0.1, units=c("cm","ano-1","ano")), vcov=mm, distr="norm")
mmm <- predict(mvrlnorm(100, invvb), len=1:160)
boxplot(as.data.frame(t(mmm)))


