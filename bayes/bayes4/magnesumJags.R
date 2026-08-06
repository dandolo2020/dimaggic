
cat("model
	{
	   for( i in 1 : Nstud ) {
	   	rA[i] ~ dbin(pA[i], nA[i])
		rB[i] ~ dbin(pB[i], nB[i])
		logit(pA[i]) <- mu[i]
		logit(pB[i]) <- mu[i] + delta[i]
		mu[i] ~ dnorm(0.0,1.0E-5)
		delta[i] ~ dnorm(d, prec)
	   }
	   OR <- exp(d)
	   d ~ dnorm(0.0,1.0E-6)
	tau~dunif(0,10)
	tau.sq<-tau*tau
	prec<-1/(tau.sq)
	}", file="magMeta.txt")

magDat<-list(rB = c(1, 9, 2, 1, 10, 1, 1, 90),
nB = c(40, 135, 200, 48, 150, 59, 25, 1159),
 rA = c(2, 23, 7, 1, 8, 9, 3, 118),
 nA = c(36, 135, 200, 46, 148, 56, 23, 1157),
Nstud = 8)


magInits<-function(){list(d = 0,tau=1, 
mu = c(0, 0, 0, 0, 0, 0, 0, 0),
delta = c(0, 0, 0, 0, 0, 0, 0, 0))}


magParams <- c("d","tau","OR")

library(R2jags)

jag.mag<-jags(data=magDat, 
inits=magInits, 
parameters.to.save=magParams, n.iter=1000, n.thin=20,
model.file="magMeta.txt")

jag.mag






