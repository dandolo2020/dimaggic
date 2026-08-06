
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
	}", file="betaMeta2.txt")

blockerDat<-list(rB = c(3,  7,  5,  102,  28, 4,  98,  60, 25, 138, 64, 45,  9, 57, 25, 33, 28, 8, 6, 32, 27, 22 ),
nB = c(38, 114, 69, 1533, 355, 59, 945, 632, 278,1916, 873, 263, 291, 858, 154, 207, 251, 151, 174, 209, 391, 680),
 rA = c(3, 14, 11, 127, 27, 6, 152, 48, 37, 188, 52, 47, 16, 45, 31, 38, 12, 6, 3, 40, 43, 39),
 nA = c(39, 116, 93, 1520, 365, 52, 939, 471, 282, 1921, 583, 266, 293, 883, 147, 213, 122, 154, 134, 218, 364, 674),
Nstud = 22)

blocker2Inits<-function(){list("d" = 0,"tau"=1, 
"mu" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
"delta" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))}

blocker2Params <- c("d","tau","OR")

library(R2jags)

jag.blocker2<-jags(data=blockerDat, 
inits=blocker2Inits, 
parameters.to.save=blocker2Params, n.iter=1000, n.thin=20,
model.file="betaMeta2.txt")

jag.blocker2




