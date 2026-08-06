# MODEL 1:  WITHIN-PAIR CORRELATION #


# read in the data
twins<-read.csv(".../mgram.csv",header=T, stringsAsFactors=F)
names(twins)

# write the model
cat( "model
      {
      for (i in 1:951)
      {
      pdens1[i] ~ dnorm(a[i],tau.e)
      pdens2[i] ~ dnorm(a[i],tau.e)
      a[i] ~ dnorm(mu,tau.a)
      }

      tau.a <- pow(sigma.a,-2)
      sigma.a ~ dunif(0,1000)

      tau.e <- pow(sigma.e,-2)
      sigma.e ~ dunif(0,1000)

      mu ~ dnorm(0,1.0E-6)

      sigma2.a <- pow(sigma.a,2)
      sigma2.e <- pow(sigma.e,2)

      }",
file = "twinMammo1.txt" )

# set up data, inits, parameters to monitor
twins.dat<-as.list(twins)
twins.inits<-list(mu = 37, sigma.a = 16, sigma.e = 13.5)
twins.pars<-c("mu","sigma2.a","sigma2.e","sigma.a","sigma.e")

# run jags model
library(R2jags)

set.seed(22) # need to set seed 
twins.jags<-jags(data=twins.dat, 
inits=twins.inits, 
parameters.to.save=twins.pars, n.chains=3, n.iter=1500,
model.file="twinMammo1.txt")


# results
twins.jags
plot(twins.jags)

twins.jags.mcmc<-as.mcmc(twins.jags)
plot(twins.jags.mcmc)

# calculate the correlation between the twin pairs

# extract mean, and standard deviations from bugs output
twins.jags$BUGSoutput$mean$mu
twins.jags$BUGSoutput$mean$sigma.a
twins.jags$BUGSoutput$mean$sigma.e

# standardize the values in the data set using above data
a<-(twins$pdens1-twins.jags$BUGSoutput$mean$mu)/(twins.jags$BUGSoutput$mean$sigma.a+twins.jags$BUGSoutput$mean$sigma.e)
b<-(twins$pdens2-twins.jags$BUGSoutput$mean$mu)/(twins.jags$BUGSoutput$mean$sigma.a+twins.jags$BUGSoutput$mean$sigma.e)

# calculate the correlation and average of product of standardized values
twin.corr<-mean(a*b)
twin.corr

# MODEL 2: REPEAT ANALYSES TO ACCOUNT FOR INFLUENCE OF MZ VS DZ ON CORRELATIONS

cat(" model
      {
      for (i in 1:951)
      {
      pdens1[i] ~ dnorm(mean.pdens1[i],tau.e)
      pdens2[i] ~ dnorm(mean.pdens2[i],tau.e)
      mean.pdens1[i] <- b.int + sqrt(rho)*a1[i] + sqrt(1-rho)*a2[i]
      mean.pdens2[i] <- b.int + sqrt(rho)*a1[i] + mz[i]*sqrt(1-rho)*a2[i] + dz[i]*sqrt(1-rho)*a3[i]
      a1[i] ~ dnorm(0,tau.a)
      a2[i] ~ dnorm(0,tau.a)
      a3[i] ~ dnorm(0,tau.a)
      }

      rho ~ dunif(0,1)

      b.int ~ dnorm(0,0.0001)

      tau.a <- pow(sigma.a,-2)
      sigma.a ~ dunif(0,1000)

      tau.e <- pow(sigma.e,-2)
      sigma.e ~ dunif(0,1000)

      sigma2.a <- pow(sigma.a,2)
      sigma2.e <- pow(sigma.e,2)

      }",
file = "twinMammo2.txt" )


# set up inits, parameters to monitor (data same as above)
twins.inits2<-list(rho = 0.5, b.int = 37, sigma.a = 16, sigma.e = 13.5)
twins.pars2<-c("b.int","rho","sigma2.a","sigma2.e","sigma.a","sigma.e")

twins.jags2<-jags(data=twins.dat, 
inits=twins.inits2, 
parameters.to.save=twins.pars2, n.chains=4, n.iter=1500,
model.file="twinMammo2.txt")


# results
twins.jags2
plot(twins.jags2)

twins.jags.mcmc2<-as.mcmc(twins.jags2)
plot(twins.jags.mcmc2)


# compare results mean, sigmas to previous run

(a<-twins.jags$BUGSoutput$mean$mu)
(b<-twins.jags$BUGSoutput$mean$sigma.a)
(c<-twins.jags$BUGSoutput$mean$sigma.e)
(d<-twins.jags2$BUGSoutput$mean$b.int)
(e<-twins.jags2$BUGSoutput$mean$sigma.a)
(f<-twins.jags2$BUGSoutput$mean$sigma.e)

mm<-matrix(c(a,b,c,d,e,f),2,3, byrow=T)
rownames(mm)<-c("Model 1", "Model 2")
colnames(mm)<-c("mu", "sigma.a", "sigma.e")
mm

# look at rho
# value of rho=0.60 not consistent with a purely genetic model, but close, and CrI of 
# 0.50 to 0.68 does include the critical value of .5

