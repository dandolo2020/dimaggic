
gambiaMissing<-read.csv("/Users/charlie/Dropbox/Bayes/bayesWeb/bayesWebPt5/gambiaMissing.csv",header=T, stringsAsFactors=F)
table(gambiaMissing$BEDNET)
sum(is.na(gambiaMissing$BEDNET))/nrow(gambiaMissing)*100
names(gambiaMissing)


cat("model {
   for(i in 1:805) {
      Y[i] ~ dbern(p[i])
      logit(p[i]) <- alpha + beta.age*(AGE[i] - mean(AGE[])) + beta.bednet*BEDNET[i] +
                                    beta.green*(GREEN[i] - mean(GREEN[])) + beta.phc*PHC[i]                                    
   }

  # model for missing exposure variable
  for(i in 1:805) {  
      BEDNET[i] ~ dbern(q[i]) 
      logit(q[i]) <- gamma[1] + gamma[2]**PHC[i]    
                                                                        
   }
   for(k in 1:4) { gamma[k] ~ dnorm(0,0.00000001) } 

    # vague priors on regression coefficients
   alpha ~ dnorm(0,0.00000001)
   beta.age ~ dnorm(0,0.00000001)    
   beta.bednet ~ dnorm(0,0.00000001)     
   beta.green ~ dnorm(0,0.00000001)     
   beta.phc ~ dnorm(0,0.00000001)     
 
   # calculate odds ratios of interest
   OR.age <- exp(beta.age)	# odds ratio of malaria per year      
   OR.bednet <- exp(beta.bednet)  # odds ratio of malaria for children using bednets vs children 
				  # not using bednets
   OR.green <- exp(beta.green)	 # odds ratio of malaria per unit increase in greenness index of village
   OR.phc <- exp(beta.phc)		 # odds ratio of malaria for children living in villages belonging to the 
                                 # primary health care system versus children living in villages not in
                                 # the health care system

   logit(baseline.prev) <- alpha   # baseline.prev = prevalence of malaria in baseline group (i.e. child
                                   # in age group 1 (<2yrs), sleeps without bednet, and lives in a
                                   # village with average greenness index and not in the health care system) 

   PP.bednet <- step(0.8 - OR.bednet) # probability that using a bed net reduces risk of malaria by at least 20%
}", file="malaria1.txt")

dat<-list(Y=gambiaMissing$Y, AGE=gambiaMissing$AGE, BEDNET=gambiaMissing$BEDNET, GREEN=gambiaMissing$GREEN,  PHC=gambiaMissing$PHC )

inits<-list(
	list(alpha = -0.51, beta.age = 1, beta.bednet = -2.41, beta.green = -0.23, beta.phc = 1.82, 
			gamma = c(0,1,-1,-1)),
	list(alpha = 1.01, beta.age = -1, beta.bednet = 0.21, beta.green = 1.81, beta.phc = -0.23, 
			gamma = c(1,-1,1,1))
	)

params<-c("alpha", "beta.age", "beta.bednet", "beta.green", "beta.phc", "OR.bednet", "PP.bednet")

library(R2jags)

malaria.sim1<-jags(data = dat,
        parameters.to.save = params,
        model.file = "malaria1.txt",
        n.chains = 2,
        n.iter = 2000,
        n.burnin = 500,
        n.thin = 1)

print(malaria.sim1)


# notice slightly increased risk of malaria with bednet use...



#################################################################
# More complex modeling of probability of missing covariate data#
#################################################################


cat("model {
   for(i in 1:805) {
      Y[i] ~ dbern(p[i])
      logit(p[i]) <- alpha + beta.age*(AGE[i] - mean(AGE[])) + beta.bednet*BEDNET[i] +
                                    beta.green*(GREEN[i] - mean(GREEN[])) + beta.phc*PHC[i]                                    
   }

  # model for missing covariate
  for(i in 1:805) {  
      BEDNET[i] ~ dbern(q[i])   # prior model for whether or not child i sleeps under a bednet
      logit(q[i]) <- gamma[1] + gamma[2]*(AGE[i] - mean(AGE[])) + gamma[3]*(GREEN[i] - mean(GREEN[])) + gamma[4]*PHC[i]    
                                                                        
   }
   for(k in 1:4) { gamma[k] ~ dnorm(0,0.00000001) } 

    # vague priors on regression coefficients
   alpha ~ dnorm(0,0.00000001)
   beta.age ~ dnorm(0,0.00000001)    
   beta.bednet ~ dnorm(0,0.00000001)     
   beta.green ~ dnorm(0,0.00000001)     
   beta.phc ~ dnorm(0,0.00000001)     
 
   # calculate odds ratios of interest
   OR.age <- exp(beta.age)	# odds ratio of malariaper year      
   OR.bednet <- exp(beta.bednet)  # odds ratio of malaria for children using bednets vs children 
				  # not using bednets
   OR.green <- exp(beta.green)	 # odds ratio of malaria per unit increase in greenness index of village
   OR.phc <- exp(beta.phc)		  # odds ratio of malaria for children living in villages belonging to the 
                                                 # primary health care system versus children living in villages not in
                                                 # the health care system

   logit(baseline.prev) <- alpha          # baseline.prev = prevalence of malaria in baseline group (i.e. child
                                                     # in age group 1 (<2yrs), sleeps without bednet, and lives in a
                                                     # village with average greenness index and not in the health care system) 

   PP.bednet <- step(0.8 - OR.bednet) # probability that using a bed net reduces risk of malaria by at least 20%
}", file="malaria1.txt")



#################################################################
# simple univariable complete case analysis                    #
#################################################################


cat("model {
	         for(i in 1:488) {
	   Y[i] ~ dbern(p[i])
       logit(p[i]) <- alpha + beta.bednet*BEDNET[i]                                    
}

      # vague priors on regression coefficients
	   alpha ~ dnorm(0,0.00000001)
	   beta.bednet ~ dnorm(0,0.00000001)     
      # calculate odds ratios of interest
       OR.bednet <- exp(beta.bednet)  # odds ratio of malaria for children using bednets vs children 
				  # not using bednets
       PP.bednet <- step(0.8 - OR.bednet) # probability that using a bed net reduces risk of malaria by at least 20%
}", file="malaria0.txt")

gambiaComplete<-gambiaMissing[!is.na(gambiaMissing$BEDNET),]

dat<-list(Y=gambiaComplete$Y, BEDNET=gambiaComplete$BEDNET)

inits<-list(
	list(alpha = -0.51, beta.bednet = -2.41),
	list(alpha = 1.01, beta.bednet = 0.21)
	)

params<-c("alpha", "beta.bednet","OR.bednet", "PP.bednet")

library(R2jags)

malaria.sim0<-jags(data = dat,
        parameters.to.save = params,
        model.file = "malaria0.txt",
        n.chains = 2,
        n.iter = 2000,
        n.burnin = 500,
        n.thin = 1)

print(malaria.sim0)

nrow(gambiaComplete)


