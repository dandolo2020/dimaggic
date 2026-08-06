

cat("model{for(i in 1:M){                          # select ith child

 for(j in offset[i]:(offset[i+1]-1)) {  # loop over observations for child

    y[j] ~ dnorm(mu[j],tau);   # specify distribution (=Normal) from
                               # exponential family for responses

   mu[j] <- alpha[i]+beta*(log.time[j]-tbar)
            + gamma*(y0[i]-y0bar);        # specify link function
                                          # (=identity) & linear predictor

   sresid[j] <- (y[j] - mu[j])/sigma;

 }

 alpha[i] ~ dnorm(mu.alpha, tau.alpha);  # random intercept for each child
                                         # - assume alpha[i]'s are exchangable
                                         # in their joint (population) distn.

}

  tbar <- mean(log.time[]);   # for centering covariates about their means
  y0bar <- mean(y0[]);

# priors

  mu.alpha ~ dnorm(0,0.0001);       # } Vague (non-informative)
  tau.alpha ~ dgamma(0.001,0.001);  # } but proper (i.e. integrate
  beta ~ dnorm(0,0.0001);           # } to 1) priors for population
  gamma ~ dnorm(0,0.0001);          # } `hyper-parameters', fixed effects
  tau ~ dgamma(0.001,0.001);        # } and residual precision
  sigma <- 1/sqrt(tau);
  sigma.alpha <- 1/sqrt(tau.alpha);

}", file="hepB.txt")

params<-c("beta", "sigma")

library(R2jags)

hepBmod<-jags(data = dat,
        parameters.to.save = params,
        model.file = "hepB.txt",
        n.chains = 2,
        n.iter = 5000,
        n.burnin = 2000,
        n.thin = 1)

hepBmod
plot(hepBmod)