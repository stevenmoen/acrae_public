

# This is what the paraFind_main.R file is referencing

# This looks like a parallelization routine
finale=foreach(k = 1:10, .combine=rbind) %dopar% {
    # Selects a burn-in period
    burnin=100
    # Generates a random variable between 0 and 1
    loc <- runif(1,0,1)
    # Generates a convex combination between the lower and upper bound
    # to start the search
    start <- loc*lb + (1-loc)*ub
    # Uses likelihood from SimMLE.R
    fitTMP=nlminb(start,fn_loglik,lower=lb,upper=ub,
                  # To shorten the estimation procedure, I'm lowering these on 2/21 from 1e7 to 1e5.
                  control=list(eval.max=1e5, iter.max=1e5), # New
                  # control=list(eval.max=1e7, iter.max=1e7), # Old
                  R=R,R2 = R2, ub=ub,lb=lb,burnin=burnin)
    result=fitTMP$par
    #print(result)
    c(result,fitTMP$objective)
}
  index =  which.min(finale[,14])
  result = finale[index,]


