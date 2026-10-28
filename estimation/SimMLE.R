options(warn=-1) #suppress the warnings for numerical optimization

#######################################
### Generate the underlying Process ###
#######################################
gen<-function(para,n=1000){

  omega=para[1]; b=para[2]; c=para[3]; d=para[4]
  beta0=para[5]; beta1=para[6]; beta2=para[7]; beta3=para[8];
  mu=para[9]

  burnin=10000
  logAlpha <- logSigma <- rep(0,(n+burnin))
  logAlpha[1]=(omega+c/2)/(1-b)
  logSigma[1]=(beta0+beta2/2)/(1-beta1)
  #Bound is for bounding the upper bound of R
  
  
  # What do these lines do?
  R=rep(0,(n+burnin))
  for(i in 1:(n+burnin)){
    R[i]=mu+exp(logSigma[i])*(log(1/runif(1,0,1)))^(-1/exp(logAlpha[i])) 
    
    if(i<(n+burnin)){
      logAlpha[i+1]=omega+b*logAlpha[i]+c*exp(-d*R[i]) 
      logSigma[i+1]=beta0+beta1*logSigma[i]+beta2*exp(-beta3*R[i])
    }
  }
  return(data.frame(R[(burnin+1):(n+burnin)],exp(logAlpha[(burnin+1):(n+burnin)]),exp(logSigma[(burnin+1):(n+burnin)])))
}

####################################################################################
### Function to recover the latent {alpha_t} series from para, alpha0 and data R ###
####################################################################################
rec_alphaLog<-function(para,R,R2){
  gamma0=para[1];gamma1=para[2];gamma2=para[3];gamma3=para[4];gamma4=para[5];gamma5=para[6]
  n=length(R)
  
  logAlpha=rep(0,n)
  logAlpha[1]=(gamma0+gamma2/2)/(1-gamma1)
  
  #recover the latent alpha series
  for(i in 2:n){
    # Modified on 9.19.2024 for a run. Simply uncomment the line below, and comment two lines below to revert back to the old version. 
    # logAlpha[i]=gamma0+gamma1*logAlpha[i-1]+gamma2/log(gamma3*R[i-1])+gamma4/log(gamma5*R2[i-1]) # Old version, contains log(A*B) terms
    logAlpha[i]=gamma0+gamma1*logAlpha[i-1]+gamma2/(log(gamma3)+log(R[i-1]))+gamma4/(log(gamma5)+log(R2[i-1])) # New version, contains log(A)+log(B) terms
  }
  return(exp(logAlpha))
}


####################################################################################
### Function to recover the latent {sigma_t} serie from para, alpha0 and data R ###
####################################################################################
rec_sigma<-function(para,R,R2){
  
  beta0=para[7];beta1=para[8];beta2=para[9];beta3=para[10];beta4=para[11];beta5=para[12]
  n=length(R)
  
  logSig=rep(0,n)
  logSig[1]=(beta0+beta2/2)/(1-beta1)
  
  #recover the latent alpha series
  for(i in 2:n){
    logSig[i]=beta0+beta1*logSig[i-1]+beta2/log(beta3*R[i-1])+beta4/log(beta5*R2[i-1])
  }
  return(exp(logSig))
}

#####################################
### Calculate the Loglik fucntion ###
#####################################
fn_loglik <- function(theta, R, R2, ub, lb, burnin=10){
    
  n=length(R)
  est_alpha=rep(0,n) #To store estimated alpha_t  
  est_sigma=rep(0,n) #To store estimated sigma_t  
  
  #Recover latent alpha and sigma
  est_alpha <- rec_alphaLog(theta, R, R2)
  est_sigma <- rec_sigma(theta, R, R2)
  
  ###########################################
  #penalize the theta not in parameter space#
  ###########################################
  penal=10^10;penalty=penal*(sum(theta<lb)+sum(theta>ub)) 
  
  #Truncate the first burnin of the data for better estimation of alpha_t
  R1=(R-theta[13])/est_sigma
  est_alpha=est_alpha[burnin:n]
  est_sigma=est_sigma[burnin:n]
  R1=R1[burnin:n]
  
  #Loglikelihood for GEV model
  loglik=-1/n*sum(log(est_alpha)-log(est_sigma)-(est_alpha+1)*log(R1)-(R1)^(-est_alpha))
  # Does this match the whiteboard
  #return(loglik)
  return(loglik+penalty)      
}
    
#####################################################
### Functions to determine initial value of sigma ###
#####################################################
fn_iidMLE <- function(theta0,R) {
  n=length(R)
  
  #theta0[1]=sigma; theta0[2]=alpha
  R1=(R)/theta0[1]

  ############################################
  #penalize the theta not in parameter space
  penal=10^10
  lb=c(0.01,0.01)
  ub=c(10,10)
  penalty=penal*(sum(theta0<lb)+sum(theta0>ub))
  -sum(log(theta0[2])-(R1)^(-theta0[2])-(theta0[2]+1)*log(R1)-log(theta0[1]))+penalty
}

initial_est <- function(R){
    initial0=c(5,5) #initial estimation of (sigma, alpha)
    fit0=optim(initial0,fn_iidMLE,R=R)
    return(fit0$par)
}

