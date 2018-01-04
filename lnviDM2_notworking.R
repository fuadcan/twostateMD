lnviD2 <- function(b,w){
  
  # b      <- c(.4,   1.1,    .8,    .85,.4,.1,.2,.3)
  # b <- inits
  # w <- rnorm(1000)
  size <- length(w)
  
  d1hat  = b[1] ; d2hat  = b[2]
  p11hat = b[3] ; p22hat = b[4]
  sigma1hat <- sigma2hat <- sigma3hat <-  b[5];
  mu1hat <- b[6]; mu2hat <- b[7]; mu3hat <- b[8]
  
  rho1hat=0; theta1hat=0;
  tranmax <- matrix(,2,2)
  diag(tranmax) <- b[3:4]
  tranmax[c(3,2)] <- apply(tranmax, 1, function(x) 1-sum(x,na.rm=T))
  
  mu     <- c(mu1hat,mu2hat)
  sigma  <- c(sigma1hat,sigma2hat)
  coefd1 = matrix(cumprod((d1hat+(0:(size-2)))/(1:(size-1))),,1)
  coefd2 = matrix(cumprod((d2hat+(0:(size-2)))/(1:(size-1))),,1)
  coeff  = cbind(coefd1,coefd2)
  
  # muhat is to store the selected path of state: from date 2 */
  eta1  = matrix(0,4,size); # row 1:2 for path1, 3:4 for path2 */
  eta   = matrix(0,4,size); # same as above */
  xihat = matrix(0,4,size); # same as above */
  ehat  = matrix(0,2,size); # row 1, for path1, row 2 for path2 */
  
  # to keep the long memory part, not iid */
  zsigmahat = matrix(0,2,size) # row 1, for path1, row 2 for path2 */
  zhat      = matrix(0,2,size) # row 1, for path1, row 2 for path2 */
  lnlk <- matrix(0,2,size)
  
  path1 = matrix(0,2,size)
  path2 = matrix(0,2,size)
  path1[,1] <- c(1,0)
  path2[,1] <- c(0,1)

  zsigmahat[,1] <- sapply(mu, function(m) w[1] - m) 
  # zsigmahat is de-drifted series for each given path; or it is z*sigmahat
  
  zhat[,1] <- zsigmahat[,1] / sigma 
  
  ehat[,1] <- zhat[,1]
  
  # limprobs <- (tranmax %*% tranmax %*% tranmax %*% tranmax %*% tranmax %*% tranmax )[1,]
  limprobs <- tranmax[2,1]/(1+tranmax[2,1]-tranmax[1,1])
  limprobs <- c(limprobs, 1-limprobs)
  lnlk[,1] <- log(exp(-ehat[,1]^2/2 + 1e-50 ) / (sqrt(2*pi))*limprobs)
  
  j = 2
  
  
  ####################!!!!!!!!!!!!!!!!!!!!!
  while(j<=size){
    # tempsMUZ and temps are needed for likelihood possibility
    
    tempsMUZ <- c(t(matrix(c(rev(zsigmahat[1,1:(j-1)]),rev(zsigmahat[2,1:(j-1)])),,2))%*%coeff[1:(j-1),])
    
    temps    <- c(matrix(rep(mu,2),,2,T)) + rho1hat*c(matrix(rep(sigma,2),,2,T))*rep(zhat[,j-1],2)+
      theta1hat*c(matrix(rep(sigma,2),,2,T))*rep(ehat[,j-1],2)
    
    # likelihood possibility
    eta1[,j]  <- exp(-(w[j] - temps - tempsMUZ)^2 / (2*c(matrix(rep(sigma,2),,2,T))^2+1e-50))/
      (sqrt(2*pi*c(matrix(rep(sigma,2),,2,T))^2))
    
    eta[,j] <- eta1[,j] * c(tranmax)
    
    # Case is 1 for 1->1; 2 for 2->1; 3 for 1->2; 4 for 2->2
    case  <- matrix(sapply(1:4, function(x) log(eta[x,j]+1e-50) + lnlk[(x-1) %% 2 + 1,j-1] ),,2)
    case  <- which(apply(case, 2, function(s) max(s) == s))
    case2 <- (case-1) %% 2 + 1
    
    # holding previous paths in temp memory
    oldlnlk <- lnlk
    oldPath <- list(path1,path2)
    oldzsigmahat <- zsigmahat
    oldzhat <- zhat
    oldehat <- ehat
    
    # recalculating values
    # path ends up with state 1:
    lnlk[1,j]   <- log(eta[case[1],j]+1e-50) + oldlnlk[case2[1],j-1]
    path1[,1:j] <- cbind(oldPath[[case2[1]]][,1:(j-1)],c(1,0))
    zsigmahat[1,1:j] <- c(oldzsigmahat[case2[1],1:(j-1)], w[j] - mu[1] - tempsMUZ[case[1]])
    zhat[1,1:j] <- c(oldzhat[case2[1],1:(j-1)],zsigmahat[1,j]/(sigma1hat));
    ehat[1,1:j] <- c(oldehat[case2[1],1:(j-1)],(w[j] - tempsMUZ[case[1]] -temps[case[1]])/sigma1hat)
    
    # path ends up with state 2:
    lnlk[2,j]   <- log(eta[case[2],j]+1e-50) + oldlnlk[case2[2],j-1]
    path2[,1:j] <- cbind(oldPath[[case2[2]]][,1:(j-1)],c(0,1))
    zsigmahat[2,1:j] = c(oldzsigmahat[case2[2],1:(j-1)], w[j] - mu[2] - tempsMUZ[case[2]])
    zhat[2,1:j] <- c(oldzhat[case2[2],1:(j-1)],zsigmahat[2,j]/(sigma2hat));
    ehat[2,1:j] <- c(oldehat[case2[2],1:(j-1)],(w[j] - tempsMUZ[case[2]] -temps[case[2]])/sigma2hat)
    
    
    j=j+1
  }
  #   lnlipath1 = t(lnlk1);
  #   lnlipath2 = t(lnlk2);
  lnlkRes <- lnlk[which(max(lnlk[,size]) == lnlk[,size])[1],size]
  # if(lnlk[1,size] > lnlk[2,size]){
  #   lnlkRes = lnlk[1,size]} else {lnlkRes = lnlk[2,size]}
  # 
  return(lnlkRes)
}

# source("~/Documents/threestate/arfimaSimH.R")
# 
# dat    <- read.csv("~/Documents/threestate/ReplciationFiles/data/madisonFrom-1930.csv",sep=";")
# pdat   <- apply(combn(ncol(dat),2),2, function(x) log(dat[,x[1]]) - log(dat[,x[2]]))
# pnames <- apply(combn(names(dat),2), 2, function(n) paste0(n,collapse = " - ")) 
# 
# selected <- c("Finland - Greece", "Germany - Italy", "Canada - USA", "Ecuador - Guatemala")
# 
# sdat <- pdat[,pnames %in% selected]
# 
# 
# lowerV <- c( -2,-2,  -2, .6 , .6 , .6 , 0, 0, 0,0.001,-5,-5,-5)
# inits  <- c(1.2, 1, 0.7, .61, .61, .61,.1,.1,.1,   .4,.1,.1,.1) 
# upperV <- c(  2, 2,   2,.999,.999,.999,.3,.3,.3,    5, 5, 5, 5)
# 
# 
# const_mat <- matrix(0,13,13)
# diag(const_mat) <- 1
# const_mat <- rbind(const_mat,-const_mat)
# dmmvec <- -c(0,0,0,1,0,0,1,0,0,0,0,0,0)
# dmmvec <- c(dmmvec,0,dmmvec,0,dmmvec)[1:39]
# const_mat <- rbind(const_mat,matrix(dmmvec,3,,T))
# const_mat <- cbind(const_mat,c(lowerV,-upperV,rep(-1,3)))
# 
# ress    <- lapply(1:ncol(pdat), function(i) constrOptim(inits, function(p) -lnviD3(p,pdat[,i]), NULL, ui = const_mat[,-14], const_mat[,14]))
# save(ress, file = "~/threestate/g7ress.rda")
# thepath <- return_path(res$par,ser)
# plot(apply(res$par[1:3]*thepath,2,sum),type="l")
# 
# res4 <- constrOptim(inits, function(p) -lnviD3(p,sdat[,4]), NULL, ui = const_mat[,-14], const_mat[,14])
# 
# res2 <- res
# plot(1930:2010,matrix(res2$par[1:3],1,) %*% return_path(res2$par,sdat[,2]), type="l")
# 
# 
# ds <- sapply(1:length(ress), function(x) matrix(ress[[x]]$par[1:3],1,) %*% return_path(ress[[x]]$par,pdat[,x]))
# 
# x <- 17
# pnames[x]
# plot(1950:2010,matrix(ress[[x]]$par[1:3],1,) %*% return_path(ress[[x]]$par,pdat[,x]), type="l")
# 
# plot(1930:2010,matrix(res4$par[1:3],1,) %*% return_path(res4$par,sdat[,4]), type="l")
