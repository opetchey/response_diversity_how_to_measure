## Function for calculating two dimensions of response diversity
resp_div <- function(x, sign_sens = TRUE) {
  
  flag <- TRUE # flag to catch if all values are the same
  
  ## set diversity to zero if all values are the same
  if(length(unique(x)) == 1) {
    div = 0
    flag <- FALSE
  }
  
  ## keep going only if all values are not the same
  if(flag) {
    
    ## stat == range
    if(!sign_sens) {
        
        d <- dist(x, diag = T) # euclidean distance matrix
        Z <- exp(as.matrix(-d)) # similarity matrix
        nspecies <- length(x)
        p=matrix(1/nspecies,nspecies) # relative abundance matrix. Needs to be changed if evenness is of interest
        lenq = 1 # initialises hill number. Need to fix if we want any evenness indices
        qq <- seq(length=lenq, from=0, by=.11)
        
        # Initialise the Zp matrix to zero
        Zp=matrix(0,nspecies,1)
        
        # Compute Zp
        for (i in 1:nspecies){
          for (j in 1:nspecies){
            Zp[i,1]<-Zp[i,1]+Z[i,j]*p[j,1]
          }
        }
        
        # Initialise the Diversity matrix to zero
        Dqz = matrix(0, lenq ,1)
        
        for (iq in 1:lenq)  {
          q<-qq[iq];
          for (zpi in 1:length(Zp[,1])){
            if (Zp[zpi,1]>0)(
              Dqz[iq,1]<-Dqz[iq,1]+ p[zpi,1]*(Zp[zpi,1])^(q-1))
          }
          
          Dqz[iq,1] <- Dqz[iq,1]^(1/(1-q));
        }
        div <- Dqz[iq,1]
    }
        
      }
      
      if(sign_sens) {
        div1 <- max(x) - min(x)
        div2 <- abs(abs(max(x)) - abs(min(x)))
        div <- (div1 - div2) / div1
    }
  
  names(div) <- paste(sign_sens)
  
  div
  
}

# function for standardising range 0-1
range01 <- function(x) {
  (x-min(x))/(max(x)-min(x))} 

# colour palette ----
Isfahan1 <- list(c("#4e3910", "#845d29", "#d8c29d", "#4fb6ca", "#178f92", "#175f5d", "#1d1f54"), c(5, 2, 4, 6, 1, 7, 3), colorblind=TRUE) 
