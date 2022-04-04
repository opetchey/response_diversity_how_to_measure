## none yet


resp_div <- function(x, stat = "range", sign_sens = TRUE) {
  
  flag <- TRUE # flag to catch if all values are the same
  
  ## set diversity to zero if all values are the same
  if(length(unique(x)) == 1) {
    div = 0
    flag <- FALSE
  }
  
  ## keep going only if all values are not the same
  if(flag) {
    
    ## stat == range
    if(stat == "range") {
      
      if(!sign_sens) {
        div <- max(x) - min(x)
      }
      
      if(sign_sens) {
        div1 <- max(x) - min(x)
        div2 <- abs(abs(max(x)) - abs(min(x)))
        div <- (div1 - div2) / div1
      }
    }
    
    ## stat == sd
    if(stat == "sd") {
      
      if(!sign_sens) {
        div <- sd(x)
      }
      
      if(sign_sens) {
        div1 <- sd(x)
        div2 <- sd(abs(x))
        div <- (div1 - div2) / div1
      }
    }
  }
    
  names(div) <- paste(stat, sign_sens, sep="-")
    
  div
  
}
  
# colour palette
Isfahan1 <- list(c("#4e3910", "#845d29", "#d8c29d", "#4fb6ca", "#178f92", "#175f5d", "#1d1f54"), c(5, 2, 4, 6, 1, 7, 3), colorblind=TRUE) 

# function for standardising range 0-1
range01 <- function(x){(x-min(x))/(max(x)-min(x))} 
