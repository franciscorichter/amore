event_simulator <- function(p, n, b1, seed) {
  
  set.seed(seed)
  print(seed)
  
  x <- matrix(rnorm(p^2), ncol=p, nrow=p)
  
  current.tm<-0
  
  #where to save
  dat<-NULL
  nondat<-NULL
  tms<-NULL
  tm = 0  # Initialize time to 0
  
  #simulation of events
  i<-1
  while (i <=n){
    
    if(i%%100==0) print(i)
    
    hazard<- exp(b1*x)
    
    # time increase
    dt<-rexp(1,sum(hazard))
    current.tm<-current.tm+dt
    tms<-c(tms,current.tm)
    event.id<-sample(1:p^2, 1,prob = hazard/sum(hazard))
    s<-(event.id-1)%%p+1
    r<-(event.id-1)%/%p+1
    dat<-rbind(dat,c(current.tm,s,r,event.id,
                     x[s,r]))
      
    non.event.id <- sample(setdiff(1:p^2, event.id),1)
    non.s <- (non.event.id-1)%%p+1
    non.r <- (non.event.id-1)%/%p+1
    nondat <- rbind(nondat,c(non.s,non.r,non.event.id,
                             x[non.s,non.r]))

    hazard<- exp(b1*x)
    i<-i+1
  }
  
  remdat <- cbind(1:n, dat, nondat,
                  dat[,5]-nondat[,4],
                  1)
  colnames(remdat) <- c("index", "tms","s","r","event.id",
                        "x","non.s","non.r","non.event.id",
                        "non.x", "delta.x", "one")
  remdat <- as.data.frame(remdat)
  
  return(remdat)
}