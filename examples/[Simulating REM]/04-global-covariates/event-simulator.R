event_simulator <- function(states, p, n, b0, b1, fit, seed) {
  
  # Set seed for reproducibility
  set.seed(seed)
  print(seed)
  
  # ----------------------------------------------------------
  # Initialize simulation objects
  # ----------------------------------------------------------
  
  # Continuous time variable
  current.tm<-0
  
  # Reciprocity matrix (counts reverse interactions r -> s)
  rec<-matrix(0,ncol=p,nrow=p)
  
  # Containers to store event and non-event observations
  dat<-NULL
  nondat<-NULL
  tms<-NULL
  
  # Initialize time variable (unused but kept for clarity)
  tm = 0  
  
  # ----------------------------------------------------------
  # Initialize last-contacted state tracking
  # ----------------------------------------------------------
  
  # Matrix storing most recent contacted state for each node
  last_contacted <- matrix(NA, nrow=n+1, ncol=p)
  
  # Random initial last-contacted assignment
  last_contacted[1,] <- sample(states, p)
  
  # Assign state names as column names
  colnames(last_contacted) <- states
  
  # ----------------------------------------------------------
  # Compute initial distance-based nonlinear effect
  # ----------------------------------------------------------
  
  # Distance between each state and its last contacted state
  dist.m <- t(sapply(states, function(st) 
    sapply(states, function(pot.st)
      dist_matrix[pot.st, last_contacted[1,st]])))
  
  # Apply spline-based nonlinear transformation
  distance <- matrix(spline_function(as.vector(dist.m),
                                     fit), 
                     nrow=p, ncol=p)
  
  # ----------------------------------------------------------
  # Simulation loop
  # ----------------------------------------------------------
  
  i<-1
  while (i <=n){
    
    # Print progress every 100 iterations
    if(i%%100==0) print(i)
    
    # Indicator for weekday (1) vs weekend (0)
    current.weekday<-1*(current.tm%%7>=2)
    
    # Hazard includes:
    # - weekday effect (b0)
    # - reciprocity effect (b1)
    # - nonlinear distance effect
    hazard<- exp(b0*current.weekday+b1*rec+distance)/(p^2)
    
    # Draw waiting time from exponential distribution
    dt<-rexp(1,sum(hazard))
    
    # Check whether time jump crosses weekday/weekend boundary
    new.weekday<-1*((current.tm+dt)%%7>=2)
    
    # If jump is too large or crosses boundary,
    # move time directly to boundary without recording event
    
    if (dt>5 | new.weekday!=current.weekday){
      
      # Jump to next weekday/weekend boundary
      current.tm<- (current.tm%/%7)*7+2+current.weekday*5
      
    } else {
      
      # Update time normally
      current.tm<-current.tm+dt
      
      # Store event time
      tms<-c(tms,current.tm)
      
      # Sample event proportional to hazard
      event.id<-sample(1:p^2, 1,prob = hazard/sum(hazard))
      
      # Convert index to sender (s) and receiver (r)
      s<-(event.id-1)%%p+1
      r<-(event.id-1)%/%p+1
      
      # Store event data
      dat<-rbind(dat,c(current.tm,s,r,event.id,
                       current.weekday,
                       dist.m[s,r],
                       rec[s,r]))
      
      # ------------------------------------------------------
      # Sample one non-event pair for case-control
      # ------------------------------------------------------
      
      non.event.id <- sample(setdiff(1:p^2, event.id),1)
      
      non.s <- (non.event.id-1)%%p+1
      non.r <- (non.event.id-1)%/%p+1
      
      # Store non-event information
      nondat <- rbind(nondat,c(non.s,non.r,non.event.id,
                               dist.m[non.s,non.r],
                               rec[non.s,non.r]))
      
      # ------------------------------------------------------
      # Update system after event
      # ------------------------------------------------------
      
      # Update reciprocity in reverse direction
      rec[r,s] <- rec[r,s] + 1
      
      # Carry forward last-contacted history
      last_contacted[i+1,] <- last_contacted[i,]
      
      # Update sender's last contacted state
      last_contacted[i+1,states[s]] <- states[r]
      
      # Recompute distance matrix after update
      dist.m <- t(sapply(states, function(st) 
        sapply(states, function(pot.st)
          dist_matrix[pot.st, last_contacted[i+1,st]])))
      
      # Recompute nonlinear distance effect
      distance <- matrix(spline_function(as.vector(dist.m),
                                         fit), 
                         nrow=p, ncol=p)
      
      # Recompute hazard with updated values
      hazard<- exp(b0*current.weekday+b1*rec+distance)/(p^2)
      
      # Move to next event
      i<-i+1
    }
  }
  
  # ----------------------------------------------------------
  # Construct final case-control dataset
  # ----------------------------------------------------------
  
  remdat <- cbind(1:n, dat, nondat,
                  dat[,6]-nondat[,4],   # delta distance
                  dat[,7]-nondat[,5],   # delta reciprocity
                  1)
  
  # Assign column names
  colnames(remdat) <- c("index", "tms","s","r","event.id",
                        "weekday","distance", "rec",
                        "non.s","non.r","non.event.id",
                        "non.distance", "non.rec",
                        "delta.dist", "delta.rec","one")
  
  remdat <- as.data.frame(remdat)
  
  # Return:
  # 1) Simulated case-control dataset
  # 2) Last-contacted history
  return(list(remdat, last_contacted))
}
