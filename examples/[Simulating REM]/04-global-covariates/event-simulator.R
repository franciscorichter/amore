library(tigris)
library(sf)
library(geosphere)
library(mgcv)
library(mgcViz)

# ------------------------------------------------------------
# Load US state shapefiles (cartographic boundary version)
# ------------------------------------------------------------
# size of simulation
states <- states(cb = TRUE)

# Convert to simple feature (sf) object for spatial operations
states_sf <- st_as_sf(states)

# Keep unique state names only
states <- unique(states$NAME)

# ------------------------------------------------------------
# Define simulation dimensions
# ------------------------------------------------------------

# number of nodes (states in the network)
p <- length(states)

# number of time-varying edges (number of simulated events)
n <- 1000

# ------------------------------------------------------------
# Load exogenous distance information
# ------------------------------------------------------------

# load distance matrix (see 02-exogenous-information/dist_matrix_script) 
# and its true non-linear effect
source("02-exogenous-information/true_non_linear_effect.R")

# Plot of the true spline-based nonlinear effect
coefficients_plot

### Linear effect of weekday
b0 = 1.5

### Linear effect of reciprocity
b1 = 0.5

# ------------------------------------------------------------
# Event Simulation Function
# ------------------------------------------------------------

event_simulator <- function(states, p, n, b0, b1, fit, seed) {
  
  set.seed(seed)
  print(seed)
  
  # some constants
  current.tm<-0
  rec<-matrix(0,ncol=p,nrow=p)
  
  #where to save
  dat<-NULL
  nondat<-NULL
  tms<-NULL
  tm = 0  # Initialize time to 0
  
  # Initialize the invaded list with a random sample of states
  last_contacted <- matrix(NA, nrow=n+1, ncol=p)
  last_contacted[1,] <- sample(states, p)
  colnames(last_contacted) <- states
  
  # Compute the distance matrix for each state and potential state pairs
  dist.m <- t(sapply(states, function(st) 
    sapply(states, function(pot.st)
      dist_matrix[pot.st, last_contacted[1,st]])))
  distance <- matrix(spline_function(as.vector(dist.m),
                                     fit), 
                     nrow=p, ncol=p)
  
  #simulation of events
  i<-1
  while (i <=n){
    
    if(i%%100==0) print(i)
    current.weekday<-1*(current.tm%%7>=2)
    hazard<- exp(b0*current.weekday+b1*rec+distance)/(p^2)
    #no self-loops
    # diag(hazard)<-0
    
    # time increase
    dt<-rexp(1,sum(hazard))
    
    # check if this change meant that the event 
    # moved from weekend to weekday or vice-versa:
    new.weekday<-1*((current.tm+dt)%%7>=2)
    if (dt>5| new.weekday!=current.weekday){
      current.tm<- (current.tm%/%7)*7+2+current.weekday*5
    } else {
      current.tm<-current.tm+dt
      tms<-c(tms,current.tm)
      event.id<-sample(1:p^2, 1,prob = hazard/sum(hazard))
      s<-(event.id-1)%%p+1
      r<-(event.id-1)%/%p+1
      dat<-rbind(dat,c(current.tm,s,r,event.id,
                       current.weekday,
                       dist.m[s,r],
                       rec[s,r]))
      
      non.event.id <- sample(setdiff(1:p^2, event.id),1)
      non.s <- (non.event.id-1)%%p+1
      non.r <- (non.event.id-1)%/%p+1
      nondat <- rbind(nondat,c(non.s,non.r,non.event.id,
                               dist.m[non.s,non.r],
                               rec[non.s,non.r]))
      
      rec[r,s] <- rec[r,s] + 1
      last_contacted[i+1,] <- last_contacted[i,]
      last_contacted[i+1,states[s]] <- states[r]
      
      dist.m <- t(sapply(states, function(st) 
        sapply(states, function(pot.st)
          dist_matrix[pot.st, last_contacted[i+1,st]])))
      distance <- matrix(spline_function(as.vector(dist.m),
                                         fit), 
                         nrow=p, ncol=p)
      hazard<- exp(b0*current.weekday+b1*rec+distance)/(p^2)
      # diag(hazard)<-0
      
      i<-i+1
    }
  }
  
  remdat <- cbind(1:n, dat, nondat,
                  dat[,6]-nondat[,4],
                  dat[,7]-nondat[,5],
                  1)
  colnames(remdat) <- c("index", "tms","s","r","event.id",
                        "weekday","distance", "rec",
                        "non.s","non.r","non.event.id",
                        "non.distance", "non.rec",
                        "delta.dist", "delta.rec","one")
  remdat <- as.data.frame(remdat)
  
  return(list(remdat, last_contacted))
}
