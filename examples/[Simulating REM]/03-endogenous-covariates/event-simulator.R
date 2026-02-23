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

### Linear effect of reciprocity
# coefficient controlling reciprocity strength in hazard
b1 = 0.5

# ------------------------------------------------------------
# Event Simulation Function
# ------------------------------------------------------------

event_simulator <- function(states, p, n, fit, b1, seed) {
  
  # Set seed for reproducibility
  set.seed(seed)
  print(seed)
  
  # ----------------------------------------------------------
  # Initialize simulation objects
  # ----------------------------------------------------------
  
  # Current continuous time
  current.tm<-0
  
  # Reciprocity matrix (counts past interactions r -> s)
  rec<-matrix(0,ncol=p,nrow=p)
  
  # Containers for storing event and non-event data
  dat<-NULL
  nondat<-NULL
  tms<-NULL
  
  # Initialize time to 0
  tm = 0  
  
  # ----------------------------------------------------------
  # Initialize last contacted state per node
  # ----------------------------------------------------------
  
  # Matrix tracking most recent contact for each state
  last_contacted <- matrix(NA, nrow=n+1, ncol=p)
  
  # Randomly initialize last contacted state for each node
  last_contacted[1,] <- sample(states, p)
  
  # Assign state names to columns
  colnames(last_contacted) <- states
  
  # ----------------------------------------------------------
  # Compute initial distance-based effect
  # ----------------------------------------------------------
  
  # Compute distance matrix between each state and its last contacted state
  dist.m <- t(sapply(states, function(st) 
    sapply(states, function(pot.st)
      dist_matrix[pot.st, last_contacted[1,st]])))
  
  # Apply spline-based nonlinear transformation to distances
  distance <- matrix(spline_function(as.vector(dist.m),
                                     fit), 
                     nrow=p, ncol=p)
  
  # ----------------------------------------------------------
  # Simulation loop over n events
  # ----------------------------------------------------------
  
  i<-1
  while (i <=n){
    
    # Print progress every 100 iterations
    if(i%%100==0) print(i)
    
    # Hazard rate for all possible directed pairs
    # Includes reciprocity effect and nonlinear distance effect
    hazard<- exp(b1*rec+distance)/(p^2)
    
    # Draw waiting time from exponential distribution
    dt<-rexp(1,sum(hazard))
    
    # Update current simulation time
    current.tm <- current.tm + dt
    
    # Store event time
    tms<-c(tms,current.tm)
    
    # Sample event proportional to hazard
    event.id<-sample(1:p^2, 1,prob = hazard/sum(hazard))
    
    # Convert linear index to sender (s) and receiver (r)
    s<-(event.id-1)%%p+1
    r<-(event.id-1)%/%p+1
    
    # Store event data
    dat<-rbind(dat,c(current.tm,s,r,event.id,
                     dist.m[s,r],
                     rec[s,r]))
    
    # --------------------------------------------------------
    # Sample one non-event pair for case-control 
    # --------------------------------------------------------
    
    non.event.id <- sample(setdiff(1:p^2, event.id),1)
    
    non.s <- (non.event.id-1)%%p+1
    non.r <- (non.event.id-1)%/%p+1
    
    # Store non-event information
    nondat <- rbind(nondat,c(non.s,non.r,non.event.id,
                             dist.m[non.s,non.r],
                             rec[non.s,non.r]))
    
    # --------------------------------------------------------
    # Update system after event occurs
    # --------------------------------------------------------
    
    # Update reciprocity (reverse direction incremented)
    rec[r,s] <- rec[r,s] + 1
    
    # Copy previous last-contacted structure
    last_contacted[i+1,] <- last_contacted[i,]
    
    # Update last contacted state for sender
    last_contacted[i+1,states[s]] <- states[r]
    
    # Recompute distance matrix after state update
    dist.m <- t(sapply(states, function(st) 
      sapply(states, function(pot.st)
        dist_matrix[pot.st, last_contacted[i+1,st]])))
    
    # Recompute nonlinear distance effect
    distance <- matrix(spline_function(as.vector(dist.m),
                                       fit), nrow=p, ncol=p)
    
    # Recompute hazard with updated values
    hazard<- exp(b1*rec+distance)/(p^2)
    
    i<-i+1
  }
  
  # ----------------------------------------------------------
  # Construct final dataset for analysis
  # ----------------------------------------------------------
  
  # Combine event and non-event information
  remdat <- cbind(1:n, dat, nondat,
                  dat[,5]-nondat[,4],   # difference in distance
                  dat[,6]-nondat[,5],   # difference in reciprocity
                  1)
  
  # Assign column names
  colnames(remdat) <- c("index", "tms","s","r","event.id",
                        "distance", "rec",
                        "non.s","non.r","non.event.id",
                        "non.distance", "non.rec",
                        "delta.dist", "delta.rec","one")
  
  # Convert to data.frame
  remdat <- as.data.frame(remdat)
  
  # Return:
  # 1) simulated case-control dataset
  # 2) history of last contacted states
  return(list(remdat, last_contacted))
}
