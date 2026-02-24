library(tigris)
library(sf)
library(geosphere)
library(mgcv)
library(mgcViz)

# ------------------------------------------------------------
# Load US state
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

# ------------------------------------------------------------
# Simulate events
# ------------------------------------------------------------

source("04-global-covariates/event-simulator.R")

# Number of simulation repetitions
n_sim = 100

# True linear effect coefficients
b0 = 1.5
b1 = 0.5

# Run the simulator n_sim times and store results as a list
# Each element corresponds to one simulated dataset
remdats <- sapply(1:n_sim, function(x)
  event_simulator(states = states, p = length(states), n = n,
                  b0 = b0, b1 = b1, fit, seed=x),
  simplify = FALSE)

remdat_shifting <- function(remdat, last_contacted, p, n, seed){
  
  # Total observation window length (max event time)
  T<-max(remdat$tms)
  
  # Random time shifts for each possible dyad
  shifts<-rexp(p^2,1/T)
  
  # Container for completed dataset with updated non-events
  remdat_complete<-NULL
  
  # Set seed for reproducibility of non-event sampling
  set.seed(seed)
  
  # Loop over observed events
  for (i in 1:n){
    
    # Current event time
    current.tm <- remdat[i,2]
    
    # Determine which dyads are "at risk" for being selected as controls
    # atrisk is a p^2-length vector:
    # keeps dyads whose shifted times fall into the admissible window
    # see Lembo et al. 2025
    atrisk<-((shifts<current.tm+shifts[remdat[i,5]]) & 
               (shifts+T>current.tm+shifts[remdat[i,5]]))
    
    # Sample a non-event dyad from the shifted set
    non.event.id <- sample(1:p^2,1,prob = atrisk/sum(atrisk))
    
    # Convert non-event id to sender (non.s) and receiver (non.r)
    non.s<-(non.event.id-1)%%p+1
    non.r<-(non.event.id-1)%/%p+1
    
    t_star <- current.tm + shifts[remdat[i,5]] - shifts[non.event.id]
    
    # Weekday indicator for the shifted time 
    # (converted to original time scale)
    non.current.weekday<-1*(t_star%%7>2)
    
    # Reciprocity for the non-event:
    # count how many past events before shifted time 
    # (converted to original time scale)
    # occurred from non.r -> non.s
    non.rec<-sum((remdat[,2]<t_star)&(remdat[,3]==non.r)&(remdat[,4]==non.s))
    
    # Recompute distance matrix
    idxs <- which(remdat[,2] < t_star)
    j <- if (length(idxs)) max(idxs) else 1
    dist.m <- t(sapply(states, function(st) 
      sapply(states, function(pot.st)
        dist_matrix[pot.st, last_contacted[j,st]])))
    
    # Append: observed event row (cols 1:8) + sampled non-event info
    remdat_complete<-rbind(remdat_complete,
                           unlist(c(remdat[i,1:8],
                                    non.s,non.r,non.event.id,
                                    non.current.weekday,
                                    dist.m[non.s,non.r],non.rec)))
  }
  
  colnames(remdat_complete) <- c(colnames(remdat)[1:8],
                                 "non.s","non.r","non.event.id",
                                 "non.weekday","non.distance","non.rec")
  remdat_complete <- as.data.frame(remdat_complete)
  
  # Difference in distance covariate
  remdat_complete$delta.dist = remdat_complete$distance - 
    remdat_complete$non.distance 
  
  # Difference in weekday indicator
  remdat_complete$delta.wkday = remdat_complete$weekday - 
    remdat_complete$non.weekday
  
  # Difference in reciprocity covariate
  remdat_complete$delta.rec = remdat_complete$rec - 
    remdat_complete$non.rec
  
  # Constant column for intercept terms in modeling
  remdat_complete$one = 1
  
  # Return completed shifted case-control dataset
  return(remdat_complete) 
  
}

shifted_rems <- sapply(1:n_sim, function(x)
  remdat_shifting(remdat = remdats[[x]][[1]], 
                  last_contacted = remdats[[x]][[2]], p, n, seed=x), 
  simplify = FALSE)

gam.fits_shifted <- lapply(shifted_rems, function(x){
  dist_MAT <- cbind(as.numeric(x$distance), 
                    as.numeric(x$non.distance))
  ONE <- cbind(x$one, -x$one)
  fit <- gam(one ~ s(dist_MAT, by=ONE, k=11, bs="bs") + 
               delta.rec + delta.wkday - 1,
             family="binomial",
             data=x)
  return(fit)
}
)

boxplot(unlist(lapply(gam.fits_shifted, function(x) 
  x$coefficients[1])))
abline(h=b1, col=2, lwd=2)

boxplot(unlist(lapply(gam.fits_shifted, function(x) 
  x$coefficients[2])))
abline(h=b0, col=2, lwd=2)

# Extract visualization objects for smooth terms
b.dist.objects <- lapply(gam.fits_shifted, getViz)

# Create dataframe for plotting the TRUE spline function
data <- data.frame(
  x = as.vector(dist_matrix),
  y = spline_function(as.vector(dist_matrix), fit) + 0.75
)

# Initialize ggplot with true spline (light coral line)
coefficients_plot <- ggplot(data=data, aes(x, y)) +
  geom_line(data=data,
            linetype = 1, col="lightcoral",
            size=1)

# Add estimated smooth curves from each simulation (light gray lines)
for (iter in 1:n_sim){
  
  # Extract smooth object for this iteration
  b.iter <- b.dist.objects[[iter]]
  
  # Extract smooth term plot data
  o.iter <- plot(sm(b.iter, 1))
  
  # Add estimated smooth curve to plot
  coefficients_plot <- coefficients_plot + 
    geom_line(data=o.iter$data$fit, 
              linetype = 3, col="lightgray", 
              size=0.5)
}

# Add final overlay of true spline (red + coral for emphasis)
coefficients_plot <- coefficients_plot +
  geom_line(data=data,
            linetype = 1, col=2,
            size=1) +
  geom_line(data=data,
            linetype = 1, col="lightcoral",
            size=1) +
  labs(title = "ESTIMATED vs TRUE non-linear effect (Model with NO SHIFTING)",
       x = "Covariate",
       y = "Log-hazard contribution") +
  theme_minimal() +
  theme(legend.position = "none",  
        axis.text.x = element_text(angle = 0, hjust = 1))

# Display final plot
coefficients_plot

save.image("04-global-covariates/simulation.RData")
