library(mgcv)
source("03-endogenous-covariates/event-simulator.R")

# Number of simulation repetitions
n_sim = 100

# Number of covariates
p = 20

# Sample size per simulation
n = 1000

# True linear effect coefficient
b1 = 1

# Run the simulator n_sim times and store results as a list
# Each element corresponds to one simulated dataset
remdats <- sapply(1:n_sim, function(x)
  event_simulator(p = p, n = n,
                  b1 = b1, seed=x),
  simplify = FALSE)

# Fit a GAM model to each simulated dataset
gam.fits <- lapply(remdats, function(x){
  
  # Extract the simulated dataset
  x <- x[[1]]
  
  # Create a constant column of 1s
  x$one <- 1
  
  # Combine distance and non-distance matrices into a single matrix
  # Create a matrix of 1s to account for the difference
  dist_MAT <- cbind(x$distance, x$non.distance)
  ONE <- cbind(x$one, -x$one)
  
  # - s(dist_MAT, by=ONE): non-linear effect
  # - delta.rec: linear effect
  # - -1 removes intercept
  
  fit <- gam(one ~ s(dist_MAT, by=ONE) + 
               delta.rec - 1,
             family="binomial",
             data=x)
  
  return(fit)
})

boxplot(unlist(lapply(gam.fits, function(x) 
  x$coefficients[1])))
abline(h=b1, col=2, lwd=2)

# Extract visualization objects for smooth terms
b.dist.objects <- lapply(gam.fits, getViz)

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