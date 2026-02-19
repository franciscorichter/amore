library(mgcv)
library(mgcViz)
load(file="dist-USA.RData")

dist_matrix <- log(dist_matrix/100000+1)
range(dist_matrix)

f_x <- as.vector(sin(-dist_matrix/1.5))
dist <- as.vector(dist_matrix)
fit <- gam(f_x ~ s(dist) )
spline_function <- function(dist_matrix, fit) {
  return(predict(fit, newdata = data.frame(dist=dist_matrix)))
}

data <- data.frame(x = as.vector(dist_matrix),
                   y = spline_function(as.vector(dist_matrix),
                                       fit))
coefficients_plot <- ggplot(data=data, aes(x, y)) +
  geom_line(data=data,
            linetype = 1, col=2,
            size=1)

coefficients_plot
