library(tigris)
library(sf)
library(geosphere)

states <- states(cb = TRUE)
states_sf <- st_as_sf(states)
states <- unique(states$NAME)

dist_matrix_new <- matrix(0, nrow = length(states),
                            ncol = length(states),
                            dimnames = list(states, states))

for (i in 1:length(states)) {
  print(i)
  for (j in 1:length(states)) {
    if (i != j) {
      state1 <- states_sf[states_sf$NAME == states[i], ]
      state2 <- states_sf[states_sf$NAME == states[j], ]
      dist_matrix_new[i, j] <- min(st_distance(state1, state2))
    }
  }
}
load(file="dist-USA.RData")
all(dist_matrix_new==dist_matrix)