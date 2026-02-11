##Exercise
## 1.1 
dev.off()
foodweb = matrix(
  c(
    # Oak, Grass, Caterpillar, Deer, Bird, Fox
    0,    0,        0.5,     0.6,  0,    0,   # Oak
    0,    0,        0.4,     0.4,  0,    0,   # Grass
    0,    0,        0,       0,    0.9,  0.2, # Caterpillar
    0,    0,        0,       0,    0,    0,   # Deer
    0,    0,        0,       0,    0,    0.7, # Bird
    0,    0,        0,       0,    0,    0    # Fox
  ),
  nrow = 6,
  byrow = TRUE,
  dimnames = list(
    c("Oak","Grass","Caterpillar","Deer","Bird","Fox"),   
    c("Oak","Grass","Caterpillar","Deer","Bird","Fox")    
  )
)


#Calculate the connectance and the weighted connectance, exclude cannibalism 
#(number of interactions observed/possible interactions)

# Calculate the generality (number of prey each species consumes).

# Calculate the vulnerability (number of predators consuming each species).

# Calculate the degree of each species (number of total links).

# Remove "Caterpillar" from the network

# Recalculate connectance, generality, vulnerability, degree

# Compare the new network to the original, what are your conclusions?

