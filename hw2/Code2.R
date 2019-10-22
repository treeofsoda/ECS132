#########################  PROBLEM A  ######################################

simA_single <- function(n, p){
  '
  simA_single: Runs one simulation of the problem and returns the minimum distance 
  
  params:
        n    : number of the number line
        p    : probability that the point is occupied
  '
  
  index_mapping <- 1:n #Get index mapping 
  probs <- runif(n-2)  # Find probability for all points between 1 and n 
  probs[probs >= 1-p] = 1 # Convert to 1's and 0's based on probability
  probs[probs < 1-p] = 0
  probs <- c(1, probs, 1) # Concat it with the first slot and the last slot since they're always taken
  
  data <- cbind(index_mapping, probs)
  valid_index <- subset(data, probs == 1)[,1]
  min_dist = min(diff(valid_index))   ## Find minimum difference 
  return (min_dist)
}

simA <- function(nreps, n, p){
  '
  simA: Runs nreps of the simulation and returns the variance.
  
  params:
        nreps: number of repitions of the experiment 
        n    : number of the number line
        p    : probability that the point is occupied
  '
  
  E_U = replicate(nreps, simA_single(n,p))
  expected_value = mean(E_U)
  difference = (E_U - expected_value)^2
  variance = mean(difference)
  return (variance)
}


#########################  PROBLEM B  ######################################