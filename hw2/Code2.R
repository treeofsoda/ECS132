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


#############3############  PROBLEM C  ######################################

matrix_pow <- function(M, n){
  '
  matrix_pow: function that takes in a square matrix and finds it power.
  
  params:
        M: matrix 
        n: power to which the matrix shoudl be raised 
  '
  ans = M 
  for (i in 2:n){
    ans = ans %*% M
    ans[ans!= 0] = 1
    ans[ans == 0] = 0
    }
  return(ans)
}

diag_fill <- function(M){
  '
  diag_fill: function that fills in the diagonal of a matrix with 1s
  
  params:
        M: matrix
  '
  ans = M 
  for (i in 1:dim(M)[1]){ans[i,i]  = 1}
  return(ans)
}

simConn_single <- function(adjMat){
  '
  simConn: Function that deletes a random edge and checks if the graph connectedness is maintained.
  
  params:
        adjMat: A matrix for the graph, i.e, a R matrix.
  '
  idx = which(adjMat != 0, arr.ind = TRUE)
  nrow = dim(idx)[1]
  deleted_edge = sample(1:nrow,1) # Choose a random edge to be deleted
  deleted_edge = idx[deleted_edge, ]
  adjMat[deleted_edge[1], deleted_edge[2]] = adjMat[deleted_edge[1], deleted_edge[2]] - 1 # Delete edge
  adjMat = diag_fill(adjMat)  # Fill diagnols w 1
  connected_matrix = matrix_pow(adjMat, nrow)  #Raise matrix to power of number of vertex
  isConnected = all(connected_matrix[lower.tri(connected_matrix)] != 0, connected_matrix[upper.tri(connected_matrix)] != 0)
  return(isConnected)
}

simConn <- function(nreps, adjMat){
  '
  simConn: Function that deletes a random edge and finds the distribution of connected of matrix.
  
  params:
        nreps: number of repetions 
        adjMat: A matrix for the graph, i.e, a R matrix or a character string to a csv file containg the matrix. 
  '
  if(class(adjMat) == 'matrix'){data = adjMat}
  else if(class(adjMat) == 'character'){
    data = read.csv(adjMat, sep = ";", header = FALSE)
    data = as.matrix(data)
    rownames(data) = NULL
    colnames(data) = NULL
  }
  isConnected = replicate(nreps, simConn_single(data))
  true_prob = mean(isConnected)
  false_prob = 1 - true_prob
  return(c(true_prob,false_prob))
}