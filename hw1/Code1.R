################# PROBLEM A ##########################################
leave_single <-function(){
  "
  leave_two: Function that use simulation to determine if the passenger alighting at stop 2 belonges to a pair.
  "
  ## define the 3 points that will be used to check how many individuals board the bus
  #P_INDIVIDUAL_NONE = 0.5
  #P_INDIVIDUAL_SINGLE = 0.9
  #P_INDIVIDUAL_DOUBLE = 1
  P_ALIGHT = 0.2
  #passengers count as individuals and pairs
  count_pair = 0
  
  ## define the 3 points that will be used to check how many pairs board the bus
  P_PAIR_NONE = 0.4
  P_PAIR_SINGLE = 1
  
  #Simulate the boarding process at Stop 1
  #p_individual_b1 = runif(1)
  p_pair_b1 = runif(1)
  if (p_pair_b1>=0.4){count_pair = count_pair + 1}
  else{return (FALSE)}
  
  #Simulate departure process at stop 2
  p_pair_l2 = runif(1)
  if (p_pair_l2< 0.2){return (TRUE)}
  else{return (FALSE)}
}

leave_simulate <-function(nreps){
  '
  leave_simulate: Function that finds the probability that a passenger alighting at stop 2 belonges to a pair 
  
  params:
        nreps: number of repitions of the simulation
        
        returns: probability that a passenger alighting at stop 2 belonges to a pair 
  '
  prob = mean(replicate(nreps,leave_single()))
  return(prob)
}

################# PROBLEM B ##########################################

simline <- function(nreps, p, q, k, r){
  '
  simline: Function that runs the simulation as described by the problem and checks if the flag is raised at bit R.
  params : 
         nreps: number of repitions of the simulation
         p    : Probability that the actual bit sent is 1
         q    : Probability that the line will fail
         k    : length of consecutive 0s after which the flag is reported
         r    : The bit AT which to check if the flag is raised
  '
  
  prob = mean(replicate(nreps,single_simline(p,q,k,r)))
  return(prob)
}

single_simline <- function(p, q, k, r){
  '
  single_simulate: Function that finds the probability that a flag is raised at bit r, r = k+1, k+2, k+,... . 
  params:
        p    : Probability that the actual bit sent is 1
        q    : Probability that the line will fail
        k    : length of consecutive 0s after which the flag is reported
        r    : The bit AT which to check if the flag is raised
  '
  previous_error = FALSE  
  len = 0 #Length up until now
  flag = FALSE
  start_count = FALSE ## Start counting once you see 1 
  k_count = 0
  
  while(flag == FALSE &  len <= r+4){
    input = generate_actual_input(p)  #Generate input
    output = generate_line_output(q, input, previous_error) #Generate output
    len = len + 1
    previous_error = output[2] #Assign prev_error
    
    ## If output is 1 then 
    if (output[1] == 1){
      start_count = TRUE
      k_count = 0
    }
    
    # start counting number of 0's if start count has been sent
    if (start_count == TRUE & output[1] == 0  ){
      k_count = k_count + 1;
    }
    
    if(k_count == k ){
      if (len == r){return(TRUE)}
      flag = TRUE
    }
  }
  return (FALSE)
}

generate_actual_input <- function(p){
  '
  generate_actual_input: Function that returns value 1, if the random number is less than p else returns 0
  
  params:
        p    : Probability of getting a 1
  '
  if (runif(1)< p){return (1)}
  else{return (0)}
}

generate_line_output <- function(q, input, prev_error){
  '
  generate_line_output: Function that simulates the behavior of the line.
  
  params:
        q         : Probability of line failing
        input     : The input given by user
        prev_error: Boolean indicating if error was previously reported.
  '
  
  if (prev_error == TRUE){
    return (c(0, TRUE))
  }
  else{
    if (runif(1) < q){return (c(0,TRUE))}
    else {return (c(input, FALSE))}
  }
}



################# PROBLEM C ##########################################
simplate <- function(nreps,p){
  "
  simplate: Function that runs the simulation for breaking of a tile and returns the probability s.t. area is less that 
            p.
  params:
        nreps: number of repetitions
        p: area 
        
        returns: probability where area of broken tile < p
  "
  prob = mean(replicate(nreps,singplate(p)))
  return(prob)

}

singplate <- function(p){
  "
  singplate: Function that runs a single simulation and checks if the area is less than p
  params:
        p: area 
        
        returns:  Returns wheter area was less than p or not
  "
  coords = runif(2) # Get co-ordinates for break point 
  theta = runif(min = 0, max = 180, n = 1) # Find the angle at which it breaks
  m = tan(theta*0.0174532925) #Find slope 
  intercept = coords[2] - m * coords[1] # Find the intercept
  #loop through edge cases and find corresponding intercept where it does pass through the tile
  x_cut <- matrix(nrow = 0, ncol = 2)
  y_cut <- matrix(nrow = 0, ncol = 2)

  for (i in c(0,1)){
    y = m * i + intercept
    if(y>=0 & y <=1){x_cut <- rbind(x_cut, c(i,y))}
    x = (i - intercept)/m
    if(x>=0 & x <=1){y_cut <- rbind(y_cut, c(x,i))}
  } #End for 
  
  #Find edge points
  ### CASE A 
  if (dim(x_cut)[1] == 2){ #Check for points going through opposite edges
    xs = c(0,x_cut[1,1],x_cut[2,1],1)
    ys = c(0,x_cut[1,2],x_cut[2,2],0)
    a1 = polyarea(ys,xs)
    a2 = 1 - a1
    a3 = lowerVal(a1,a2)
    if (a3<p){return (TRUE)}
    else{return (FALSE)}
  }
  ### CASE B 
  else if (dim(y_cut)[1] == 2){ #Check for points going through opposite edges
    xs = c(0,y_cut[1,1],y_cut[2,1],0)
    ys = c(0,y_cut[1,2],y_cut[2,2],1)
    a1 = polyarea(xs,ys)
    a2 = 1 - a1
    a3 = lowerVal(a1,a2)
    if (a3<p){return (TRUE)}
    else{return (FALSE)}
  }
  ### CASE C 
  else if(x_cut[1,1] == 0 & y_cut[1,2]==1 ){
    xs = c(0,0,y_cut[1,1],1,1)
    ys = c(0,x_cut[1,2],1,1,0)
    a1 = polyarea(ys,xs)
    a2 = 1 - a1
    a3 = lowerVal(a1,a2)
    if (a3<p){return (TRUE)}
    else{return (FALSE)}
  }
  ### CASE D 
  else if (x_cut[1,1]==1 & y_cut[1,2]==1){
    xs = c(0,0,y_cut[1,1],1,1)
    ys = c(0,1,1,x_cut[1,2],0)
    a1 = polyarea(ys,xs)
    a2 = 1 - a1
    a3 = lowerVal(a1,a2)
    if (a3<p){return (TRUE)}
    else{return (FALSE)}
  }
  ### CASE E 
  else if (x_cut[1,1]==0 & y_cut[1,2]==0){
    xs = c(0,0,y_cut[1,1])
    ys = c(0,x_cut[1,2], 0 )
    a1 = polyarea(ys,xs)
    a2 = 1 - a1
    a3 = lowerVal(a1,a2)
    if (a3<p){return (TRUE)}
    else{return (FALSE)}
  }
  ### CASE F 
  else if (x_cut[1,1]==1 & y_cut[1,2]==0){
    xs = c(0,0,1,1,y_cut[1,1])
    ys = c(0,1,1,x_cut[1,2],0)
    a1 = polyarea(ys,xs)
    a2 = 1 - a1
    a3 = lowerVal(a1,a2)
    if (a3<p){return (TRUE)}
    else{return (FALSE)}
  }
}

"Function that returns the lower value"
lowerVal <- function(a1,a2){
  if (a1<a2){return (a1)}
  else{return(a2)}
}

" Function taken from library: Pracma that calculates area of polygon given co-ordinates"
polyarea <- function(x, y) {
  if (length(x) == 0 && length(y) == 0) return(0)
  if (!(is.numeric(x) || is.complex(x)) ||
      !(is.numeric(y) || is.complex(y)))
    stop("Arguments 'x' and 'y' must be real or complex.")
  if (is.null(dim(x))) x <- matrix(x, length(x), 1)
  if (is.null(dim(y))) y <- matrix(y, length(y), 1)
  if (any(dim(x) != dim(y)))
    stop("Matrices 'x' and 'y' must be of same size.")

  n <- nrow(x); m <- ncol(x)
  z <- numeric(m)
  for (i in 1:m) {
    xi <- x[, i]
    yi <- y[, i]
    # Gauss' formula
    p1 <- sum(xi[1:(n-1)]*yi[2:n]) + xi[n]*yi[1]
    p2 <- sum(xi[2:n]*yi[1:(n-1)]) + xi[1]*yi[n]
    z[i] <- 0.5*(p1-p2)
  }
  return(z)
}
