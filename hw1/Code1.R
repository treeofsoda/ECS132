simplate <- function(nreps,p){
  "
  simplate: Function that runs the simulation for breaking of a tile and returns the probability s.t. area is less that 
            p.
  params:
        nreps: number of repetitions
        p: area 
        
        returns: probability where area of broken tile < p
  "

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
    if (y<0 | y>1){
      print("Messed up y")
    }
    else{
      x_cut <- rbind(x_cut, c(i,y))
    }
    
    x = (i - intercept)/m
    if (x<0 | x>1){
      print("Messed up X")
    }
    else{
      y_cut <- rbind(y_cut, c(x,i))
    }
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
