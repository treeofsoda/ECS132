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
  c = a[2] - m * a[1] # Find the intercept
  y0 = 
}

polyArea <- function(x,y){
  
  
}
