##Code check for part 2 

single_sim <- function(p,q,k){
  zero_count = 0  ## Count the number of zeros 
  i = 0
  failed = FALSE
  while(i < k){
    probs = 
    p_prob = runif(1)
    q_prob = runif(1)
    if(failed == TRUE){zero_count = zero_count + 1}
    else{
      if (q_prob<q){
        failed = TRUE
        zero_count = zero_count + 1
      }
      else if (p_prob >= p){zero_count = zero_count + 1}
    }
    i = i + 1
  }
  return (zero_count)
}

multi_sim <- function(nreps,p,q,k){
  vals = replicate(nreps, single_sim(p,q,k))
  return(mean(vals))
  
}