threeinrow<- function(ntimesteps){
  consec <- 0 
  nwins <- 0
  wintimes <- 0
  startplay <- 0
  for (i in 1:ntimesteps){
    if(toss() == 'H'){
      consec <- consec + 1
      if(consec == 3){
        nwins <- nwins + 1
        wintimes <- wintimes + i - startplay
        consec <- 0 
        startplay <- i
      }
    }
    else consec <- 0 
  }
  return(wintimes/nwins)
}

toss <- function(){
  if(runif(1)>=0.5){return("H")}
  else{return("T")}
}