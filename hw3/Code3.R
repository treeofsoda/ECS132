############################## PROBLEM A  ##############################

dmb <- function(x, np){
  '
  dmb: Returns the probability that W == x, i.e, the max was x and every othe value was less than or equal to x.
  
  Args:
      x: The value x should have
      np: matrix containing of shape k x 2. the first column is the number of trials, and the second column consists
           of the success probabilities for k examples.
  '
  nrow = dim(np)[1]
  output = 0
  for (i in 1:nrow){
    output = output +  dbinom(x,np[i,1], np[i,2]) * pmb(x, np[-i,])
  }
  return(output)
}

pmb <- function(x, np){
  '
  pmb: Returns the probability that every toss in np was less than x.
  
  Args:
       x: the maximum value
       np: matrix containing of shape k x 2. the first column is the number of trials, and the second column consists
           of the success probabilities for k examples.
  '
  if (class(np) == "numeric"){
    return(pbinom(x,np[1],np[2]))
  }
  nrow = dim(np)[1]
  output = 1
  for (i in 1:nrow){
    output = output* pbinom(x,np[i,1], np[i,2])
  }
  return(output)
}

qmb <-function(q,np){
  '
  qmb: Returns the c s.t. P(W<= c) = q
  
  Args:
      q: The probability
      np: matrix containing of shape k x 2. the first column is the number of trials, and the second column consists
           of the success probabilities for k examples.
  '
  output = apply(np,1, myQbinom, myQ = q )
  return(max(output))
}

myQbinom <- function(myQ,myX){
  '
  myQbinom: Find the qbinom given a vector in the format defined for the matrix np, i.e, first col = #Examples
            second col = Success Prob
  Args:
      myQ: The probability
      myX: Vector containing the # of examples and success prob in the first and second col respectively.
      
  '
  return(qbinom(myQ,myX[1],myX[2]))
}

rmb <- function(n, npr){
  '
  rmb: Random numbers generated from the max binomial distribution
  Args:
      n: number of random numbers to be generated
      npr: matrix containing of shape k x 2. the first column is the number of trials, and the second column consists
           of the success probabilities for k examples.
  '
  tmp <- as.matrix(runif(n))
  return(apply(tmp,1,qmb, np = npr))
}