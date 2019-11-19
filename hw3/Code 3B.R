############################## PROBLEM B  ##############################

mc <- function(n, q, p, r) {
  tm <- matrix(rep(0, (n+q+1)^2), nrow=n+q+1)
                                          # Create (n+q+1)^2 square matrix. n is dominent. 
                                          # in other words, lines of states 1, 2, 3, ..., 
                                          #            represent (0,0), (1,0), (2,0), ...,
                                          #                      (n,0), (n,1), (n,2), ..., 
                                          #                      (n,q-2), (n,q-1),(n,q) respectively.
  
  for (n_rows in 0:n) {                   # fill in posibilities by rows. first for loop is first n+1 statements: 
                                         # (0,0), (1,0), (2,0), ..., (n,0)  in other words, top-left part of matrix
    for (n_cols in 0:n) {                 # transitting to n+1 states of (0,0), (1,0), (2,0), ..., (n,0)
      if        (n_rows == n_cols-1) {
        tm[n_rows+1, n_cols+1] <- (1-p)^n_rows*r
      } else if (n_rows == n_cols) {
        if (n_rows == 0) {
          tm[n_rows+1, n_cols+1] <- 1-r
        } else {
          tm[n_rows+1, n_cols+1] <- (1-r)*(1-p)^n_rows + n_rows*r*(1-p)^(n_rows-1)*p
        }
      } else if (n_rows > n_cols) {
        if (n_cols == 0) {
          tm[n_rows+1, n_cols+1] <- (1-r)*p^(n_rows-n_cols)
        } else {
          tm[n_rows+1, n_cols+1] <- choose(n_rows,n_cols)*(1-r)*(1-p)^n_cols*p^(n_rows-n_cols) + choose(n_rows,n_cols-1)*r*(1-p)^(n_cols-1)*p^(n_rows-n_cols+1)
        }
      }
    }
    
    for (q_cols in 1:q) {                # top-right part of matrix
      if (n_rows == n && q_cols == 1) {
        tm[n_rows+1, n_rows+2] <- r*(1-p)^n_rows
      }
    }
  }
  
  for (q_rows in 1:q) {
    for (n_cols in 0:n) {                # bottom-left part of matrix
      if (q_rows+n-n_cols == n) {
        tm[n+q_rows+1,n_cols+1] <- (1-r)*p^n
      } else if (q_rows+n-n_cols < n) {
        tm[n+q_rows+1,n_cols+1] <- r*choose(n,q_rows+n-n_cols+1)*p^(q_rows+n-n_cols+1)*(1-p)^(n_cols-q_rows-1) + 
                                   (1-r)*choose(n,q_rows+n-n_cols)*p^(q_rows+n-n_cols)*(1-p)^(n_cols-q_rows)
      }
    }
    
    for (q_cols in 1:q) {                # bottom-right part of matrix
      if (q_rows == q_cols - 1) {
        tm[n+q_rows+1,n+q_cols+1] <- r*(1-p)^n
      } else if (q_rows >= q_cols && q_rows - q_cols <= n) {
        tm[n+q_rows+1,n+q_cols+1] <- r*choose(n,q_rows-q_cols+1)*p^(q_rows-q_cols+1)*(1-p)^(n-(q_rows-q_cols+1)) + 
                                     (1-r)*choose(n,q_rows-q_cols)*p^(q_rows-q_cols)*(1-p)^(n-(q_rows-q_cols))
        if (q_rows == q && q_cols == q) {
          tm[n+q+1,n+q+1] <- tm[n+q+1,n+q+1] + r*(1-p)^n    # Here a dropped call occur
        }
      } else if (q_rows - q_cols == n) {
        tm[n+q_rows+1,n+q_cols+1] <- (1-r)*p^n
      }
    }
  }
  tm
}
