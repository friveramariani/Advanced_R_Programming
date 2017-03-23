# 1. factorial_loop: computes an integer's factorial using 
# looping, for example a "for" loop

factorial_loop <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to zero or greater than zero")
  }
  
  if(x == 0){
    return(1)
  } else{
    y <- 1
    for(i in 1:x){
      y <- y * ((1:x)[i])
    }
    return(y)
  }
}

# 2. factorial_reduce: computes the factorial using the the purrr package reduce() 
# function. 

factorial_reduce <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to zero or greater than zero")
  }
  
  # makes sure purrr package is installed
  if (!require('purrr', quietly = TRUE)) {
    stop('Please install the purrr package')
  }
  
  if(x == 0){
    return(1)
  } else{
    reduce(as.numeric(1:x), `*`) %>% return()
    }
}

# 3. Factorial_func: computes the factorial through recursion.

factorial_func <- function(x){
  
  if(x < 0){
    stop("Factorials can only be computed when x is equal to zero or greater than zero")
  }
  
  if (x == 0){
    return (1)
  } else{
    return (x * Factorial_func(x-1))
  }          
}

# 4. Factorial_mem: computes the factorial through memoization.

memoization <- function(){
  
  values <- 1
  
  factorial_mem <- function(x){
    
    if(x < 0){
      stop("Factorials can only be computed when x is equal to zero or greater than zero")
    }
    
    if (x == 0 | x == 1){
      return(1)
    } 
    
    if (length(values) < x){
      values <<- `length<-`(values, x)
    }
    
    if (!is.na(values[x])){
      return(values[x])
    }
    
    #calculate new values
    values[x] <<- x * factorial(x-1)
    values[x]
  }
  factorial_mem
}

factorial_mem <- memoization()

# benchmarking the four functions described above
library(microbenchmark)
microbenchmark(
  factorial_loop(1),
  factorial_reduce(1),
  factorial_func(1),
  factorial_mem(1)
)

microbenchmark(
  factorial_loop(10),
  factorial_reduce(10),
  factorial_func(10),
  factorial_mem(10)
  )

microbenchmark(
  factorial_loop(100),
  factorial_reduce(100),
  factorial_func(100),
  factorial_mem(100)
)
