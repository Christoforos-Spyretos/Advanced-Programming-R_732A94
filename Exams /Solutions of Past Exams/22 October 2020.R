# Problem 1 

# a) To make it available for the user of the package the creator should write @export 
#    to the documentation area of roxygen2.

# b) 

###############################################################################

# Problem 2

create_robot_cleaner <- function(n, current_location){
  stopifnot( is.numeric(n), n %% 1 == 0, n > 1,
             is.numeric(current_location), current_location > 0)
  
  room <- data.frame(matrix(rbinom(n*n,1, p = ((n^2)/4)/(n*n))), nrow = n)
  colnames(room) <- c(1:length(n))
  
  clean <- sum(room)
  dirty <- n*n - clean
  
  n_dirty <- dirty
  
  battery <- 500 
  
  output <- list( "room" = room,
                  "clean" = clean, 
                  "dirty" = dirty, 
                  "battery" = battery, 
                  "n_dirty" = n_dirty,
                  "current_location" = current_location)
  
  class(output) <- "cleaner"
  return(output)
}

robot_cleaner <- create_robot_cleaner(n=10,current_location=c(1,1))

go_clean_tile <- function(cleaner){
  stopifnot( class(cleaner) == "cleaner")
  
  
  
  
}

################################################################################

# Problem 3

# a)

my_binary <- function(x){
  stopifnot(is.numeric(x), 
            is.matrix(x), 
            !any( x!=1 & x!=0))
  
  count <- 0
  
  for ( i in 1:nrow(x)){
    for ( j in 1:ncol(x)){
      if ( x[i,j] == 1){
        count <- count + 1
      }
    }
  }
  return(count)
}

# b) O(nrow*ncol)

# c) 

testthat::test_that("Compare my function with sum()",
                    {
                      x <- matrix(c(1,0,0,0,1,0,0,0,1) , nrow = 3)
                      m <- my_binary(x)
                      s <- sum(x)
                      testthat::expect_true(m == s)
                    })


