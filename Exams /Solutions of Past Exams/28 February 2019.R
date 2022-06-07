# Problem 3

# a) 

build_data_object <- function(p){
  stopifnot(is.numeric(p), p >0)
  
  cluster_id <- c(NA)
  cluster_center <- c(NA)
  number_points <- c(NA)
  
  clusters <- data.frame( "cluster_id" = cluster_id,
                          "cluster_center" = cluster_id,
                          "number_points" = number_points)
  
  observation_id <- c(NA)
  observation_value <- c(NA)
  cluster_id <- c(NA)
  distance <- c(NA)
  
  observations <- data.frame( "observation_id" = observation_id,
                              "observation_value" = observation_value,
                              "cluster_id" = cluster_id,
                              "distance" = distance)
  
  lp <- function(x,y,p){
    return((abs(x-y))^p)
  }
  
  number_clusters <- 0
  number_observations <- 0
  
  output <- list ( "clusters" = clusters, 
                   "observations" = observations, 
                   "lp" = lp, 
                   "number_clusters" = number_clusters,
                   "number_observations" = number_observations)
  
  class(output) <- "data_obj"
  
  return(output)
}

data_obj <- build_data_object(p=2)

# b)

create_cluster <- function(data_obj, center){
  stopifnot(class(data_obj) == "data_obj", 
            is.numeric(center),length(center) == 1)
  
  if (center %in% data_obj$cluster$center_cluster){
    stop( "The cluster does exists.")
  }
  
  data_obj$number_clusters <- data_obj$number_cluster + 1
  
  if(is.na(data_obj[["clusters"]]["cluster_id"])){
    data_obj[["clusters"]]["cluster_id"] <- 1
    data_obj[["clusters"]]["cluster_center"] <- center
    data_obj[["clusters"]]["number_points"] <- 0
  } else{
    data_obj$clusters <- rbind(data_obj$clusters, c( nrow(data_obj$clusters) + 1, center, 0))
  }
  
  return(data_obj)
}
for (i in 1:10) {
  data_obj <-create_cluster(data_obj, 2*i)
}

# c)

add_observation <- function(data_obj,observation){
  stopifnot(class(data_obj) == "data_obj",
            is.numeric(observation), length(observation) == 1)
  
  min <- min(data_obj$lp(data_obj$clusters$cluster_center, observation, 2))
  index <- which.min(data_obj$lp(data_obj$clusters$cluster_center, observation, 2))
  
  if(is.na(data_obj$observations$observation_id)){
    data_obj[["observations"]]["observation_id"] <- 1
    data_obj[["observations"]]["observation_value"] <- observation
    data_obj[["observations"]]["cluster_id"] <- index
    data_obj[["observations"]]["distance"] <- min
  } else{
    data_obj$observations <- rbind(data_obj$observations, c(nrow(data_obj$observations)+1, 
                                                            observation, index, min))
  }
  
  data_obj$clusters$number_points<- data_obj$clusters$number_points + 1
  
  data_obj$number_observations <- data_obj$number_observations + 1
  
  return(data_obj)
}

for (i in 1:10){
  data_obj <-add_observation(data_obj,2*i)
}

# d)

print.data_obj <- function(data_obj){
  stopifnot(class(data_obj) == "data_obj")
  
  number_of_clusters <- data_obj$number_clusters
  number_of_observations <- data_obj$number_observations
  cat("The number of clusters is", number_of_clusters, "and the number of observations is",
      number_of_observations,".")
}

print(data_obj)

###############################################################################

# Problem 3

# a)

find_binomial <- function(n,k){
  stopifnot( is.numeric(n), length(n) == 1, n >=0, (n) %% 1 == 0,
             is.numeric(k),length(k) == 1, k>=0, (k) %% 1 == 0)
  
  b <- 1
  
  fact <- function(z){
    f <- 1
    for( i in 1:z){
      f <- f*i
    }
    return(f)
  }
  
  if( n < 0 | k < 0) {
    print ("The n or k is negative. It must be positive")
  } else if ( n < 0 && k < 0){
    print ("The n and K are negatives. They must be positive")
  } else if ( n == 0) {
    b <- 0
    print(b)
  } else if ( k == 0 | k == n ) {
    print (b)
  }else {
    b <- fact(n) / (fact((n-k))*fact(k))
    return(b)
  }
}
 

# b) The complexity of your solution in terms of the number of required 
#    multiplication operations is n!+k!+(n-k)!.

# c)

testthat:: test_that( "Compare my function to choose()",
                      {
                        n <- 4
                        k<-2
                        testthat::expect_true(find_binomial(n,k) == choose(n,k))
                      })


