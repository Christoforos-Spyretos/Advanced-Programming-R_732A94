# 23 of October 2018
# Advanced R Programming

# Problem 1

# a)

# b)

# c)

################################################################################

# Problem 2

# a)

build_Batory <- function(fuel,max_cargo_weight){
  stopifnot(is.numeric(fuel), length(fuel) == 1, fuel>0,
            is.numeric(max_cargo_weight), length(max_cargo_weight) == 1,
            max_cargo_weight > 0, fuel <= max_cargo_weight)
  
  name <- c(NA)
  weight <- c(NA)
  
  cargo <- data.frame( "name" = name, "weight" = weight)
  cargo_weight <- fuel 
  
  output <- list( "cargo" = cargo, "fuel" = fuel, "cargo_weight" = cargo_weight,
                  "max_cargo_weight" = max_cargo_weight)
  
  class(output) = "ship"
  return(output)
  
}

ORP_Batory <- build_Batory(fuel=1.5,max_cargo_weight=10)

equip_Batory <- function(ship, name, weight){
  stopifnot(class(ship) == "ship", 
            is.character(name), length(name) == 1,
            is.numeric(weight), length(weight) == 1, weight > 0)
  
  if( ship[["cargo_weight"]] + weight > ship[["max_cargo_weight"]]){
    stop("The ship is getting heavy. Do not add more stuff")
  }
  
  if( is.na(ship[["cargo"]]["name"])){
    ship[["cargo"]]["name"] <- name
    ship[["cargo"]]["weight"] <- weight
  } else{
    ship[["cargo"]] <- rbind(ship[["cargo"]], c(name,weight))
  }
  
  ship[["cargo_weight"]] <- ship[["cargo_weight"]] + weight
  ship[["cargo"]][["weight"]] <- as.numeric(ship[["cargo"]][["weight"]])
  
  return(ship)
}

for (i in 1:50) {
  ORP_Batory <-equip_Batory(ORP_Batory,"food",as.numeric(rexp(1, rate = 5)))
}

embark_sailor <- function(ship,weight){
  stopifnot(class(ship) == "ship",
            is.numeric(weight), length(weight) == 1, weight > 0)
  
  if( ship[["cargo_weight"]] + weight <= ship[["max_cargo_weight"]]){
    ship[["cargo"]] <- rbind(ship[["cargo"]],c("sailor",weight))
  } 
  
  index <- which(ship[["cargo"]][,"name"] != "sailor")
    
  if( length(index) == 0){
    stop("The ship has no more room for sailors")
    }
    
  remove_weight <- 0
      
    for( i in index){
      remove_weight <- ship[["cargo"]][,"weight"][i]
      if( remove_weight >= weight ){
        if( ship[["cargo_weight"]] - remove_weight < ship[["fuel"]]){
          stop( "Cannot remove more weight")
        } else {
          ship[["cargo"]] <- ship[["cargo"]][-c(i)]
          ship[["cargo_weight"]] <- ship[["cargo_weight"]] - remove_weight
        }
      }
    }
  ship[["cargo"]] <- rbind(ship[["cargo"]],c("sailor",weight))
  return(ship)
}

for (i in 1:10) {
  ORP_Batory <-embark_sailor(ORP_Batory,runif(1, min = 0.08, max = 0.2))
}

print.ship <- function(ship){
  stopifnot( class(ship) == "ship")
  number_of_sailors <- length(which(ship[["cargo"]][,"name"] == "sailor"))
  ship_cargo_weight <- ship[["cargo_weight"]]
  cat( "The number of sailors is", number_of_sailors, "and the cargo weight is", ship_cargo_weight)
}

print(ORP_Batory)

################################################################################

# Problem 3

# a) 

multi_matrix <- function (A,B){
  stopifnot(is.numeric(A), is.matrix(A),
            is.numeric(B), is.matrix(B),
            ncol(A) == nrow(B))
  
  V <- matrix(0, nrow =  nrow(A), ncol = ncol(B))
  
  for (i in 1:nrow(A)){
    for (j in 1:ncol(B)){
      for ( k in 1:ncol(A)){
        V[i,j] <- V[i,j] + A[i,k]*B[k,j]
      }
    }
  }
  return(V)
}

# b) n*p*m

# c)

testthat::test_that ( "Compare my function to %*%",
                       {
                         A <- matrix(rep(1:9), ncol = 3)
                         B <- matrix(rep(9:1), ncol = 3)
                         testthat::expect_true(all(A%*%B == multi_matrix(A,B)))
                       }
                       )

