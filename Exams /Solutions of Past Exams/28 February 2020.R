# 28 February 2020

################################################################################

# Problem 2

create_toy_store <- function( max_number_toys){
  stopifnot( is.numeric(max_number_toys), 
             length(max_number_toys) == 1,
             max_number_toys > 0)
  
  id <- 1:max_number_toys
  name <- c(NA)
  price <- c(NA)
  min_age <- c(NA)
  max_age <- c(NA)
  
  output <- list( "id" = id, "name" = name, "price" = price, "min_age" = min_age,
                  "max_age" = max_age, "capacity" = max_number_toys)
  
  class(output) <- "toys"
  return(output)
  
}

toy_store <- create_toy_store(max_number_toys=5)

add_to_toy_store <- function(toys, name, price, min, max){
  stopifnot( class(toys) == "toys", 
             is.character(name), length(name) == 1,
             is.numeric(price),length(price) ==1 ,price > 0 ,
             is.numeric(min),length(min) == 1, min > 0,
             is.numeric(max), length(max) == 1, max > 0,
             min < max)
  
  if (length(toys[["name"]]) == toys[["capacity"]]) { 
    stop("The storage is full.")
  }
  
  if(is.na(toys[["name"]])){
    toys[["name"]] <- name
    toys[["price"]] <- price
    toys[["min_age"]] <- min
    toys[["max_age"]] <- max
    } else {
    toys[["name"]] <- append(toys[["name"]], name)
    toys[["price"]] <- append(toys[[ "price"]], price)
    toys[["min_age"]] <- append(toys[["min_age"]], min)
    toys[["max_age"]] <- append(toys[["max_age"]], max)
    }
  
  return(toys)
}

for (i in 1:5) {
  toy_store <-add_to_toy_store(toy_store,"car",100,3,10)
}

recommend_toy <- function(toys, age, money){
  stopifnot(class(toys) == "toys",
            is.numeric(age), length(age) == 1, age > 0,
            is.numeric(money), length(money) == 1, money > 0)

  index <- which( toys[["min_age"]] <= age & age <= toys[["max_age"]]  & toys[["price"]] <= money)
  
  if (length(index) == 1){
    toys[["id"]] <- toys[["id"]][-c(index)]
    toys[["name"]] <- toys[["name"]][-c(index)]
    toys[["price"]] <- toys[["price"]][-c(index)]
    toys[["min_age"]] <- toys[["min_age"]][-c(index)]
    toys[["max_age"]] <- toys[["max_age"]][-c(index)]
    toys[["capacity"]] <- toys[["capacity"]] - 1
  } else if(length(index) >1){
      cat( "The recommended toys are", toys[["names"]][index], ".")
    } 
  } else{
    print( "There are no toys recommended")
  }
  
  # for( i in 1:length(toys[["id"]])){
  #   if ( toys[["price"]][i] > money){
  #     stop( "The toy is too expensive to purchase")
  #   }
  #   
  #   if ( toys[["min_age"]][i] > age){
  #     stop("The age of the child is not appropiate for this toy. The child is too young.")
  #   }
  #   
  #   if( toys[["max_age"]][i] < age){
  #     stop("The age of the child is not appropiate for this toy. The child is too old.")
  #   }
  # }
  
  return(toys)
}
  
toy_store <-recommend_toy(toy_store,6,90)  

print.toys <- function(toys){
  stopifnot(class(toys) == "toys")
  
  number_of_toys <- toys[["capacity"]]
  
  cat("The available number of toys inside the store are", number_of_toys,".")
  
  return(toys)
}
  
print(toy_store)  
  
###############################################################################

# Problem 3

# a)

my_exp <- function(value,depth){
  stopifnot(is.numeric(value), length(value) == 1,
            is.numeric(depth), length(depth) == 1)
    
    s <- 0
    
    for ( i in 0:depth){
      s <- s + (value^(i))/factorial(i)
    }
  return(s)
}

# c)

testthat::test_that("Compare my function to exp",{
  testthat::expect_true( round(my_exp(2,10)) == round(exp(2)))
})  
  
  

