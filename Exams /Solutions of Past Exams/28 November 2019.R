# Solutions 28 November 2019

################################################################################

#### Problem 1

################################################################################

### Problem 2

# a)

create_wardrobe <- function(max_number_clothes){
  stopifnot( is.numeric(max_number_clothes), 
             length(max_number_clothes) == 1,
             max_number_clothes >0 )
  
  id <- 1:max_number_clothes
  name <- c(NA)
  condition <- c(NA)
  
  output <- list( "id" = id,
                  "name" = name,
                  "condition" = condition,
                  "capacity" = max_number_clothes)
  
  class(output) <- "clothes"
  return(output)
  
}

future_wardrobe <- create_wardrobe(max_number_clothes=5)

# b)

add_to_wardrobe <- function(clothes, name, weather){
  stopifnot( class(clothes) == "clothes",
             is.character(name), 
             is.character(weather))
  
  if ( length(clothes[["name"]]) == clothes[["capacity"]]){
    stop("The wardrobe is full of clothes.")
  }
  
  if ( all(is.na(clothes[["name"]]))){
    clothes[["name"]] <- name
    clothes[["condition"]] <- weather
  } else if(length(name) < length(clothes[["id"]])){
    clothes[["name"]] <- append(clothes[["name"]], name)
    clothes[["condition"]] <- append(clothes[["condition"]], weather)
  } 
  
  return(clothes)
  
}

for (i in 1:5) {
  future_wardrobe <-add_to_wardrobe(future_wardrobe,"raincoat","rain")
}


obtain_clothes <- function(clothes, weather, number_of_clothes){
  stopifnot( class(clothes) == "clothes",
             is.character(weather), length(weather) == 1,
             is.numeric(number_of_clothes),length(number_of_clothes) == 1, number_of_clothes > 0)
  
  index <- which( clothes[["condition"]] == weather)
  
  if ( number_of_clothes <= length(index)){
    for (i in index){
      cat("The type of clothes are", tail(clothes[["name"]][i], n =1), "and the amount is", length(index))
      clothes[["name"]] <- clothes[["name"]][-c(index)]
      clothes[["condition"]] <- clothes[["condition"]][-c(index)]
      clothes[["capacity"]] <- clothes[["capacity"]] - 1
    }
  } else{
    print("The wardrobe does not have that amount of clothes")
  }

  return(clothes)
}

future_wardrobe <-obtain_clothes(future_wardrobe,"rain",5)

print.clothes <- function(clothes){
  stopifnot( class(clothes) == "clothes")
  
  number <- clothes[["capacity"]]
  
  cat( "The total number of clothes that the wardrobe has is right now is", number, ".")
}

print(future_wardrobe)
################################################################################

# Problem 3

my_matrix <- function(value, dimension){
  stopifnot( is.numeric(value), length(value) ==1,
             is.numeric(dimension), length(dimension) == 1 , dimension > 1,
             dimension %% 1 == 0)
  
  A <- c()
  
  for (i in 1:dimension){
    c <- c(rep( value, times = dimension))
    A <- rbind(A,c)
    }
  return(A)
}

# dimension

testthat::test_that(" Compare my function to matrix()",{
   m1 <- my_matrix(5,3)
   m2 <- matrix(5, 3, 3)
   testthat::expect_true(all( m1 == m2))
})



