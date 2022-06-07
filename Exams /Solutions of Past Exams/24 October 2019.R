# Problem 2

# a) 

create_fridge <- function(number_users){
  stopifnot( is.numeric(number_users), 
             number_users > 0,
             length(number_users) ==1)
  
  list <- list()
  
  for ( i in 1:number_users){
    
    food_id <- c(1,2,3,4,5,2,3,1)
    eat <- c(2,2,2,2,2,2,2,2)
    compat <- c(2,1,3,4,2,1,3,2)
    
    df <- data.frame( "food_id" = food_id, "eat" = eat, "compat" = compat)
    list[[i]] <- df
  }
  
  class(list) <- "fridge"
  
  return(list)
}

modern_fridge <- create_fridge(number_users=2)

update_food_list <- function(fridge, user_id, food_id, update_max = 0, update_conflicting_food = 0){
  stopifnot(class(fridge) == "fridge",
            is.numeric(user_id), length(user_id) == 1,
            is.numeric(food_id), length(food_id) == 1,
            is.numeric(update_max), length(update_max) == 1, update_max >= 0,
            is.numeric(update_conflicting_food))
  
  if ( update_max != 0){
    fridge[[user_id]][[food_id,"eat"]] <- update_max
  }
 
  if( any(update_conflicting_food != 0)){
    add <- which(update_conflicting_food > 0)
    
    fridge[[user_id]]["compat"] <- paste(fridge[[user_id]]["compat"], 
                                         update_conflicting_food[add])
  
    remove <- which(update_conflicting_food < 0)
      fridge[[user_id]]["compat"] <- gsub(paste(abs(update_conflicting_food[remove]), 
                                                fridge[[user_id]][food_id,"compat"]))
  }
  return(modern_fridge)
}

user_id<-1
food_id<-1
update_max<-2
update_conflicting_food<-c(-2,3)

modern_fridge <-update_food_list(modern_fridge, user_id, food_id, update_max, update_conflicting_food)

request_food <- function(fridge, user_id, food_ids){
  stopifnot(class(fridge) == "fridge", is.numeric(user_id),
            length(user_id) == 1,user_id>0)
  
  user <- modern_fridge[[user_id]]
  
  if ( user$foodid == food_id){
    if ( user$eat < eat){
      modern_fridge[[user_id]][eat] <- eat
    } 
  } 
  
  if ( user$foodid =! food_id) {
    if( user$compat == compat){
      print ( "You cannot have the food")
    } else {
    modern_fridge[[user_id]]<- rbind( modern_fridge[[user_id]], c( food_ids, eat, compat))
    }
  }
  return(modern_fridge)
}

user_id<-1
food_ids<-1:3

modern_fridge <-request_food(modern_fridge,user_id,food_ids)

print.modern_fridge <- function(modern_fridge){
  stopifnot( class(fridge) == "fridge")
  
  
  
}

###############################################################################

# Problem 3 

# a)

find_factorial <- function(n){
  stopifnot(is.numeric(n), length(n) == 1, (n) %% 1 == 0)
  
  fact <- 1
  
  if( n<0 )
    print("The factorial of a negative does not exist")
  else if( n==0 )
    print(fact)
  else{
    for( i in 1:n){
      fact <- fact*i
    }
  print(fact)
  }
}

# b) The complexity of your solution in terms of the number of required 
#    multiplication operations is n!.

# c) 

stirling <- function(n){
  stopifnot(is.numeric(n), n>=0)
  
  strl <- (sqrt(2*pi*n))*((n/exp(1))^(n))
  
  return(strl)
}

tolerance <- function(n){
  stopifnot(is.numeric(n), n>=0)
  
  low_limit <- sqrt(2*pi)*(n^(n+0.5))*((exp(1))^(-n))
  upper_limit <- exp(1)*(n^(n+0.5))*((exp(1))^(-n))
  
  return(c(low_limit,upper_limit))                               
}

testthat::test_that( "Compare my factorial function to Stirling's formula",
                     {
                       n <- 4
                       ff <- find_factorial(n)
                       st <- stirling(n)
                       tol <- tolerance(n)
                       testthat:: expect_true( ff >= tol[1] && ff <= tol[2])
                     })








  
  
  



