
# Problem 2

create_household <- function(address,number_of_devices){
  stopifnot(is.character(address),
            is.numeric(number_of_devices), number_of_devices > 0,
            number_of_devices %% 1 == 0)
  
  id <- 1:number_of_devices
  time <- rep( 0 , times = number_of_devices)
  connection_rate <- runif( number_of_devices, min = 0, max = 1)
  
  df <- data.frame( "id" = id,
                    "time" = time,
                    "connection_rate" = connection_rate)
  
  output <- list( "df" = df)
  
  class(output) <- "household"
  return(output)
  
}

household_1 <- create_household(address="Circle Drive 3",number_of_devices=5)

simulate_internet_usage <- function(household, number_of_days){
  stopifnot( class(household) == "household",
             is.numeric(number_of_days), number_of_days > 1,
             number_of_days %% 1 == 0)
  
  
  for (i in 1:number_of_days){
    for (j in 1:length(household$df$id)){
      household$df$time[j] <- household$df$time[j] + rexp(1, household$df$connection_rate[j])
    }
  }
  
  return(household)
}

household_1 <- simulate_internet_usage(household_1,100)

library(ggplot2)
plot.household <- function(household){
  stopifnot( class(household) == "household")
  
  ggplot(household$df, aes( x = id, y = time)) + geom_point()
  
}

plot(household_1)

################################################################################

# Problem 3

# a)

scalar_product <- function(x,y){
  stopifnot(is.numeric(x), is.numeric(y),
            length(x) == length(y))
  
  s <- 0
  
  for (i in 1:length(x)){
    
    s <- s + x[i]*y[i]
    
  }
  
  return(s)
}

x<-1:4
y<-5:8
scalar_product(x,y)

# b) length(x)

# c)

testthat::test_that(" Compare my scalar_product with %*%",
                    {x<-1:4
                     y<-5:8
                     testthat::expect_true( scalar_product(x,y) == x%*%y)
                    })









