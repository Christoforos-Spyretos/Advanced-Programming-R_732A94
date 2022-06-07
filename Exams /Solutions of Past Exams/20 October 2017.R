build_mgb <- function(name, speed){
  stopifnot( is.character(name), length(name) == 1,
             is.numeric(speed), length(speed) == 1, speed > 0)
  
  weight_cargo <- c(NA)
  travel_time <- c(NA)
  
  runs <- data.frame( "weight_cargo" = weight_cargo, "travel_time" = travel_time)
  
  status <- "in_service"
  
  output <- list( "name" = name, "speed" = speed, "runs" = runs, "status" = status)
  class(output) <- "mgb"
  return(output)
  
}

MGB_504 <- build_mgb(name="Hopewell",speed=46)

simulate_home_run <- function(mgb){
  stopifnot( class(mgb) == "mgb")
  
  if(mgb[["status"]] == "sunk" ){
    stop( "The ship has sunk.")
  }
  
  weight <- rnorm( 1, mean = 40, sd = 2)
  
  r <- 1/((weight)*((900/mgb[["speed"]])/250))
  
  time <- rexp(1, rate = r)
  
  mgb[["runs"]][["weight_cargo"]] <- weight
  mgb[["runs"]][["travel_time"]] <- time
  
  if ( mgb[["runs"]]["travel_time"] <= 10){
    print("The ship has sunk.")
    mgb[["status"]] <- "sunk"
  }
  
  return(mgb)
}

MGB_504 <-simulate_home_run(MGB_504)

for (i in 1:100) {
  MGB_504 <-simulate_home_run(MGB_504) 
}

library(ggplot2)

plot.mgb <- function(mgb){
  stopifnot(class(mgb) == "mgb")
  ggplot(data = mgb[["runs"]], aes(x = travel_time, y = weight_cargo)) + 
    geom_point()
}

plot(MGB_504)

################################################################################

# Problem 3

# a)

find_max_value <- function(x){
  stopifnot(is.vector(x), is.numeric(x))
  
  max_value <- 0
  
  for ( i in x){
    if ( i > max_value){
      max_value <- i
    }
  }
  
  index <- tail(which( x == max_value), n=1)
  position <- index
  
  output <- list( "max_value" = max_value, "position" = position)
  
  return(output)
}

x<-c(1,2,3,56,4,5)
find_max_value(x)

x<-c(1,2,3,56,4,56,5)
find_max_value(x)

# b) The complexity is the length of the vector.

# c)

find_max_value2 <- function(x){
  stopifnot(is.vector(x), is.numeric(x))
  
  max_value <- (-1)*min(-x)
  
  position <- tail(which( max_value == x), n =1)
  
  output <- list( "max_value" = max_value, "position" = position)

  return(output)
}

x<-c(1,2,3,56,4,5)
find_max_value2(x)

x<-c(1,2,3,56,4,56,5)
find_max_value2(x)

testthat::test_that("Compare my functions",{
                    x<-c(1,2,3,56,4,5)
                    y<-c(1,2,3,56,4,56,5)
                    testthat:: expect_true(find_max_value(x)$max_value == find_max_value2(x)$max_value)
                    testthat:: expect_true(find_max_value(y)$max_value == find_max_value2(y)$max_value)
                    })

