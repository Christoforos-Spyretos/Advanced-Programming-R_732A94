create_robot_cleaner <- function( n, current_location){
  stopifnot(is.numeric(n), length(n) == 1, n %% 1 == 0,
            is.numeric(current_location), is.vector(current_location),
            length(current_location) == 2, current_location[1] < n, 
            current_location[2] < n)
  
  room <- matrix(sample(c(0,1), size=n*n, replace = TRUE, prob = c((n^2/4)/100, (n^2/4)/100)),
                  nrow=n)
  room <- as.data.frame(room)
  
  dirty_tiles <- sum(room)
  clean_tiles <- n*n - dirty_tiles
 
  battery <- 500
 
  output <- list( "room" = room,
                  "dirty_tiles" = dirty_tiles,
                  "clean_tiles" = clean_tiles,
                  "battery" = battery,
                  "size" = n,
                  "current_location" = current_location)
  
  class(output) <- "robot"
  return(output)
}

robot_cleaner <- create_robot_cleaner(n=10,current_location=c(1,1))

go_clean_tile <- function(robot){
  stopifnot( class(robot) == "robot")
  
  if ( robot[["battery"]] == 0){
    stop("The battery of the robot is empty.")
  }
  
  if ( all(robot[["room"]] == 0)){
    stop("The room is clean.")
  }
  
  dist_room <- matrix(0, nrow = robot[["size"]], ncol = robot[["size"]])
  dist_room <- as.data.frame(dist_room)
  
  for ( i in 1:robot[["size"]]){
    for ( j in 1:robot[["size"]]){
      dist_room[i,j] <- abs(robot[["current_location"]][1]-i) + abs(robot[["current_location"]][2]-j)
    }
  }
  
  for ( i in 0:max(dist_room)){
    index <- which( min(dist_room) + i == dist_room, arr.ind = T)
    for ( j in 1:index[1]){
      for ( k in 1:index[2]){
        if( robot[["room"]][j,k] == 1){
          robot[["room"]][j,k] <- 0
          robot[["current_location"]][1] <- robot[["current_location"]][j]
          robot[["current_location"]][2] <- robot[["current_location"]][k]
          robot[["battery"]] <- robot[["battery"]]- 1
          robot[["dirty_tiles"]] <- robot[["dirty_tiles"]] - 1
          robot[["clean_tiles"]] <- robot[["clean_tiles"]] + 1
          }
        }
      }
    } 
  return(robot)
}

for( i in 1:10){
  robot_cleaner <-go_clean_tile(robot_cleaner)
}




