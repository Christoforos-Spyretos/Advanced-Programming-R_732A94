create_robot_cleaner <- function(size, init){
  stopifnot(is.numeric(size), length(size)==1, size > 0, is.vector(init),
            length(init)==2, is.numeric(init), init[1] < size, init[2] < size)
  
  tiles <- rep(0, times = size^2)
  
  #this is the amount of tiles that can be clean, so we sample one
  clean <- sample(ceiling(size^2/4):(size^2-ceiling(size^2/4)), 1)
  dirty <- size^2 - clean
  tiles[sample(1:(size^2), dirty)] <- 1
  
  tiles_info <- cbind(1:size^2,expand.grid(1:size,1:size),tiles, NA)
  colnames(tiles_info) <- c("ID","i_var","j_var","dirty","distance")
  
  output <- list("room_map"=tiles_info,
                 "number_dirty"=dirty,
                 "location"=init,
                 "battery"=500,
                 "size"=size)
  
  class(output) <- "cleaner"
  return(output)
  
}

robot_cleaner <- create_robot_cleaner(size = 10, init = c(8,1))


## b

go_clean_tile <- function(rob){
  stopifnot(class(rob)=="cleaner")
  
  if(length(which(rob[["room_map"]]["dirty"]==1)) == 0){
    stop("All tiles are clean! No more cleaning necessary.")
  }
  
  if(rob[["battery"]] == 0){
    stop("Robot battery is empty!")
  }
  
  i_init <- rob[["location"]][1]
  j_init <- rob[["location"]][2]
  
  dist <- matrix(0, nrow=rob[["size"]], ncol=rob[["size"]])
  for(i in 1:rob[["size"]]){
    for(j in 1:rob[["size"]]){
      dist[i,j] <- abs(i-i_init)+abs(j-j_init)
    }
  }
  rob[["room_map"]]["distance"] <- as.vector(dist)
  
  # we found the one we are cleaning
  dirty_new <- rob[["room_map"]][which(rob[["room_map"]]["dirty"]==1),]
  ID_chosen <- dirty_new[which.min(dirty_new[,"distance"]),"ID"]
  
  # now updating everything in rob object
  rob[["room_map"]][which(rob[["room_map"]]["ID"]==ID_chosen),"dirty"] <- 0 
  
  rob[["location"]][1] <- rob[["room_map"]][which(rob[["room_map"]]["ID"]==ID_chosen),"i_var"]
  rob[["location"]][2] <- rob[["room_map"]][which(rob[["room_map"]]["ID"]==ID_chosen),"j_var"]
  
  rob[["battery"]] <- rob[["battery"]] - rob[["room_map"]][which(rob[["room_map"]]["ID"]==ID_chosen),"distance"]
  
  rob[["number_dirty"]] <- rob[["number_dirty"]] - 1
  
  return(rob)
}

for(i in 1:20){
  robot_cleaner <- go_clean_tile(robot_cleaner)
}

## c

print.cleaner <- function(rob){
  
  stopifnot(class(rob)=="cleaner")
  
  clean_matrix <- matrix(t(rob[["room_map"]]["dirty"]), 
                         nrow = rob[["size"]],
                         ncol = rob[["size"]])
  clean_matrix[rob[["location"]][1],rob[["location"]][2]] <- 8
  
  cat("All number 1 show that a tile is dirty and 0 that a tile is clean. Moreover the robot is standing where the number 8 is displayed in the picture. That is what the room currently looks like:\n")
  print(clean_matrix)
  cat("This means that", rob[["number_dirty"]], "out of" , rob[["size"]]^2, "tiles are dirty.\n")
  cat("The current battery power of the robot is", rob[["battery"]],".")
}

print(robot_cleaner)