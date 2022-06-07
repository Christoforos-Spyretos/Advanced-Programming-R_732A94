n <- 4
current_location <- c(1,1)

room <- matrix(sample(c(0,1), size=n*n, replace = TRUE, prob = c((n^2/4)/100, (n^2/4)/100)),
               nrow=n)

room <- as.data.frame(room)

dirty_tiles <- sum(room)

clean_tiles <- n*n - dirty_tiles

battery <- 20

dist_room <- matrix(0, nrow = n, ncol = n)

dist_room <- as.data.frame(dist_room)

for ( i in 1:n){
  for ( j in 1:n){
    dist_room[i,j] <- abs(current_location[1]-i) + 
      abs(current_location[2]-j)
  }
}

for ( i in 0:max(dist_room)){
  index <- which( min(dist_room) + i == dist_room, arr.ind = T)
    for ( j in 1:index[1]){
      for ( k in 1:index[2]){
        if( room[j,k] == 1){
          room[j,k] <- 0
          current_location[1] <- current_location[j]
          current_location[2] <- current_location[k]
          battery <- battery - 1
        }
      }
    }
  } 
}
