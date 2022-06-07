name = "Christoforos Spyretos"
liuid = "chrsp415"


library(markmyassignment)
lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml" 
set_assignment(lab_path)



sheldon_game <- function(player1, player2)
  {choices <- c("rock", "lizard", "spock", "scissors", "paper") 
  stopifnot(player1 %in% choices, player2 %in% choices)
  choice_1 <- which(choices %in% player1)
  choice_2 <- which(choices %in% player2)
  if (((choice_1 + 1) %% 5) == choice_2 | ((choice_1 + 3) %% 5) == choice_2){
    return( "Player 1 wins!")
    }else if ( choice_1 == choice_2 ){
     return( "Draw!")  
    }else( "Player 2 wins")
}

sheldon_game("lizard", "spock")
sheldon_game("rock", "paper")


my_moving_median <- function(x, n, ...)
  {stopifnot(is.numeric(x), is.numeric(n))
   v1 <- c()
   for (i in 1:(length(x)-n)){
   v1 <- c(v1, median(x[i:(i+n)], na.rm = ...))
   }
   return(v1)
}
my_moving_median(1:10, 2)
my_moving_median(5:15, 4)
my_moving_median(c(5,1,2,NA,2,5,6,8,9,9),2)
my_moving_median(c(5,1,2,NA,2,5,6,8,9,9), 2, na.rm = TRUE)


for_mult_table <- function(from, to){
  {if(is.numeric(from) & is.numeric(to)){
    m1 <- matrix(,nrow = length(from:to), ncol = length(from:to))
    x <- from
    for (i in 1:(length(from:to))){
         y <- from
         for (j in 1:(length(from:to))){
         m1[i,j] <- x*y
         y <- y+1
         }
         x <- x+1
    } 
   }else {stop()
   }
  }
  rownames(m1) <- c(from:to)
  colnames(m1) <- c(from:to)
  return(m1)
}
  
for_mult_table(1, 5)
for_mult_table(10, 12)

find_cumsum <- function(x, find_sum)
  {stopifnot(is.numeric(x), is.numeric(find_sum))
   s <- 0
   i <- 1
   while (( i <= length(x)) & (s <= find_sum)){
     s <- s+x[i]
     i <- i+1
   }
   return(s)  
}
find_cumsum(x=1:100, find_sum=500)


while_mult_table <- function(from, to)
  {if(is.numeric(from) & is.numeric(to)){
   m1 <- matrix(,nrow = length(from:to), ncol = length(from:to))
   x <- from
   i <- 1
   while (x <= to){
     y <- from
     j <- 1
     while (y <= to){
       m1[i,j] <- x*y 
       y <- y+1
       j <- j+1
     }
     x <- x+1
     i <- i+1
   }
  } else {stop()
  }
  rownames(m1) <- c(from:to)
  colnames(m1) <- c(from:to)
  return(m1)
}
while_mult_table(from = 3, to = 5)
while_mult_table(from = 7, to = 12)


repeat_find_cumsum <- function(x,find_sum)
  {stopifnot(is.numeric(x), is.numeric(find_sum))
   s <- 0
   i <- 1
   repeat{
     s <- s+x[i]
     i <- i+1
     if (( i > length(x)) | (s > find_sum)){
       break
     }
   }
   return(s)  
}

repeat_find_cumsum( 1:100, 500)
repeat_find_cumsum( 1:10, 1000)


repeat_my_moving_median <- function(x,n,...)
  {stopifnot(is.numeric(x), is.numeric(n))
   v1 <- c()
   i <- 1
   repeat{
      v1 <- c(v1, median(x[i:(i+n)], na.rm = ...))
      i <- i+1
      if (i > (length(x)-n)){
        break()
     }
   }
   return(v1) 
}

repeat_my_moving_median( 1:10, 2)
repeat_my_moving_median( 5:15, 4)
repeat_my_moving_median( c(5,1,2,NA,2,5,6,8,9,9), 2)
repeat_my_moving_median(c(5,1,2,NA,2,5,6,8,9,9), 2, na.rm = TRUE)


in_environment <- function(env)
  { x = ls(env)
    x = as.character(x)
    return(x)
}

env <- search()[length(search())]
funs <- in_environment(env)
funs[1:5]


cov <- function(X)
  {stopifnot(is.data.frame(X))
   v1 <- c(unlist(lapply(X, function(x) sd(x) / mean(x))))
   return(v1)
}
data(iris)
cov(X = iris[1:4])
cov(X = iris[3:4])
cov(X = 1)

moment <- function(i)
  {stopifnot(is.numeric(i))
    function(x) return(mean((x - mean(x)) ** i))
}

m1 <- moment(1) 
m2 <- moment(2)
m1(1:100)
m2(1:100)






