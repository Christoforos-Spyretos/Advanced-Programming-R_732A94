name = "Christoforos Spyretos"
liuid = "chrsp415"


library(markmyassignment)
lab_path<-"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)


my_num_vector <- function()
  {return(c(log10(11),cos(pi/5),exp(pi/3),((1173%%7)/19)))
}
my_num_vector()


filter_my_vector <- function (x, leq)
  {replace(x,x>=leq,NA)
}
v1 <- c(2,9,2,4,102)
filter_my_vector(v1,4)  


dot_prod <- function(a,b)
  {return(sum(a*b))
}
dot_prod(a<-c(3,1,12,2,4), b<-c(1,2,3,4,5))                      


approx_e <- function(N)
  {return(sum(1/ factorial(0:N)))
}
approx_e(2)
approx_e(3)
approx_e(4)
approx_e(5)
approx_e(6)
approx_e(7)
approx_e(8)
approx_e(9)
approx_e(10)
exp(1)
#We get a better approach of e when N is larger.

my_magic_matrix <- function()
  {return(matrix(c(4,9,2,3,5,7,8,1,6), nrow=3, byrow=TRUE))
}
my_magic_matrix()


calculate_elements <- function(A)
  {return(length(A))
}
calculate_elements(my_magic_matrix())


row_to_zero <- function(A,i)
  {(A[i,]<-0)
  return(A)
}
row_to_zero(my_magic_matrix(),3)
row_to_zero(my_magic_matrix(),2)
row_to_zero(my_magic_matrix(),1)


add_elements_to_matrix <- function( A, x, i, j)
  {(A[i,j] <- A[i,j]+x)
  return(A)
} 
add_elements_to_matrix(my_magic_matrix(),2,1,1)
add_elements_to_matrix(my_magic_matrix(), -2, 1:3, 2:3)


my_magic_list <- function()
  {return(list(info="my own list", my_num_vector(), my_magic_matrix()))
}
my_magic_list()   


change_info <- function(x,text)
  {x$info <- text
  return(x)
}
change_info(my_magic_list(), "Some new info")


add_note <- function(x,note)
  {x$note <- note
   return(x)
}
add_note(my_magic_list(), "This is a magic list")


sum_numeric_parts <- function(x)
  {return(sum(as.numeric(unlist(x)), na.rm=TRUE))
}  
sum_numeric_parts(my_magic_list())


my_data.frame <-function()
  {return(data.frame( "id" = c(1, 2, 3), 
                      "name" = c( "John", "Lisa", "Azra"),
                      "income" = c(7.30, 0.00, 15.21),
                      "rich" = c( FALSE, FALSE, TRUE )))
}
my_data.frame()


sort_head <- function( df, var.name, n)
  {return(head(df[order(df[,var.name], decreasing = TRUE),], n=n))
}
data(iris)
sort_head( df = iris, var.name = "Petal.Length", n =5)


add_median_variable <- function(df, j)
  {x = median(df[[j]]) 
   df$compared_to_median <- c( ifelse( x == df[[j]], "Median", ifelse( x>= df[[j]], "Smaller" , "Greater")))
   return(df)
}
add_median_variable(faithful,1)
add_median_variable(faithful,2)
head(add_median_variable(df = faithful, 1))
tail(add_median_variable(df = faithful, 2))


analyze_columns <- function( df, j)
    {my_list <- list(c( "mean" = mean(df[[j[1]]]), "median" = median(df[[j[1]]]), "sd" = sd(df[[j[1]]])), 
     c("mean" = mean(df[[j[2]]]), "median" = median(df[[j[2]]]), "sd" = sd(df[[j[2]]])),
     cor(df[c(j[1], j[2])]))
     names(my_list) <- c( colnames(df)[j] , "correlation_matrix")
     return(my_list)
}
data(faithful)
analyze_columns(faithful, 1:2)
data(iris)
analyze_columns(iris, c(1,3))
analyze_columns(iris, c(4,1))




