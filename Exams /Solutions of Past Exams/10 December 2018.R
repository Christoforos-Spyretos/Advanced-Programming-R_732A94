

# Problem 2

# a)

build_library <- function(book_capacity){
  stopifnot(is.numeric(book_capacity), length(book_capacity) == 1, book_capacity > 0)
  
  title <- c(NA)
  author <- c(NA)
  id <- c(1:book_capacity)
  status <- factor( "available", levels = c("available", "borrowed"))
  
  df <- data.frame( "title" = title, "author" = author, "id" = id, "status" = status)
  
  n_b <- 0
  n_l <- 0
  
  output <- list( "books" = df, "n_b" = n_b, "n_l" = n_l)
  class(output) <- "library"
  return(output)
  
}

my_library <- build_library(book_capacity=5)

acquire_book <- function(library, title, author){
  stopifnot(class(library) == "library", 
            is.character(title), length(title) == 1,
            is.character(author), length(author) == 1)
  
  if( library$n_b +1 > nrow(library$books)){
    stop( "The library is full, cannot acquire more books")
  }
  
  index <- which(is.na(library[["books"]]["title"]))
  
  library[["books"]]["title"][index,] <- title
  library[["books"]]["author"][index,] <- author
  
  library$n_b <- library$n_b + 1
  
  return(library)
}

for (i in 1:5){
  my_library <-acquire_book(my_library,"John","Statistics")
}

borrow_book <- function(library, id){
  stopifnot( class(library) == "library",
             is.numeric(id), length(id) == 1, id > 0)
  
  if( id %in% library[["books"]][id]){
    stop( "The library does not have this book")
  }
  
  index <- which( library[["books"]]["id"] == id)
  
  if( library[["books"]][index, "status"] == "borrowed"){
    stop( "The book is not available")
  }
  
  library[["books"]][index, "status"] == "borrowed"
  
  library$n_b <- library$n_b - 1
  library$n_l <- library$n_l + 1
  
  return(library)
}

my_library <- borrow_book(my_library, 3)

return_book <- function(library, id){
  stopifnot( class(library) == "library",
             is.numeric(id), length(id) == 1, id > 0,
             id %in% library[["books"]][id])
  
  # if( id != library[["books"]][id]){
  #   stop( "The library does not have this book")
  # }
  
  index <- which( library[["books"]]["id"] == id)
  
  if( library[["books"]][index, "status"] == "available"){
    stop( "The book is not borrowed")
  }
  
  library[["books"]][index, "status"] == "available"
  
  library$n_b <- library$n_b + 1
  library$n_l <- library$n_l - 1
  
  return(library)
  
}

my_library <- return_book(my_library, 3)

# d) 

print.library <- function(library){
  stopifnot(class(library) == "library")
  number_of_books <- library[["n_b"]]
  number_of_loaned <- library[["n_l"]]
  cat( "The number of books is", number_of_books, "and the number of loaned 
       books is", number_of_loaned)
}

print(my_library)

################################################################################

# Problem 3

# a) 

quad <- function(A,x){
  stopifnot( is.matrix(A), is.numeric(x),
             nrow(A) == ncol(A),
             nrow(A) == length(x),
             ncol(A) == length(x))
  
  if( nrow(A) != ncol(A)){
    stop("The matrix is not square")
  }
  
  if(nrow(A) != length(x) | ncol(A) != length(x)){
    stop("The matrix and the vector must have the same dimensions")
  }
  
  if(nrow(A) != length(x) && ncol(A) != length(x)){
    stop("The matrix and the vector must have the same dimensions")
  }
  
  sum <- 0
  
  for ( i in 1:length(x)){
    for (j in 1:length(x)){
      sum <- sum + x[i]*A[i,j]*x[j]
      }
  }
  
  return(sum)
}

# b) The complexity of your solution in terms of the number of required 
#    multiplication operations is p^3.

# c)

testthat::test_that( "Compare my function to %*%",
                     {
                       A <- matrix(1:9, 3,3)
                       x <- c(1:3)
                       testthat::expect_true( quad(A,x) == x%*%A%*%x)
                     })

