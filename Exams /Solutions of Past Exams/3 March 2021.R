create_memory_manager <- function(n, trigger_check, max_processes){
  stopifnot( is.numeric(n), n %% 1 == 0, length(n) == 1, n > 0,
             is.numeric(trigger_check), length(trigger_check) == 1, trigger_check >= 0 & trigger_check <=1,
             is.numeric(max_processes), length(max_processes) == 1, max_processes > 0)
  
  df <- data.frame( "id" = c(NA),
                    "size_id" = c(NA))
  
  output <- list( "size" = n,
                  "trigger_check" = trigger_check,
                  "max_processes" = max_processes,
                  "df" = df)
  
  class(output) <- "memory"
  
  return(output)
}

memory_manager <- create_memory_manager(n=100,trigger_check=0.5,max_processes=1000)

new_process <- function(memory, required_memory){
  stopifnot( class(memoer) == "memory",
             is.numeric(required_memory), length(required_memory) == 1, required_memory > 0)
  
  if( required_memory > memory[["size"]]){
    stop( "There is not any available amount of memory.")
  } 
  
  if ( length(memory[["df"]]["id"] > memory[["max_processes"]])){
    stop("The maximum number of processes is  reached.")
  }
  
  id <- id + 1
  
  memory_manager <- list( memory_manager, df)
  
  if( is.na( memory[["df"]]["id"])){
    memory[["df"]][,1:2] <- c( id, required_memory)
  } else{
    memory[["df"]] <- rbind(memory[["df"]], c(id,required_memory))
  }
  
  s <- sum([["memory"]]["df"]["size_id"])
  
  p <- s/100
  
  if( p > memory[["trigger_check"]]){
    
  }
  
  memory[["size"]] <- memory[["size"]] - required_memory
  
  return(memory)
}