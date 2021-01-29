get_data <- function(query){
  res<- dbExecute(con, query)
  return(dbFetch(res))
}


source("r-codes/lm_simulator.R")