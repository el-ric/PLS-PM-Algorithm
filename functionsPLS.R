
defineEnvironment <- function(file_directory="C:\\Users\\",file_name ){
  rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
  setwd(file_directory)
  read_file = read.csv(file_name)
  x = 1  
  #Cleans all variables
  # 
  #Working directory
  
  #read and discover the data 
  
  return(read_file)
}
