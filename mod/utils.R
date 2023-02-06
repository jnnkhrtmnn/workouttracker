

library(tidyverse)

add_workout_to_df <- function(df, list_in){
    
  df_w = tibble(date=c(),exercise=c(), weight=c(), set=c(), reps=c())
  for (i in (1:length(list_in$exercises))){
    
    ex = list_in$exercises[[i]]
  
      df_ex = tibble(
          date=rep(list_in$date, length(ex$set)),
          exercise=rep(names(list_in$exercises)[i],length(ex$set)),
          weight=ex$weight,
          set=ex$set,
          reps=ex$reps
          )
    
    df_w = bind_rows(df_w,df_ex)
  }
  
  df = bind_rows(df, df_w)
  
  return(df)
}


save_df <- function(df){
  save_path = "./data"
  file_name ="app_data.csv"
  write.csv(df, paste(save_path, file_name,sep="/"), row.names=FALSE)
  return('Added workout to dataframe!')
}


load_df <- function(){
  load_path = "./data"
  file_name ="app_data.csv"
  df = read_csv(paste(load_path, file_name, sep="/"))
  return(df)
}

load_bw_data <- function(){
  load_path = "./data"
  file_name ="bw_data.csv"
  df = read_csv(paste(load_path, file_name, sep="/"))
  return(df)
}

save_bw_df <- function(df){
  save_path = "./data"
  file_name ="bw_data.csv"
  write.csv(df, paste(save_path, file_name,sep="/"), row.names=FALSE)
  return('Added bodyweight to dataframe!')
}
