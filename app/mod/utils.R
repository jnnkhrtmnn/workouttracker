

library(dplyr)
library(rdrop2)


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


save_df <- function(df, local=FALSE){
  save_path = "./data"
  file_name ="app_data.csv"
  write.csv(df, paste(save_path, file_name,sep="/"), row.names=FALSE)
  if(!local){
    rdrop2::drop_upload(paste(save_path, file_name,sep="/"), path="workout")
  }
  return('Added workout to dataframe!')
}


load_df <- function(local=FALSE){
  if(local){
    load_path = "./data"
    file_name ="app_data.csv"
    df = read_csv(paste(load_path, file_name, sep="/"))
  } else {
    df = rdrop2::drop_read_csv("workout/app_data.csv")
    df = mutate(df, date = as.Date(date))
  }
  return(df)
}

load_bw_data <- function(local=FALSE){
  if(local){
    load_path = "./data"
    file_name ="bw_data.csv"
    df = read_csv(paste(load_path, file_name, sep="/"))
  } else {
    df = rdrop2::drop_read_csv("workout/bw_data.csv")
  }
  return(df)
}

save_bw_df <- function(df, local=FALSE){
  save_path = "./data"
  file_name ="bw_data.csv"
  write.csv(df, paste(save_path, file_name,sep="/"), row.names=FALSE)
  if(!local){
    rdrop2::drop_upload(paste(save_path, file_name,sep="/"), path="workout")
  }
  return('Added bodyweight to dataframe!')
}
