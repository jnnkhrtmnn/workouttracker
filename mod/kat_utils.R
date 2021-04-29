





list_in = list('date' = '2021-04-22'
               ,'bodyweight' = NA
               ,'exercises' = list(
                  'squat' = list(
                    'weight' = 79
                    ,'sets' = 3
                    ,'reps'= c(5,5,5)
                  ),
                  'bench_press' = list(
                    'weight' = 79
                    ,'sets' = 3
                    ,'reps'= c(5,5,5)
                  )
               )
)


library(tidyverse)

#df = tibble(date=c(),exercise=c(), weight=c(), sets=c(), reps=c())


add_workout_to_df <- function(df, list_in){
    
  df_w = tibble(date=c(),exercise=c(), weight=c(), sets=c(), reps=c())
  for (i in (1:length(list_in$exercises))){
    
    ex = list_in$exercises[[i]]
  
      df_ex = tibble(
          date=rep(list_in$date, ex$sets),
          exercise=rep(names(list_in$exercises)[i],ex$sets),
          weight=rep(ex$weight,ex$sets),
          sets=rep(ex$sets, ex$sets),
          reps=ex$reps
          )
    
    df_w = bind_rows(df_w,df_ex)
  }
  
  df = bind_rows(df, df_w)
  
  return(df)
}


save_df <- function(df){
  save_path = "C:/Users/janni/Desktop/gym/data"
  file_name ="kat_app_data.csv"
  write.csv(df, paste(save_path, file_name,sep="/"), row.names=FALSE)
  return('Added workout to dataframe!')
}


load_df <- function(){
  load_path = "C:/Users/janni/Desktop/gym/data"
  file_name ="kat_app_data.csv"
  df = read_csv(paste(load_path, file_name, sep="/"))
  return(df)
}

load_bw_data <- function(){
  load_path = "C:/Users/janni/Desktop/gym/data"
  file_name ="kat_bw_data.csv"
  df = read_csv(paste(load_path, file_name, sep="/"))
  return(df)
}

save_bw_df <- function(df){
  save_path = "C:/Users/janni/Desktop/gym/data"
  file_name ="kat_bw_data.csv"
  write.csv(df, paste(save_path, file_name,sep="/"), row.names=FALSE)
  return('Added bodyweight to dataframe!')
}



#df = load_df()
#plot_dat = group_by(df, exercise, date) %>% summarize(weight = mean(weight), reps=mean(reps), sets=mean(sets))
#exercises = c(plot_dat %>% count(exercise) %>% arrange(desc(n)) %>% select(exercise))[[1]]

#da = plot_dat %>% filter(exercise %in%c("bench_press"))


#lm = lm(weight ~ date, da)

#plot(lm)
#summary(lm)