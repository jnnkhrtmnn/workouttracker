#######################################################################################
#######################################################################################

library(imputeTS)

setwd("C:/Users/janni/Desktop/gym")
source("utils.R")


#######################################################################################
#######################################################################################

list_in = list('date' = '2021-04-22'
              ,'workout' = 'A'
              ,'squat' = 79
              ,'bench_press' = 82.5
              ,'press' = NA #50
              ,'cable_rows' = 75
              ,'pull_ups' =  c(NA, NA) #c(0,'7/7/6') #
              ,'chin_ups' = c(NA, NA)  #c(0,'8/8/7')
              ,'power_clean' = NA #60
              ,'deadlift' = 110
              ,'bodyweight' = NA
              )


#data = add_to_df(list_in=list_in, save=TRUE)
#data = add_to_df(list_in=list_in, save=FALSE)

#plot_progress(data=data$weights, relative=FALSE)
#plot_progress(data=data$weights, relative=TRUE)


#######################################################################################
#######################################################################################

data = load_df()


plot_progress(data=data$weights, relative=TRUE)
plot_progress(data=data$weights, relative=FALSE)



#######################################################################################
#######################################################################################




