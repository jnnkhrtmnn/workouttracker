
library(imputeTS)


create_df <- function(){
  
  col_names = c('date', 'regime', 'workout'
                ,'squat'
                ,'bench_press', 'press' 
                ,'cable_rows', 'pull_ups', 'chin_ups'
                ,'power_clean','deadlift'
                ,'bodyweight')
  
  data = data.frame(matrix(ncol=length(col_names), nrow=0))
  colnames(data) = col_names
  
  return(data)
  
}

load_df <- function(){
  
  path = "C:\\Users\\janni\\Desktop\\gym\\data\\"
  weights <- read.csv(paste(path, "weights.csv", sep=""), stringsAsFactors=FALSE, strip.white=TRUE)
  sets <- read.csv(paste(path, "sets.csv", sep=""), stringsAsFactors=FALSE, strip.white=TRUE)
  reps <- read.csv(paste(path, "reps.csv", sep=""), stringsAsFactors=FALSE, strip.white=TRUE)
  
  cls <- c('squat'
           ,'bench_press', 'press' 
           ,'cable_rows'
           ,'power_clean','deadlift'
           )
  
  weights[append(cls,'bodyweight')] <- sapply(weights[append(cls,'bodyweight')],as.numeric)
  sets[cls] <- sapply(sets[cls],as.numeric)
  reps[cls] <- sapply(reps[cls],as.numeric)
  
  data = list("weights" = weights,
              "sets" = sets,
              "reps" = reps)
  return(data) 
}


add_to_df <- function(list_in, save=FALSE){
  
  data = load_df()
  
  weights = data$weights
  sets = data$sets
  reps = data$reps

  weights_new = c(list_in$date,
                  list_in$workout,
               list_in$squat,
               list_in$bench_press,
               list_in$press,
               list_in$cable_rows,
               list_in$pull_ups[1],
               list_in$chin_ups[1],
               list_in$power_clean,
               list_in$deadlift,
               list_in$bodyweight)
  
  sets_new = c(list_in$date,
              if(!is.na(list_in$squat)){3}else{NA},
              if(!is.na(list_in$bench_press)){3}else{NA},
              if(!is.na(list_in$press)){3}else{NA},
              if(!is.na(list_in$cable_rows)){3}else{NA},
              if(!is.na(list_in$pull_ups[1])){3}else{NA},
              if(!is.na(list_in$chin_ups[1])){3}else{NA},
              if(!is.na(list_in$power_clean)){5}else{NA},
              if(!is.na(list_in$deadlift)){1}else{NA})

  reps_new = c(list_in$date,
             if(!is.na(list_in$squat)){5}else{NA},
             if(!is.na(list_in$bench_press)){5}else{NA},
             if(!is.na(list_in$press)){5}else{NA},
             if(!is.na(list_in$cable_rows)){5}else{NA},
             if(!is.na(list_in$pull_ups[1])){list_in$pull_ups[2]}else{NA},
             if(!is.na(list_in$chin_ups[1])){list_in$chin_ups[2]}else{NA},
             if(!is.na(list_in$power_clean)){3}else{NA},
             if(!is.na(list_in$deadlift)){5}else{NA})
  
  weights[nrow(weights)+1,] = weights_new  
  sets[nrow(sets)+1,] = sets_new
  reps[nrow(reps)+1,] = reps_new
  
  
  cls <- c('squat'
           ,'bench_press', 'press' 
           ,'cable_rows'
           ,'power_clean','deadlift'
           )
  
  weights[append(cls,'bodyweight')] <- sapply(weights[append(cls,'bodyweight')],as.numeric)
  sets[cls] <- sapply(sets[cls],as.numeric)
  reps[cls] <- sapply(reps[cls],as.numeric)
  
  if (save==TRUE){
    path = "C:\\Users\\janni\\Desktop\\gym\\data\\"
    write.csv(weights,paste(path, "weights.csv", sep=""), row.names=FALSE)
    write.csv(sets,paste(path, "sets.csv", sep=""), row.names=FALSE)
    write.csv(reps,paste(path, "reps.csv", sep=""), row.names=FALSE)
    print('Added workout to dataframe!')
  }
  
  data = list("weights" = weights,
              "sets" = sets,
              "reps" = reps)
  
  return(data) 
}





plot_progress <- function(data, relative=FALSE){
  
  l = 30
  u = 160
  date <- as.Date(data$date, "%Y-%m-%d")
  ma = "Weight increases over time"
  we = "Weight in kg"
  c = 2
  
  int_data = na.interpolation(data[,c(3,4,5,6,9,10,11)], option = "linear")
  if(relative==TRUE){
      data = as.data.frame(round((mapply('/', int_data,int_data[1,])) * 100))
      int_data = data
      l = 60
      u = 200
      ma = "Weight increases over time (relative)"
      we = "Percent"
      c=0.01
  }
  data$date = date
  
  plot(squat ~ date, data, xaxt = "n", type = "p", col='cyan', ylim=c(l, u),
       main=ma, xlab="Date", ylab=we, pch=19, cex=c)
  
  
  points(data$deadlift ~ date, col="blue", type="p", pch=19, cex=c)
  points(data$bench_press ~ date, col="purple", type="p", pch=19, cex=c)
  points(data$press ~ date, col="cornflowerblue", type="p", pch=19, cex=c)
  points(data$power_clean ~ date, col="aquamarine3", type="p", pch=19, cex=c)

    
  points(int_data$squat ~ date, col="cyan", type="l", lwd=2)
  points(int_data$deadlift ~ date, col="blue", type="l", lwd=2)
  points(int_data$bench_press ~ date, col="purple", type="l", lwd=2)
  points(int_data$press ~ date, col="cornflowerblue", type="l", lwd=2)
  points(int_data$power_clean ~ date, col="aquamarine3", type="l", lwd=2)

  
  axis(1, date, format(date, "%d-%m"), cex.axis = 1.)
  legend("topleft", legend=c("Squat", "Deadlift", "Bench press", "Press", "Power clean"),
         col=c("cyan","blue","purple", "cornflowerblue", "aquamarine3"), cex=1.2, pch=19,bty='n', pt.cex=2, 
         horiz=FALSE, x.intersp=0.5, y.intersp=0.5)
  if(relative==TRUE){
    abline(h=100, col="red", lty=2, lwd=2)
    abline(h=120, col="grey", lty=2, lwd=1)
    abline(h=140, col="grey", lty=2, lwd=1)
  }
  else{
    abline(h=40, col="grey", lty=2, lwd=1)
    abline(h=40, col="grey", lty=2, lwd=1)
    abline(h=60, col="grey", lty=2, lwd=1)
    abline(h=80, col="grey", lty=2, lwd=1)
    abline(h=100, col="grey", lty=2, lwd=1)
    abline(h=120, col="grey", lty=2, lwd=1)
  }
}