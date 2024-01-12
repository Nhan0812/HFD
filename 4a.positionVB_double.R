positionVB_double <- function(signal, 
                               lower_entry, 
                               upper_entry, 
                               lower_exit,
                               upper_exit,
                               pos_flat, 
                               strategy)
{
  require(xts)
  
  # lets check the value of the strategy parameter
  if (! strategy %in% c("mom", "mr"))
  {  print("Strategy parameter incorrect. Please use 'mom' or 'mr'!")
    stop
  }
  
  # convert inputs to simpler objects  
  signal = coredata(signal)
  lower_entry = coredata(lower_entry)
  upper_entry = coredata(upper_entry)
  lower_exit = coredata(lower_exit)
  upper_exit = coredata(upper_exit)
  pos_flat = coredata(pos_flat)
  
  # MEAN REVERTING APPROACH!!!
  
  # lets first create a vector of 0s
  position <- rep(0, length(signal))
  
  for (i in 2:length(signal)) {
    if ( pos_flat[i] == 1 ) position[i] <- 0 
    else
    { # check if values are nonmissing (otherwise calculations not possible)
      if (!is.na(signal[i-1]) & !is.na(upper_entry[i-1]) &
          !is.na(upper_exit[i-1]) & !is.na(lower_exit[i-1]) &
          !is.na(lower_exit[i-1])) 
      { 
        # what if previous position was 0
        if (position[i-1] == 0){
          if (signal[i-1] > upper_entry[i-1]) position[i] <- -1
          if (signal[i-1] < lower_entry[i-1]) position[i] <- 1
        } else if (position[i-1]==-1){
          # what if previous position was -1
          if (signal[i-1] > lower_exit[i-1]) position[i] <- -1
          if (signal[i-1] <= lower_exit[i-1] & 
              signal[i-1] >= lower_entry[i-1]) position[i] <- 0
          if (signal[i-1] < lower_entry[i-1]) position[i] <- 1
        } else if (position[i-1]==1){
          # what if previous position was 1
          if (signal[i-1] < upper_exit[i-1]) position[i] <- 1
          if (signal[i-1] >= upper_exit[i-1] & 
              signal[i-1] <= upper_entry[i-1]) position[i] <- 0
          if (signal[i-1] > upper_entry[i-1]) position[i] <-  -1
        }
      }  else  position[i] <- position[i-1]
      # if anything is missing, keep previous position
    }
  }
  
  # reverse the position if we use a momentum ("mom") strategy
  if(strategy == "mom") position <- (-position)
  
  # return() function clearly indicates 
  # what the function should return
  return(position)
}