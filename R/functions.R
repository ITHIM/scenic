#' Trip Speed
#'
#' Returns speed of a cycling trip, based on following parameters:
#' 1. age: With two categories 16-59 and 60+ year olds
#' 2. gender: Male or Female
#' 3. bike_type: A boolean variable whether it's a pedal bike (0) or an ebike (1)
#' 
#'
#' @param baseline Baseline Travel Survey Dataframe
#' @param DP Direct probability of potential cyclists
#' @param ebikes Boolean variable for ebikes
#' @param equity Boolean variable for equity (between men and women)
#' @param pcycl_baseline Cycling probability broken down by age and gender groups
#' @export

trip_speed <- function(age, sex, bike_type)  {
  if (bike_type == 0) {
    if (sex == 'Female') {
      if (age == '16.59')
        speed = 10.12
      else
        speed = 8.27
    }
    
    else
      #sex=male
    {
      if (age == '16.59')
        speed = 10.87
      else
        speed = 9.08
    }
  }
  
  if (bike_type == 1)
    speed = 11.58
  
  tripspeed = speed
}



bikechoice <-function(dist, tripsebike) { 
  #calculates prob of switch to cycling depending on: [age-sex-trip distance]
  
  #intervals for distance binning
  distIntervals <-c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,9.5,12.5,15.5,20.5,30.5,40.5,10000)
  
  
  #get interval to use as nrow
  nrow <- findInterval(dist,distIntervals)+1  # starts @ 0, add 1
  # Comment out hard coded trips ebike probabilities
  # probstrip <- c(0.550,0.680,0.751,0.815,0.889,0.889,0.905,0.929,0.947,0.919,0.919,1.000,1.000,0.000)
  
  # Read tripsebike from the pcycl_baseline variable tripsebike
  probstrip <- tripsebike
  
  result <- probstrip[nrow]
  x <- runif(1,0,1)
  
  bikechoice <- ifelse(x<result,1,0)
}




oddsp <- function (x){   
  podds <- x/(x+1) 
}

podds <- function (x)
{   podds<- x/(1-x) }



pcyc21 <-function(age,sex,dist,ebikes,equity,MS) {
  
  #intervals for distance binning
  distIntervals <-c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,9.5,12.5,15.5,20.5,30.5,40.5,10000)
  #get interval to use as nrow
  nrow <- findInterval(dist,distIntervals)+1  # starts @ 0, add 1
  prob <- 0
  if (ebikes == 0) {
    ncol <- paste(sex,age,sep="_")
    prob <- pcycl_baseline[nrow,ncol]   
  } #end ebikes=0
  else {    #ebikes=1
    ncol <- 'ebike'   #same w or w/o  equity
    prob <- pcycl_baseline[nrow,ncol] 
  }
  
  pcyc <- prob
  pcyc
}
