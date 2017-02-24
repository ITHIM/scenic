tripspeed <-function(age,sex,biketype)  {
  #ageIntervals <-c(16,30,45,60,70,80)
  # cat(age, " : ", sex, "\n")
  if (biketype == 0) {     
    if (sex == 'Female'){
      if (age == '16.59') 
        speed = 10.12
      else 
        speed = 8.27
    }
    
    else    #sex=male
    {
      if (age == '16.59') 
        speed = 10.87
      else          
        speed = 9.08}
  }     
  
  if (biketype == 1) 
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
