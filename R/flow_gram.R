#' Flow_gram
#'
#' Creates a scenario based on three paramets:\cr 
#' 1. Direct Probability (Percentage of potential cyclists in the overall population). Its value is in percentages (between 0 and 1)\cr
#' 2. Ebikes: A boolean variable indicating whether ebikes are being used or not. This allows longer journeys to be converted into ebike journeys.\cr
#' 3. Equity: A boolean variable whether women have the same probablity of converting their trips into cycling trips.\cr
#' 
#' At the end, this function returns a data frame (similar to the baseline data frame), with new cycling trips
#' 
#'
#' @param baseline Baseline Travel Survey Dataframe
#' @param DP Direct probability of potential cyclists
#' @param ebikes Boolean variable for ebikes
#' @param equity Boolean variable for equity (between men and women)
#' @param pcycl_baseline Cycling probability broken down by age and gender groups
#' @export

flow_gram <-function(baseline, DP,ebikes,equity, pcycl_baseline) {

  #resets all senarios parameters: trip cycled(now_cycle) | person=cyclist | prob cycling a trip (Pcyc)
  baseline$now_cycle <- 0
  baseline$cyclist <- 0
  baseline$Pcyc <- 0
  baseline$ebike   <- 0

  ## add choice variable to all rows
  baseline$choice <- 0

  #resets travel times
  baseline$TripTotalTime1 <- baseline$TripTotalTime
  baseline$TripTravelTime1 <- 0  #not used

  justrandom <- runif(nrow(baseline),0,1)   #prob vector for now_cycle (trip probability)
  #NON-EQUITY scenario (mode shift * odds in both)
  #EQUITY scenario

  if (equity == 0) {
    Pcyc0 <- (DP - 1) * Pcyc0.eq0
  }else {
    Pcyc0 <- (DP - 1) * Pcyc0.eq1
  }

  #calc new probs
  Pcyc0 <- sapply(Pcyc0,oddsp)
  lookup <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'),
                       Pcyc0)
  lookup$agesex <- as.character(lookup$agesex)
  #calculate if people become cyclists
  baseline <- inner_join(baseline,lookup,by='agesex')
  baseline$cyclist <- 0
  #baseline$cyclist[baseline$Pcyc0 > baseline$prob] <- 1

  # direct probs

  # init processedBaseline DF which should include results of DP function
  # for whole country and for every region. Same structure as baseline

  baselineProcessed <- baseline[0, ]

  # iterate over all regions

  for (region in unique(baseline$HHoldGOR_B02ID)){

    # subset data for a particular region

    baselineSubset <- subset(baseline, HHoldGOR_B02ID == region)

    IDOfPplCyclist <- directProbRRPPLIDs(baselineSubset, DP, ebikes, equity, pcycl_baseline, region)

    baselineSubset[baselineSubset$ID %in% IDOfPplCyclist,]$cyclist <- 1

    # add baselineSubset to baselineProcessed

    baselineProcessed <- rbind(baselineProcessed, baselineSubset)

    rm(baselineSubset)

  }

  # calc DP for whole country treating it as region with id = 0

  # set new region value

  baselineCoutry <- baseline

  baselineCoutry$HHoldGOR_B02ID <- 0

  IDOfPplCyclist <- directProbRRPPLIDs(baselineCoutry, DP, ebikes, equity, pcycl_baseline, 0)

  baselineCoutry[baselineCoutry$ID %in% IDOfPplCyclist,]$cyclist <- 1

  # add baselineCoutry to baselineProcessed

  baselineProcessed <- rbind(baselineProcessed, baselineCoutry)
  rm(baselineCoutry)

  # add tripIDRegional column with unique value for every trip on regional level
  # (since TripID is duplicated because region 0 is just a copy of all other regions)

  baselineProcessed$TripIDRegional <- seq.int(nrow(baselineProcessed))

  # baselineProcessed shoud be now baseline

  baseline <- baselineProcessed
  rm(baselineProcessed)
  gc()
  # end of direct probs

  baseline$newtime <- baseline$TripDisIncSW / apply(data.frame(baseline$Age, baseline$Sex), 1, function(x) tripspeed(x[1], x[2], 0))

  baseline[baseline$Cycled == 1,]$Pcyc <- 1

  #baseline[baseline$Cycled == 1,]$METh <- METcycling * baseline[baseline$Cycled == 1,]$newtime

  #baseline[baseline$Cycled == 1,]$MMETh <- (METcycling - 1) * baseline[baseline$Cycled == 1,]$newtime

  baseline[baseline$Cycled != 1 & baseline$cyclist == 0 ,]$TripTotalTime1 <-
    baseline[baseline$Cycled != 1 & baseline$cyclist == 0 ,]$TripTotalTime

  #calculate prob of a given trip being cycled
  baseline[baseline$Cycled != 1 & baseline$cyclist != 0 ,]$Pcyc <-
    apply(subset(baseline, Cycled != 1 & cyclist != 0, select = c(Age,Sex,TripDisIncSW)), 1,
          function(x) pcyc21(x[1],x[2], x[3], ebikes, equity, DP))

  ## add random column to the baseline data.frame
  baseline$justrandom <- justrandom
  cat(class(baseline$Cycled), " : ", class(baseline$cyclist), " : ", class(baseline$Pcyc), " : ", class(baseline$justrandom), "\n")
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$now_cycle <- 1

  if (ebikes == 0 && (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]) > 0)){

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$METh <-
      METcycling * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$newtime

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$MMETh <-
      (METcycling - 1) * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$newtime

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$TripTotalTime1 <-
      round(60 * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) ,]$newtime, 0)

  }

  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom),]$choice <-
    apply(subset(baseline, Cycled != 1 & cyclist == 1 & (Pcyc > justrandom),
                 select = c(TripDisIncSW)), 1, function(x) bikechoice(x[1], unlist(subset(pcycl_baseline, select = tripsebike), use.names = FALSE)))

  if (ebikes == 1){
    if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]) > 0)
      baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$ebike <- 1

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime <-
      baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$TripDisIncSW /
      apply(subset(baseline, Cycled != 1 & cyclist == 1 & (Pcyc > justrandom) & choice == 1, select = c(Age, Sex)), 1, function(x) tripspeed(x[1], x[2], 1))

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$METh <-
      METebikes * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$MMETh <-
      (METebikes - 1) * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$TripTotalTime1 <-
      round(60 * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice == 1,]$newtime, 0)

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$METh <-
      METcycling * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$newtime

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$MMETh <-
      (METcycling - 1) * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$newtime

    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$TripTotalTime1 <-
      round(60 * baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc > baseline$justrandom) & baseline$choice != 1,]$newtime, 0)
  }

  # Cycled != 1 & cyclist == 1 & Pcyc <= justrandom
  if (nrow(baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]) > 0)
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$now_cycle <- 0

  baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$TripTotalTime1 <-
    baseline[baseline$Cycled != 1 & baseline$cyclist == 1 & (baseline$Pcyc <= baseline$justrandom),]$TripTotalTime1

  # nombre <- paste("DP",DP,"_ebik",ebikes,"_eq" ,equity,".csv",sep="")
  nombre <- paste("DP",DP,"_ebik",ebikes,"_eq" ,equity,sep="")

  # Fixed a bug: replaced colnames with c
  # Removed TripTravelTime1
  blsave <- baseline[,c('ID','HHoldGOR_B02ID','TripID','TripIDRegional','now_cycle','ebike','cyclist','METh','MMETh','TripTotalTime1')]

  # write.csv(blsave,file=paste(scenarioFolderNameAndPath, nombre, sep = "\\"), row.names=F)
  cat("size: ", nrow(blsave), " - ", nombre,'\n',' done !!','\n')

  # Return blsave
  blsave
}
