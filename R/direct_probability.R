#' Return IDs of all ppl (including also these who were cyclist already) who became potential cyclist using relative risk approach

directProbRRPPLIDs <- function(baselineSubset, DP, ebikes, equity, pcycl_baseline, region) {
  
  # if DP == 1 -> don't process just return all IDs assuming that there is no population in which cyclists are 100%
  
  if (DP == 1){
    
    return(unique(baselineSubset$ID))
    
  }
  
  # calc number of cyclist in baselineSubset
  
  totalNumberOfCyclistInBaselineSubset <- length(unique(baselineSubset[baselineSubset$Cycled == 1,]$ID))
  
  # if observed proportion of cyclist in baselineSubset > DP value -> don't process, just return IDs of all cyclist, print info in a console.
  # These cases could be filtered out in UI
  
  shareOfCyclistInBaselineSubset <- totalNumberOfCyclistInBaselineSubset / length(unique(baselineSubset$ID))
  
  if (shareOfCyclistInBaselineSubset > DP){
    
    # save that case in directProbCasesAboveGivenPerc
    
    directProbCasesAboveGivenPerc[nrow(directProbCasesAboveGivenPerc) + 1, ] <<- list(DP, ebikes, equity, region)
    
    # print info
    
    print('Observed > DP')
    print(shareOfCyclistInBaselineSubset)
    
    return(unique(baselineSubset[baselineSubset$Cycled == 1,]$ID))
  }
  
  # just in case reset cyclist column
  
  baselineSubset$cyclist <- 0
  
  # init vector with IDs of ppl who are cyclist already
  
  IDOfPplAlreadyCyclist <- c()
  
  # init vector with IDs of ppl who are going to become cyclist
  
  IDOfPplBecomingCyclist <- c()
  
  # init vector with IDs for output (combining IDOfPplAlreadyCyclist + IDOfPplBecomingCyclist)
  
  IDOfPplAllCyclistOutput <- c()
  
  # calc how many cyclists should be drawn excluding # of ppl who already cycle
  
  howManyCyclistNeeded <- round(DP * length(unique(baselineSubset$ID)), digits = 0) - totalNumberOfCyclistInBaselineSubset
  
  # just in case check if number exceeds # of all ppl
  
  howManyCyclistNeeded <- ifelse(howManyCyclistNeeded > length(unique(baselineSubset$ID)), length(unique(baselineSubset$ID)), howManyCyclistNeeded)
  
  # cyclists in a population should be marked as cyclists for all trips
  
  IDOfPplAlreadyCyclist <- unique(baselineSubset[baselineSubset$Cycled == 1, ]$ID)
  
  baselineSubset[baselineSubset$ID %in% IDOfPplAlreadyCyclist, ]$cyclist <- 1
  
  if (equity == 0) {
    
    # init counter of remaining ppl (ppl that should be pick up from other subgroups because of lack of ppl in particular subgroups)
    
    remainingCyclistsCounter <- 0
    
    # work out proportion of cyclists by age-sex subgroups in baselineSubset, also store number of observations in every subgroup
    
    cyclistsPropBySubgroups <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'), stringsAsFactors = FALSE)
    
    for (i in seq_len(nrow(cyclistsPropBySubgroups))){
      
      cyclistsPropBySubgroups[i, c('ppl')] <- length(unique(baselineSubset[baselineSubset$agesex == cyclistsPropBySubgroups$agesex[i], ]$ID))
      cyclistsPropBySubgroups[i, c('pplCyclist')] <- length(unique(baselineSubset[baselineSubset$Cycled == 1 & baselineSubset$agesex == cyclistsPropBySubgroups$agesex[i], ]$ID))
      cyclistsPropBySubgroups[i, c('pplNonCyclist')] <- cyclistsPropBySubgroups[i, ]$ppl - cyclistsPropBySubgroups[i, ]$pplCyclist
      cyclistsPropBySubgroups[i, c('prop')] <- round(cyclistsPropBySubgroups[i, ]$pplCyclist/cyclistsPropBySubgroups[i, ]$ppl, digits = 10)

    }
    
    # work out ratio, referencing (ratio=1) group is this with the largest number of ppl
    
    referencingValue <- cyclistsPropBySubgroups[which.max(cyclistsPropBySubgroups$ppl), ]$prop
    
    cyclistsPropBySubgroups$ratio <- mapply(function(propValue){
      
      propValue/referencingValue
      
    }, cyclistsPropBySubgroups$prop)
    
    # calc working column
    
    cyclistsPropBySubgroups$working <- mapply(function(ratio, pplNonCyclist){
      
      ratio * pplNonCyclist
      
    }, cyclistsPropBySubgroups$ratio, cyclistsPropBySubgroups$pplNonCyclist)
    
    # calc working sum
    
    workingSum <- sum(cyclistsPropBySubgroups$working)
    
    # iterate over subgroups - calc pplAdditionalCyclists, pplAfterCyclistsSub
    
    for (i in seq_len(nrow(cyclistsPropBySubgroups))){
      
      cyclistsPropBySubgroups[i, c('pplAdditionalCyclists')] <- cyclistsPropBySubgroups[i, ]$working / workingSum * howManyCyclistNeeded
      cyclistsPropBySubgroups[i, c('pplAfterCyclistsSub')] <- cyclistsPropBySubgroups[i, ]$pplNonCyclist - cyclistsPropBySubgroups[i, ]$pplAdditionalCyclists
      
      # check if there are enough ppl from subgroup in a population; if more are selected -> use total number of subgroup members
         
      realCyclistsInSubgroup <- round(ifelse(round(cyclistsPropBySubgroups[i, ]$pplAdditionalCyclists, digits = 0) > cyclistsPropBySubgroups[i, ]$pplNonCyclist, cyclistsPropBySubgroups[i, ]$pplNonCyclist, cyclistsPropBySubgroups[i, ]$pplAdditionalCyclists), digits = 0)
      
      # pick up ppl who become cyclist but are not cyclist already

      subgroupIDsOfPplBecomeCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex),]$ID), realCyclistsInSubgroup, replace = F)

      IDOfPplBecomingCyclist <- append(IDOfPplBecomingCyclist, subgroupIDsOfPplBecomeCyclist)
      
      # work out remaining diff (if value > 0 this means that sample should be filled with ppl from other subgroups)

      remainingCyclistsCounter <- remainingCyclistsCounter + ifelse(round(cyclistsPropBySubgroups[i, ]$pplAdditionalCyclists, digits = 0) - length(subgroupIDsOfPplBecomeCyclist) > 0, round(cyclistsPropBySubgroups[i, ]$pplAdditionalCyclists, digits = 0) - length(subgroupIDsOfPplBecomeCyclist), 0)
      
    } 
    
    # print(cyclistsPropBySubgroups)
    
    # fill scenario with ppl from other subgroups if remaining ppl exist
    
    if (remainingCyclistsCounter > 0){
      
      # just in case check if there is enough remaining ppl (rounding issues)
      
      remainingPplCounter <- length(unique(baselineSubset[baselineSubset$cyclist != 1 & !(baselineSubset$ID %in% IDOfPplBecomingCyclist),]$ID))
      
      if (remainingCyclistsCounter > remainingPplCounter){
        
        print('remainingCyclistsCounter > remainingPplCounter')
        
        remainingCyclistsCounter <- remainingPplCounter
        
      }
      
      # pick up remaining cyclists
      
      filledIDsOfPplBecomeCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1 & !(baselineSubset$ID %in% IDOfPplBecomingCyclist),]$ID), remainingCyclistsCounter, replace = F)
      
      IDOfPplBecomingCyclist <- append(IDOfPplBecomingCyclist, filledIDsOfPplBecomeCyclist)
    }
    
    # IDOfPplAlreadyCyclist are not included thus it means that trips which belong to already cyclist
    # but are not cycled will be never drawn
    
    IDOfPplAllCyclistOutput <- IDOfPplBecomingCyclist
    
    # return IDs
    
    return(IDOfPplAllCyclistOutput)
    
  } else {
    
    # pick up randomly ppl who are not cyclist using same prob. for all
    
    IDOfPplBecomingCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1,]$ID), howManyCyclistNeeded, replace = F)
    
    # IDOfPplAlreadyCyclist are not included thus it means that trips which belong to already cyclist
    # but are not cycled will be never drawn
    
    IDOfPplAllCyclistOutput <- IDOfPplBecomingCyclist
    
    # return IDs
    
    return(IDOfPplAllCyclistOutput)
    
  }
}

#' Return IDs of all ppl (including also these who were cyclist already) who became potential cyclist using proportions of cyclists group

directProbProportionsPPLIDs <- function(baselineSubset, DP, ebikes, equity, pcycl_baseline, region) {
  
  # if DP == 1 -> don't process just return all IDs assuming that there is no population in which cyclists are 100%
  
  if (DP == 1){
    
    return(unique(baselineSubset$ID))
    
  }
  
  # calc number of cyclist in baselineSubset
  
  totalNumberOfCyclistInBaselineSubset <- length(unique(baselineSubset[baselineSubset$Cycled == 1,]$ID))
  
  # if observed proportion of cyclist in baselineSubset > DP value -> don't process, just return IDs of all cyclist, print info in a console.
  # These cases could be filtered out in UI
  
  shareOfCyclistInBaselineSubset <- totalNumberOfCyclistInBaselineSubset / length(unique(baselineSubset$ID))
  
  if (shareOfCyclistInBaselineSubset > DP){
    
    # save that case in directProbCasesAboveGivenPerc
    
    directProbCasesAboveGivenPerc[nrow(directProbCasesAboveGivenPerc) + 1, ] <<- list(DP, ebikes, equity, region)
    
    # print info
    
    print('Observed > DP')
    print(shareOfCyclistInBaselineSubset)
    
    return(unique(baselineSubset[baselineSubset$Cycled == 1,]$ID))
  }
  
  # just in case reset cyclist column
  
  baselineSubset$cyclist <- 0
  
  # init vector with IDs of ppl who are cyclist already
  
  IDOfPplAlreadyCyclist <- c()
  
  # init vector with IDs of ppl who are going to become cyclist
  
  IDOfPplBecomingCyclist <- c()
  
  # init vector with IDs for output (combining IDOfPplAlreadyCyclist + IDOfPplBecomingCyclist)
  
  IDOfPplAllCyclistOutput <- c()
  
  # init counter of remaining ppl (ppl that should be pick up from other subgroups because of lack of ppl in particular subgroups)
  
  remainingCyclistsCounter <- 0
  
  # work out proportion of cyclists by age-sex subgroups in baselineSubset
  
  cyclistsPropBySubgroups <- data.frame(agesex = c('16.59Male','16.59Female','60plusMale','60plusFemale'))
  
  cyclistsPropBySubgroups$prop <-mapply(function(whichGroup) {
    round(length(unique(baselineSubset[baselineSubset$Cycled == 1 & baselineSubset$agesex == whichGroup,]$ID))/totalNumberOfCyclistInBaselineSubset, digits = 2)
  }, cyclistsPropBySubgroups$agesex)
  
  # calc how many cyclists should be drawn excluding # of ppl who already cycle
  
  howManyCyclistNeeded <- round(DP * length(unique(baselineSubset$ID)), digits = 0) - totalNumberOfCyclistInBaselineSubset
  
  # just in case check if number exceeds # of all ppl
  
  howManyCyclistNeeded <- ifelse(howManyCyclistNeeded > length(unique(baselineSubset$ID)), length(unique(baselineSubset$ID)), howManyCyclistNeeded)
  
  # cyclists in a population should be marked as cyclists for all trips
  
  IDOfPplAlreadyCyclist <- unique(baselineSubset[baselineSubset$Cycled == 1, ]$ID)
  
  baselineSubset[baselineSubset$ID %in% IDOfPplAlreadyCyclist, ]$cyclist <- 1
  
  if (equity == 0) {
    
    for (i in seq_len(nrow(cyclistsPropBySubgroups))){
      
      # calc how many cyclists should be drawn, taking into account cyclists prop
      
      projectedCyclistsInSubgroup <- round(as.numeric(cyclistsPropBySubgroups[i, ]$prop) * howManyCyclistNeeded, digits = 0)
      
      # check if there are enough ppl from subgroup in a population; if more are selected -> use total number of subgroup members
      
      realCyclistsInSubgroup <- ifelse(projectedCyclistsInSubgroup > length(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex), ]$ID)), length(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex), ]$ID)), projectedCyclistsInSubgroup)
      
      # pick up ppl who become cyclist but are not cyclist already
      
      subgroupIDsOfPplBecomeCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1 & baselineSubset$agesex == as.character(cyclistsPropBySubgroups[i, ]$agesex),]$ID), realCyclistsInSubgroup, replace = F)
      
      IDOfPplBecomingCyclist <- append(IDOfPplBecomingCyclist, subgroupIDsOfPplBecomeCyclist)
      
      # work out remaining diff (if value > 0 this means that sample should be filled with ppl from other subgroups)
      
      remainingCyclistsCounter <- remainingCyclistsCounter + ifelse(projectedCyclistsInSubgroup - length(subgroupIDsOfPplBecomeCyclist) <= 0, 0, projectedCyclistsInSubgroup - length(subgroupIDsOfPplBecomeCyclist))
      
    }
    
    # fill scenario with ppl from other subgroups if remaining ppl exist
    
    if (remainingCyclistsCounter > 0){
      
      filledIDsOfPplBecomeCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1 & !(baselineSubset$ID %in% IDOfPplBecomingCyclist),]$ID), remainingCyclistsCounter, replace = F)
      
      IDOfPplBecomingCyclist <- append(IDOfPplBecomingCyclist, filledIDsOfPplBecomeCyclist)
    }
    
    # IDOfPplAlreadyCyclist are not included thus it means that trips which belong to already cyclist
    # but are not cycled will be never drawn
    
    IDOfPplAllCyclistOutput <- IDOfPplBecomingCyclist
    
    # return IDs
    
    return(IDOfPplAllCyclistOutput)
    
  } else {
    
    # pick up randomly ppl who are not cyclist using same prob. for all
    
    IDOfPplBecomingCyclist <- sample(unique(baselineSubset[baselineSubset$cyclist != 1,]$ID), howManyCyclistNeeded, replace = F)
    
    # IDOfPplAlreadyCyclist are not included thus it means that trips which belong to already cyclist
    # but are not cycled will be never drawn
    
    IDOfPplAllCyclistOutput <- IDOfPplBecomingCyclist
    
    # return IDs
    
    return(IDOfPplAllCyclistOutput)
    
  }
}