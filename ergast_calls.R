library(httr)
library(rjson)

reqTeams  <-  function(year){
  print(paste("Request Team list for", year))
  api_head <- 'https://ergast.com/api/f1/'
  file_results <- '/constructors.json'
  url <- paste(api_head,year,file_results,sep="", collapse=NULL)
  req = GET(url)
  response <- content(req)
  seasonTeams <- response$MRData$ConstructorTable$Constructors
  ids <- c()
  names <-c()
  teamCounter <- c(1:length(seasonTeams))
  for (i in teamCounter){
    ids[i] <- seasonTeams[[i]]$constructorId
    names[i] <- seasonTeams[[i]]$name
  }
  singleSeason <<- data.frame(names,ids)
  return(singleSeason)
}

reqRaceRounds <-  function(season){
  print(paste("Request races ", season))
  api_head <- 'https://ergast.com/api/f1/'
  file_results <- '.json'
  url <- paste(api_head,season,file_results,sep="", collapse=NULL)
  req <- GET(url)
  seasonRounds <- content(req)
  seasonRounds <-seasonRounds$MRData$RaceTable$Races
  return(length(seasonRounds))
}

reqPointsSystem <- function(season){
  print(paste("req point system",season))
  if(season<2003){
    return(list(Position = c("1°","2°","3°","4°","5°","6°"),
                 Points = c(10,6,4,3,2,1)))
  }
  if(season>=2003 && season<=2009 ){
    return(list(Position = c("1°","2°","3°","4°","5°","6°","7°","8°"),
                      Points = c(10,8,6,5,4,3,2,1)))
  }
  if(season>=2010 && season<=2018 ){
    return(list(Position = c("1°","2°","3°","4°","5°","6°","7°","8°","9°","10°"),
                 Points = c(25,18,15,12,10,8,6,4,2,1)))
  }
  if(season>2018){
    return(list(
      Position = c("1°","2°","3°","4°","5°","6°","7°","8°","9°","10°","Fastest Lap"),
      Points = c(25,18,15,12,10,8,6,4,2,1,1)))
  }
}

reqTeamId <- function(team){
  print(paste("Request team ID ", team))
  
  teamId <<- subset(singleSeason,singleSeason[1] == team)[2]
  return(teamId)
}

pointsMaximum <- function(year){
  return(40)
}
reqDrivers <- function(season,team){
  print("request drivers")
  driverIds <- c()
  driverNames <- c()
  if(missing(team) || !nzchar(team)){
    url <- paste("http://ergast.com/api/f1/",season,"/driverStandings.json", sep="")
    print(url)
    req <-GET(url)
    data <- content(req)
    driverInfo <- data$MRData$StandingsTable$StandingsLists[[1]]$DriverStandings
    for(driver in driverInfo){
      driverIds <-c(driverIds,driver$Driver$driverId)
      driverNames <- c(driverNames, paste(driver$Driver$givenName, driver$Driver$familyName))
    }
  }
  else{
    constructor <- reqTeamId(team)$ids
    url <- paste('https://ergast.com/api/f1/',season,'/1/constructors/',constructor,'/drivers.json', sep="")
    req <-GET(url)
    print(url)
    data <- content(req)
    driverInfo <- data$MRData$DriverTable$Drivers
    for(driver in driverInfo){
      driverIds <-c(driverIds,driver$driverId)
      driverNames <- c(driverNames, paste(driver$givenName, driver$familyName))
    }
  }
  
  drivers <- list(driverIds = driverIds,
                  driverNames = driverNames)
  return(drivers)
}

reqStandings <- function(season, driver_id){
  url <- paste("http://ergast.com/api/f1/",season,"/drivers/",driver_id,"/driverStandings.json", sep="")
  print(url)
  req <-GET(url)
  data <- content(req)
  return (as.integer(data$MRData$StandingsTable$StandingsLists[[1]]$DriverStandings[[1]]$position))
}


reqSeasonResult <- function(season,driver){
  driver <- tolower(driver)
  driver <- gsub("[[:punct:]]", " ",driver)
  if(driver==""){
    return(NULL)
  }
  name <- unlist(strsplit(driver,"\\s"))
  url <- paste("http://ergast.com/api/f1/",season,"/drivers/",tail(name,1),"/results.json", sep="")
  print(url)
  req <-GET(url)
  data <- content(req)
  results <- data$MRData$RaceTable$Races
  if(length(results)==0){
    name <- tail(name,2)
    gsub("[[:punct:]]", " ",driver)
    url <- paste("http://ergast.com/api/f1/",season,"/drivers/",name[1],"_",name[2],"/results.json", sep="")
    print(url)
    req <-GET(url)
    data <- content(req)
    results <- data$MRData$RaceTable$Races
  }
  races <- reqRaceRounds(season)
  points <- c()
  qualified <- c()
  finished <- c()
  poles <- 0
  podiums <- 0
  fastestLaps <- 0
  tPoints <- 0
  retirements <- 0
  team <-  results[1][[1]]$Results[[1]]$Constructor$name
  driver_id <- results[1][[1]]$Results[[1]]$Driver$driverId
  for(race in results){
    tPoints <- tPoints + as.integer(race$Results[[1]]$points)
    points <- c(points,tPoints)
    if(!is.na(as.integer(race$Results[[1]]$positionText))){
      finished <- c(finished,as.integer(race$Results[[1]]$position))
      if(season>2003){
        if(race$Results[[1]]$FastestLap$rank == 1)
          fastestLaps <- fastestLaps+1
      }
    }
    else{
      retirements <- retirements+1
      finished <- c(finished,as.integer(race$Results[[1]]$position))
    }
    qualified <- append(qualified, as.integer(race$Results[[1]]$grid))
  }
  poles <- sum(qualified == 1, na.rm = TRUE)
  wins <- sum(finished == 1, na.rm = TRUE)
  podiums <- sum(finished < 4, na.rm = TRUE)
  while(length(points)<races){
      points <- append(points, points[length(points)])
      finished <- append(finished,NA)
      qualified <- append(qualified,NA)
      print("race added")
  }
  seasonDriver <- list(
    races = races,
    wins = wins,
    podiums = podiums,
    retirements = retirements,
    points = points,
    poles = poles,
    team = team,
    standings = reqStandings(season, driver_id),
    finished = finished,
    qualified = qualified,
    fastestLaps = fastestLaps
  )
  return(seasonDriver)
}

reqTeamPoints = function(season, team, races){
  if(!nzchar(team))
    return("")
  constructor <- reqTeamId(team)$ids
  api_head <- 'https://ergast.com/api/f1/'
  file_results <- '/constructorStandings.json'
  url <- paste(api_head,season,'/',races[2],'/constructors/',constructor,file_results, sep="",collapse=NULL)
  req <-GET(url)
  print(url)
  data <- content(req)
  pointsGained <- data$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings[[1]]$points
  pointsGained <- as.integer(pointsGained)
  position <- data$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings[[1]]$positionText
  if(is.na(as.integer(position)))
    position <- "DSQ"
  else
    position <- paste(position,"°")
  wins <- data$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings[[1]]$wins
  wins <- as.integer(wins)
  file_results <- '/results.json'
  podiums <- 0
  fastestLaps <- 0
  polePositions <- 0
  raceBestResult <-""
  maximumPoints <- 0
  retirements <- 0
  racePoints <- 0
  totalPoints <- 0
  pointsProgress <- c()
  for(race in seq(from=races[1], to=races[2], by=1)){
    print(race)
    url <- paste(api_head,season,'/',race,'/constructors/',constructor,file_results, sep="",collapse=NULL)
    req <- GET(url)
    print(url)
    data <- content(req)
    raceInfo <-  data$MRData$RaceTable$Races[[1]]
    if (raceInfo$Results[[1]]$grid =="1" || raceInfo$Results[[2]]$grid == "1"){
      polePositions<- polePositions+1
    }
    positions <- c(as.integer(raceInfo$Results[[1]]$position), as.integer(raceInfo$Results[[2]]$position))
    podiums <- podiums + sum(positions<4)
    if(!raceInfo$Results[[1]]$positionText == "R" && !raceInfo$Results[[1]]$positionText == "D"  && !raceInfo$Results[[1]]$positionText == "W" && !raceInfo$Results[[1]]$positionText == "F" ){
      if(season>2003){
        if (raceInfo$Results[[1]]$FastestLap$rank =="1"){
          fastestLaps<- fastestLaps+1
        }
      }else
        fastestLaps <- "Available from 2004 season"
    }
    else
      retirements <- retirements+1
    if(!raceInfo$Results[[2]]$positionText == "R" && !raceInfo$Results[[2]]$positionText == "D"  && !raceInfo$Results[[2]]$positionText == "W" && !raceInfo$Results[[1]]$positionText == "F"){
      if(season>2003){
       if(raceInfo$Results[[2]]$FastestLap$rank == "1"){
        fastestLaps<- fastestLaps+1
        print("Fastest Lap")
        }
      }else
        fastestLaps <- "Available from 2004 season"
      }else
        retirements <- retirements+1
    racePoints <- as.integer(raceInfo$Results[[1]]$points) + as.integer(raceInfo$Results[[2]]$points )
    totalPoints <- totalPoints+racePoints
    pointsProgress <- append(pointsProgress, totalPoints)
    if(racePoints >= maximumPoints){
      maximumPoints <- racePoints
      raceBestResult <- raceInfo$raceName
      positionsBest <- paste(positions[1],"°", " and ", positions[2],"°" ," position",sep="")
      }
  }
  summarySeason <- list(position = position,
                        podiums = podiums,
                        pointsGained = pointsGained,
                        wins = wins,
                        retirements = retirements,
                        polePositions = polePositions,
                        fastestLaps = fastestLaps,
                        raceBestResult = raceBestResult,
                        positionsBest = positionsBest,
                        maximumPoints = maximumPoints,
                        pointsProgress = pointsProgress,
                        teamId = constructor,
                        drivers = reqDrivers(season, team))
  return(summarySeason)
}



