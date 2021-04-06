library(httr)
library(rjson)

reqTeams  = function(year){
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
  print(names)
  singleSeason <<- data.frame(names,ids)
  return(singleSeason)
}

reqRaceRounds = function(season){
  print(paste("Request races ", season))
  api_head <- 'https://ergast.com/api/f1/'
  file_results <- '.json'
  url <- paste(api_head,season,file_results,sep="", collapse=NULL)
  req <- GET(url)
  seasonRounds <- content(req)
  seasonRounds <-seasonRounds$MRData$RaceTable$Races
  return(length(seasonRounds))
}

reqTeamId <- function(team){
  print(paste("Request team ID ", team))
  
  teamId <<- subset(singleSeason,singleSeason[1] == team)[2]
  return(teamId)
}

pointsMaximum <- function(year){
  return(40)
}


reqTeamPoints = function(season, team, races){
  print(nzchar(team))
  if(!nzchar(team))
    return("")
  print(paste("Request points of", team, season, sep =" "))
  constructor <- reqTeamId(team)
  api_head <- 'https://ergast.com/api/f1/'
  file_results <- '/constructorStandings.json'
  url <- paste(api_head,season,'/',races,'/constructors/',constructor,file_results, sep="",collapse=NULL)
  req <-GET(url)
  print(url)
  data <- content(req)
  pointsGained <- data$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings[[1]]$points
  pointsGained <- as.integer(pointsGained)
  position <- data$MRData$StandingsTable$StandingsLists[[1]]$ConstructorStandings[[1]]$positionText
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
  print(c(1:races))
  for(race in seq(from =1, to = races, by = 1)){
    url <- paste(api_head,season,'/',race,'/constructors/',constructor,file_results, sep="",collapse=NULL)
    req <- GET(url)
    print(url)
    data <- content(req)
    raceInfo <-  data$MRData$RaceTable$Races[[1]]
    if (raceInfo$Results[[1]]$grid =="1" || raceInfo$Results[[2]]$grid == "1"){
      polePositions<- polePositions+1
      print("Pole")
    }
    positions <- c(as.integer(raceInfo$Results[[1]]$position), as.integer(raceInfo$Results[[2]]$position))
    podiums <- podiums + sum(positions<4)
    if(!raceInfo$Results[[1]]$positionText == "R" && !raceInfo$Results[[1]]$positionText == "D"  && !raceInfo$Results[[1]]$positionText == "W" ){
      if(season>2003){
        if (raceInfo$Results[[1]]$FastestLap$rank =="1"){
          fastestLaps<- fastestLaps+1
          print("Fastest Lap")
        }
      }else
        fastestLaps <- "Available from 2004 season"
    }
    else
      retirements <- retirements+1
    if(!raceInfo$Results[[2]]$positionText == "R" && !raceInfo$Results[[2]]$positionText == "D"  && !raceInfo$Results[[2]]$positionText == "W"){
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
    pointsProgress[race] <- totalPoints
    if(racePoints >= maximumPoints){
      maximumPoints <- racePoints
      raceBestResult <- raceInfo$raceName
      positionsBest <- paste(positions[1],"°", " and ", positions[2],"°" ," position",sep="")
      }
  }
  summarySeason <- data.frame(position, podiums, pointsGained, wins, retirements, polePositions, fastestLaps, raceBestResult,positionsBest, maximumPoints, pointsProgress)
  return(summarySeason)
}



