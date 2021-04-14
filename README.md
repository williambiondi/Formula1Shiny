# Formula1Shiny

## Intro
This university project is about developing a dashboard in the language R, using the framework Shiny. The project is a dasboard where is possible to visualize statistics about a team or a single driver in a given formula one season from 2000 to the 2021 (the 2021 is still in progress).
All the data is retrieved through the API of Ergast (http://ergast.com/mrd/) which is an experimental web service that provides an historical record of Formula One data.

## Formula One series
Formula One, also called F1 in short, is an international auto racing sport. F1 is the highest level of single-seat, open-wheel and open-cockpit professional motor racing contest.

### Objectives
A Formula One season is composed by a given number of races called Grand Prix conducted over a period of time (usually every year from march to october), they're located all around the world
The results of all the Grand Prix races in a season are taken together to determine two annual Championship awards. They are:

- Drivers' Championship Award (for the driver who scored most points)
- Constructors' Championship Award (for the team who scored most points)

### Teams
A team is usually a firm which joins the championship hiring two drivers, engineers, managers, mechanics and so on to build and manage the two cars allowed to race. A team is rewarded by the total points scored by the two drivers (example: if the two Ferrari drivers scored points, the team Ferrari gained the sum of the points scored for the constructor championship)

### Grand Prix
A Grand Prix is a weekend composed by sessions of free practice on friday, qualifying on saturday, and race on sunday.

Free practice is used by drivers to test and setup the car
Qualifying session builds up the starting grid rewarding with the first position (Pole position) the driver who has the fastest lap and so on with ascending order.
And finally the race, where the driver who crosses the finish line first after completing a pre-determined number of laps is declared the winner and score the maximum points. 
Other positions, usually top 10 or top 8 are rewarded by points based on the system which changed over the years.
Recently the driver who performed the fastest lap during the race is rewarded by an extra point.


## Teams Panel
In this panel it is possible to pick a season from 2000 to 2021 and a team, in the main plot we can see the points scored by the team during the whole season or a specific part of the season using the slider (example: from race 5 to race 10).
Below the plot there are other results of the team, like the number of wins, podiums, pole positions, retirements, and fastest laps

## Drivers Panel
In this panel it is possible to pick up to two drivers to make some comparison between two drivers with the points plot and the season review below. By default we have the list with all the drivers, but there is the option to specify the team and consequently choosing one of the two drivers in the team 
