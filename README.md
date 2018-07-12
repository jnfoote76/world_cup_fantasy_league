# World Cup Fantasy League Result Calculator
This repo contains a CLI tool written in Go that determines the result of my fantasy football league's World Cup bracket competition (don't think too much about it). 

## Usage
The following command-line flags are supported:

```
  -countriesOfInterestFilename string
    	A newline separated file containing all of the countries whose results affect the remaining branches (default "countries_of_interest.csv")
  -countryFile string
    	A csv file containing the starting positions of all countries from the group stage (default "countries.csv")
  -debug
    	Whether or not to log in debug mode
  -decidedMatchesFilename string
    	A csv file containing all decided matches of the knockout stage (default "decided.csv")
  -outcomeFile string
    	The path of the outputted csv file (default "outcomes.csv")
  -picksFile string
    	A csv file containing each person's picks for each country (default "responses.csv")
 ```
 
 To run (from the root of the repo):
 ```
 $ go build
 $ ./world-cup-fantasy <flags>
 ```