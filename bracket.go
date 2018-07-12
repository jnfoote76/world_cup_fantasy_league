package main

import (
	"bufio"
	"encoding/csv"
	"errors"
	"flag"
	"io"
	"math"
	"os"
	"regexp"
	"sort"
	"strconv"

	"github.com/Sirupsen/logrus"
)

// Branch represents the overall state of country standings at the start of a particular round, given
// one particular combination of possible outcomes
// Contains a map with all currently-known final rounds for countries that have been eliminated,
// a list of all match-ups for the upcoming round, as well as the branches Semifinalists, Finalists, and Winner
// (if these have been determined)
type Branch struct {
	CountryOutcomes  map[string]int
	Winner           string
	Finalists        map[string]bool
	SemiFinalists    map[string]bool
	CurrRoundMatches [][2]string
}

// MatchOutcome contains a pair of the winning and losing countries for a particular match
type MatchOutcome struct {
	Winner string
	Loser  string
}

// Picks contains a person's predictions for the results of the World Cup
type Picks struct {
	OutcomesByCountry map[string]int
	Winner            string
	Finalists         []string
	SemiFinalists     []string
}

// BranchPersonOutcomes contains a pairing of a Branch with a map containing each person's score breakdown
// in the event of the Branch outcomes coming to pass
type BranchPersonOutcomes struct {
	Branch         Branch
	PersonOutcomes map[string]PersonOutcome
}

// PersonOutcome contains a score breakdown (scores yielded from distance between prediction and result for each)
// country, as well as bonuses for correctly predicting semifinalists, finalists, and/or the winner of the cup)
type PersonOutcome struct {
	CountryScores      map[string]int
	SemiFinalistsBonus int
	FinalistsBonus     int
	WinnerBonus        int
}

var (
	debug                       bool
	countryFilename             string
	picksFilename               string
	decidedMatchesFilename      string
	countriesOfInterestFilename string
	outomesFilename             string
)

func main() {
	flag.BoolVar(&debug, "debug", false, "Whether or not to log in debug mode")
	flag.StringVar(&countryFilename, "countryFile", "countries.csv", "A csv file containing the starting positions of all countries from the group stage")
	flag.StringVar(&picksFilename, "picksFile", "responses.csv", "A csv file containing each person's picks for each country")
	flag.StringVar(&decidedMatchesFilename, "decidedMatchesFilename", "decided.csv", "A csv file containing all decided matches of the knockout stage")
	flag.StringVar(&countriesOfInterestFilename, "countriesOfInterestFilename", "countries_of_interest.csv", "A newline separated file containing all of the countries whose results affect the remaining branches")
	flag.StringVar(&outomesFilename, "outcomeFile", "outcomes.csv", "The path of the outputted csv file")
	flag.Parse()

	if debug {
		logrus.SetLevel(logrus.DebugLevel)
		logrus.Debug("Logging in debug mode")
	}

	logrus.WithFields(logrus.Fields{
		"path": picksFilename,
	}).Info("Reading picks file")
	picksByName, err := readPicksFile(picksFilename)
	if err != nil {
		logrus.WithError(err).Error("Failed to read picks file")
		return
	}

	logrus.WithFields(logrus.Fields{
		"path": countryFilename,
	}).Info("Reading countries file")
	knockoutCountriesByCode, countryOutcomes, err := readCountryFile(countryFilename)
	if err != nil {
		logrus.WithError(err).Error("Failed to read countries file")
		return
	}

	var countries []string
	for _, country := range knockoutCountriesByCode {
		countries = append(countries, country)
	}

	for country := range countryOutcomes {
		countries = append(countries, country)
	}

	sort.Strings(countries)

	firstRoundMatches := [][2]string{
		[2]string{knockoutCountriesByCode["A1"], knockoutCountriesByCode["B2"]},
		[2]string{knockoutCountriesByCode["C1"], knockoutCountriesByCode["D2"]},
		[2]string{knockoutCountriesByCode["E1"], knockoutCountriesByCode["F2"]},
		[2]string{knockoutCountriesByCode["G1"], knockoutCountriesByCode["H2"]},
		[2]string{knockoutCountriesByCode["A2"], knockoutCountriesByCode["B1"]},
		[2]string{knockoutCountriesByCode["C2"], knockoutCountriesByCode["D1"]},
		[2]string{knockoutCountriesByCode["E2"], knockoutCountriesByCode["F1"]},
		[2]string{knockoutCountriesByCode["G2"], knockoutCountriesByCode["H1"]},
	}

	logrus.WithFields(logrus.Fields{
		"first_round_matches": firstRoundMatches,
	}).Debug("Determined first round matches")

	logrus.WithFields(logrus.Fields{
		"path": decidedMatchesFilename,
	}).Info("Reading decided matches file")
	decidedMatches, err := readDecidedMatchesFile(decidedMatchesFilename)
	if err != nil {
		logrus.WithError(err).Error("Failed to read decided matches file")
		return
	}

	for pairing, outcome := range decidedMatches {
		logrus.WithFields(logrus.Fields{
			"pairing": pairing,
			"winner":  outcome[0],
			"loser":   outcome[1],
		}).Debug("Will set predetermined outcome in all branches")
	}

	logrus.Info("Determining all possible branches")
	finalBranches := findAllBranches(countryOutcomes, firstRoundMatches, decidedMatches)
	logrus.WithFields(logrus.Fields{
		"num_branches": len(finalBranches),
	}).Info("Determined all possible branches")

	logrus.Info("Finding each person's score for each outcome")
	var branchOutcomeEntries []BranchPersonOutcomes
	for _, branch := range finalBranches {
		branchPersonOutcomes := BranchPersonOutcomes{
			PersonOutcomes: make(map[string]PersonOutcome),
		}
		for person, picks := range picksByName {
			personOutcome := findPersonBranchOutcome(branch, picks.OutcomesByCountry, picks.Winner, picks.Finalists, picks.SemiFinalists)
			branchPersonOutcomes.PersonOutcomes[person] = personOutcome
		}

		branchOutcomeEntries = append(branchOutcomeEntries, branchPersonOutcomes)
	}

	logrus.Info("Writing outcome file")
	err = writeOutcomeFile(countries, branchOutcomeEntries, outomesFilename)
	if err != nil {
		logrus.WithError(err).Error("Failed to write outcome file")
		return
	}

	logrus.WithFields(logrus.Fields{
		"path": outomesFilename,
	}).Info("Wrote outcome file")
}

func readCountryFile(path string) (map[string]string, map[string]int, error) {
	countriesByCode := make(map[string]string)
	countryOutcomes := make(map[string]int)

	countryFile, err := os.Open(path)
	if err != nil {
		return nil, nil, err
	}
	defer countryFile.Close()

	reader := csv.NewReader(bufio.NewReader(countryFile))
	for {
		line, err := reader.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, nil, err
		}

		country := line[0]
		code := line[1]

		if code == "DNQ" {
			countryOutcomes[country] = 0
		} else {
			countriesByCode[code] = country
		}
	}

	return countriesByCode, countryOutcomes, nil
}

func readPicksFile(path string) (map[string]Picks, error) {
	picksByPerson := make(map[string]Picks)

	picksFile, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer picksFile.Close()

	reader := csv.NewReader(bufio.NewReader(picksFile))
	firstLine, err := reader.Read()
	if err != nil {
		err = errors.New("Failed to read first line of picks file")
		return nil, err
	}

	// Dynamically collects all countries from picks file in the order they appear in columns
	var countries []string
	countryCaptureRegex := regexp.MustCompile("\\[([A-Z]+)\\]")
	for i := 3; i < len(firstLine); i++ {
		country := countryCaptureRegex.FindStringSubmatch(firstLine[i])[1]
		countries = append(countries, country)
	}

	// Reads file line-by-line, creating set of entries for each person's round prediction for each country
	// while also separately capturing each person's predictions for semifinalists, finalists, and winner for
	// efficient retrieval later
	for {
		line, err := reader.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, err
		}

		name := line[2]
		picks := Picks{
			OutcomesByCountry: make(map[string]int),
		}

		roundValues := map[string]int{
			"Group Stage Contender (16)": 0,
			"Round of 16 (8)":            1,
			"Quarter Finalist (4)":       2,
			"Semi Finalist (2)":          3,
			"Second (1)":                 4,
			"First (1)":                  4,
		}
		for i := 3; i < len(line); i++ {
			country := countries[i-3]
			elimRound := roundValues[line[i]]
			picks.OutcomesByCountry[country] = elimRound

			if line[i] == "First (1)" {
				picks.Winner = country
				picks.Finalists = append(picks.Finalists, country)
				picks.SemiFinalists = append(picks.SemiFinalists, country)
			} else if line[i] == "Second (1)" {
				picks.Finalists = append(picks.Finalists, country)
				picks.SemiFinalists = append(picks.SemiFinalists, country)
			} else if line[i] == "Semi Finalist (2)" {
				picks.SemiFinalists = append(picks.SemiFinalists, country)
			}
		}

		picksByPerson[name] = picks
	}

	return picksByPerson, nil
}

// Reads csv file containing set of knockout stage matches that have already been decided
// Format of each line is:
// <countryA>,<countryB>,<winner>,<loser>
// Order of countryA and countryB does not matter; the resulting map will contain entries for both orders
func readDecidedMatchesFile(path string) (map[[2]string][2]string, error) {
	decidedMatches := make(map[[2]string][2]string)

	decidedMatchesFile, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer decidedMatchesFile.Close()

	reader := csv.NewReader(bufio.NewReader(decidedMatchesFile))
	for {
		line, err := reader.Read()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, err
		}

		countries := [2]string{line[0], line[1]}
		countriesReversed := [2]string{line[1], line[0]}
		outcome := [2]string{line[2], line[3]}
		decidedMatches[countries] = outcome
		decidedMatches[countriesReversed] = outcome
	}

	return decidedMatches, nil
}

// Calculates all possible branches given the first round match pairings and the decided matches
func findAllBranches(currOutcomes map[string]int, firstRoundMatches [][2]string, decidedMatches map[[2]string][2]string) []Branch {
	branches := []Branch{
		Branch{
			CountryOutcomes:  currOutcomes,
			CurrRoundMatches: firstRoundMatches,
		},
	}

	for roundNum := 1; roundNum <= 4; roundNum++ {
		var nextRoundBranches []Branch
		for _, branch := range branches {
			currRoundOutcomeSets := [][]MatchOutcome{
				[]MatchOutcome{},
			}

			for _, match := range branch.CurrRoundMatches {
				var newOutcomeSets [][]MatchOutcome
				outcome, matchDecided := decidedMatches[match]

				// If the current match outcome is undecided, generates two outcome sets for each of the current
				// outcome sets (one where country A wins and another where country B wins)
				// If it's already decided, simply extends each of the current outcome sets with the decided match
				// result
				for _, outcomeSet := range currRoundOutcomeSets {
					if matchDecided {
						var newOutcomeSet []MatchOutcome
						for _, outcome := range outcomeSet {
							newOutcomeSet = append(newOutcomeSet, MatchOutcome{
								Winner: outcome.Winner,
								Loser:  outcome.Loser,
							})
						}
						newOutcomeSet = append(newOutcomeSet, MatchOutcome{
							Winner: outcome[0],
							Loser:  outcome[1],
						})
						newOutcomeSets = append(newOutcomeSets, newOutcomeSet)
					} else {
						var firstOutcomeSet []MatchOutcome
						var secondOutcomeSet []MatchOutcome
						for _, outcome := range outcomeSet {
							firstOutcomeSet = append(firstOutcomeSet, MatchOutcome{
								Winner: outcome.Winner,
								Loser:  outcome.Loser,
							})

							secondOutcomeSet = append(secondOutcomeSet, MatchOutcome{
								Winner: outcome.Winner,
								Loser:  outcome.Loser,
							})
						}

						firstOutcomeSet = append(firstOutcomeSet, MatchOutcome{
							Winner: match[0],
							Loser:  match[1],
						})

						secondOutcomeSet = append(secondOutcomeSet, MatchOutcome{
							Winner: match[1],
							Loser:  match[0],
						})

						newOutcomeSets = append(newOutcomeSets, firstOutcomeSet)
						newOutcomeSets = append(newOutcomeSets, secondOutcomeSet)
					}
				}

				currRoundOutcomeSets = newOutcomeSets
			}

			// Uses each of the outcome sets to determine a new set of branches
			for _, outcomeSet := range currRoundOutcomeSets {
				// Creates copy of current outcome set
				outcomeSetCountryOutcomes := make(map[string]int)
				for country, result := range branch.CountryOutcomes {
					outcomeSetCountryOutcomes[country] = result
				}

				var outcomeSetNextMatches [][2]string
				var winner string
				finalists := make(map[string]bool)
				semiFinalists := make(map[string]bool)

				// Copies semi-finalists if determined in previous round
				if branch.SemiFinalists != nil {
					semiFinalists = branch.SemiFinalists
				}

				// Updates result table for newly-eliminated countries and adds semifinalists, finalists, or winner
				// if possible
				if len(outcomeSet) > 1 {
					for _, outcome := range outcomeSet {
						outcomeSetCountryOutcomes[outcome.Loser] = roundNum
					}

					for pairNum := 0; pairNum < len(outcomeSet)/2; pairNum++ {
						firstResult := outcomeSet[2*pairNum]
						secondResult := outcomeSet[2*pairNum+1]
						outcomeSetNextMatches = append(outcomeSetNextMatches, [2]string{firstResult.Winner, secondResult.Winner})
					}

					if len(outcomeSet) == 2 {
						semiFinalists[outcomeSet[0].Winner] = true
						semiFinalists[outcomeSet[0].Loser] = true
						semiFinalists[outcomeSet[1].Winner] = true
						semiFinalists[outcomeSet[1].Loser] = true
					}
				} else {
					outcomeSetCountryOutcomes[outcomeSet[0].Loser] = roundNum
					outcomeSetCountryOutcomes[outcomeSet[0].Winner] = roundNum
					winner = outcomeSet[0].Winner
					finalists[outcomeSet[0].Loser] = true
					finalists[outcomeSet[0].Winner] = true
				}

				// Adds branch to set for next loop iteration
				nextRoundBranches = append(nextRoundBranches, Branch{
					CountryOutcomes:  outcomeSetCountryOutcomes,
					CurrRoundMatches: outcomeSetNextMatches,
					Winner:           winner,
					Finalists:        finalists,
					SemiFinalists:    semiFinalists,
				})
			}
		}

		branches = nextRoundBranches
	}

	return branches
}

// Calculates a score given a person's cup predictions and the actual outcomes of a branch
func findPersonBranchOutcome(branch Branch, personPicks map[string]int, chosenWinner string, chosenFinalists []string, chosenSemiFinalists []string) PersonOutcome {
	personOutcome := PersonOutcome{
		CountryScores: make(map[string]int),
	}

	for country, pick := range personPicks {
		actualResult := branch.CountryOutcomes[country]
		diff := int(math.Abs(float64(pick - actualResult)))

		if diff == 0 {
			personOutcome.CountryScores[country] = 7
		} else if diff == 1 {
			personOutcome.CountryScores[country] = 4
		} else if diff == 3 {
			personOutcome.CountryScores[country] = -5
		} else if diff == 4 {
			personOutcome.CountryScores[country] = -11
		}
	}

	if chosenWinner == branch.Winner {
		personOutcome.WinnerBonus = 25
	}

	correctSemiFinalists := numCorrect(chosenSemiFinalists, branch.SemiFinalists)
	personOutcome.SemiFinalistsBonus = 20 * correctSemiFinalists / 4

	correctFinalists := numCorrect(chosenFinalists, branch.Finalists)
	if correctFinalists == 2 {
		personOutcome.FinalistsBonus = 11
	} else if correctFinalists == 1 {
		personOutcome.FinalistsBonus = 4
	}

	return personOutcome
}

// Compares two sets and returns the number of elements in common
func numCorrect(inputSet []string, actualSet map[string]bool) int {
	var numCorrect int

	for _, elem := range inputSet {
		if actualSet[elem] {
			numCorrect++
		}
	}

	return numCorrect
}

// Writes the result file
// The format of each line:
// <branchNum>,<personName>,<firstCountryScore>,...,<lastCountryScore>,<semiFinalistBonus>,<finalistBonus>,<winnerBonus>
func writeOutcomeFile(countries []string, branchPersonOutcomes []BranchPersonOutcomes, path string) error {
	outcomeFile, err := os.Create(path)
	if err != nil {
		return err
	}
	defer outcomeFile.Close()

	writer := csv.NewWriter(outcomeFile)
	defer writer.Flush()

	firstLine := []string{"BranchNum", "Person"}
	for _, country := range countries {
		firstLine = append(firstLine, country)
	}
	firstLine = append(firstLine, []string{"Semi-final bonus", "Final bonus", "Winner bonus"}...)

	err = writer.Write(firstLine)
	if err != nil {
		return err
	}

	for index, branch := range branchPersonOutcomes {
		for person, outcome := range branch.PersonOutcomes {
			line := []string{strconv.Itoa(index + 1), person}
			for _, country := range countries {
				line = append(line, strconv.Itoa(outcome.CountryScores[country]))
			}

			line = append(line, strconv.Itoa(outcome.SemiFinalistsBonus))
			line = append(line, strconv.Itoa(outcome.FinalistsBonus))
			line = append(line, strconv.Itoa(outcome.WinnerBonus))

			err = writer.Write(line)
			if err != nil {
				return err
			}
		}
	}

	return nil
}
