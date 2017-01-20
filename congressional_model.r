library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)

# List of everyone who's running for a congressional seat in 2016
# need to do some filtering to get just House, not senate
races_2016 <- read.csv("2016_house_race_general.csv") %>%
  filter(STATE.ABBREVIATION != "" & STATE.ABBREVIATION != "STATE ABBREVIATION") %>%
  filter(D != "S") %>%
  mutate(CD = paste(STATE.ABBREVIATION, D, sep = "-"))

# First we calculate Partisan Voter Index, based on the lean of the district in past presidental races
OBAMA_MARGIN_2008 = 7.2
OBAMA_MARGIN_2012 = 3.9

PVI_08_12 <- read.csv("2008_2012_pres_by_2012_districts.csv") %>%
  mutate(diff_2012 = Obama.2012 - Romney.2012) %>%
  mutate(diff_2008 = Obama.2008 - McCain.2008) %>%
  mutate(PVI_2008 = diff_2008 - OBAMA_MARGIN_2008) %>%
  mutate(PVI_2012 = diff_2012 - OBAMA_MARGIN_2012) %>%
  mutate(PVI = (PVI_2008 + PVI_2012)/2) %>%
  separate(CD, into = c("ST", "D"), sep = "-")

# this replaces "AL" with "00" for at large districts.
# simple sub() would have replaced AL in alabama. 
single_district <- group_by(PVI_08_12, ST) %>% summarize(n = n()) %>%
  filter(n == 1)
single_district <- single_district$ST

# This gets the PVI from 08-12 for all our districts
PVI_08_12 <- mutate(PVI_08_12, D = ifelse(ST %in% single_district, "00", D)) %>%
  mutate(CD = paste(ST, D, sep="-"))
PVI_only <- select(PVI_08_12, CD, PVI)

## Polling errors are from RCP going back to 2002
RCP_GENERIC_BALLOT_2016 = 1.4
polling_errors = c(3.3, 1.4, 2.6, 1.7, 3.6, 2.6, 2.9)
GEN_BAL_SD <- sd(polling_errors) # polling errors are symetric, so mean is set to 0

## Most simple outcome predictor is just the generic ballot + the PVI of a district
races_2016 <- left_join(races_2016, PVI_only, by="CD") 
races_2016$PVI_outcome = races_2016$PVI + RCP_GENERIC_BALLOT_2016

## Honestly, I forget if this is still used, but it's just a df of state names and abbreviations
state_abbreviations = cbind(state.abb, state.name) %>%
  data.frame() %>% 
  mutate(state.abb = as.character(state.abb)) %>% 
  mutate(state.name = as.character(state.name))

## The incbument wins more often, so we need to determine what party is the incumbent in each district
incumbent_party_2016 <- races_2016 %>% 
  mutate(I.P. = ifelse(X.I. == "(I)", as.character(PARTY), "")) %>% 
  filter(I.P. != "") %>% 
  select(`I.P.`, `CD`)

races_2016 <- left_join(races_2016, incumbent_party_2016, by = "CD") 

## To handle the effect of incumbancy I used incumbent_effect.r
## The numbers below are the additive effect of being incumbent on the democratic margin of victory (or loss)
## The numbers are generated for each election going back to 2004.
## The means are just based on "close races" where the margin of victory was less than 20 points
## This makes it more accurate in predicting close races, and doesn't really deminish accuracy in uncompetitive races
I.E.DEM = 6.36
I.E.DEM.sd = 13.79601
I.E.REP = -6.12
I.E.REP.sd = 13.79601
I.E.OPN = 0 # this is sort of conjecture? actual is -0.48
I.E.OPN.sd = 19.36249
# sd based on full history, mean based just on close races

# This adjusts the outcome expected by PVI to reflect incumbancy effect
races_2016 <- mutate(races_2016, I.P. = substr(I.P., 1, 3)) %>% 
  mutate(I.P. = ifelse(is.na(I.P.), "OPN", I.P.)) %>% 
  mutate(PVI_outcome_i_adj = ifelse(I.P. == "REP", PVI_outcome + I.E.REP, ifelse(I.P. == "DEM", PVI_outcome + I.E.DEM, PVI_outcome + I.E.OPN))) %>% 
  mutate(FEC.ID. = gsub(" ", "", as.character(FEC.ID.)))

winners <- races_2016 %>% filter(!is.na(PVI)) %>% 
  group_by(CD) %>% summarize(margin = mean(PVI_outcome_i_adj)) %>% 
  mutate(winner = ifelse(margin > 0, "DEM", "REP")) %>% 
  group_by(winner) %>% summarize(seats = n())

# This helps check if prediction passes the eye test, if we're predicting way too many or too few incumbents winning
winners_by_incumbent <- races_2016 %>% group_by(CD, I.P.) %>% summarize(margin = max(PVI_outcome_i_adj)) %>% 
  mutate(inc = ifelse(I.P. != "OPN", T, F)) %>% 
  mutate(inc_win = ifelse(inc, ifelse(I.P. == "REP", (margin < 0), (margin > 0)), F)) %>% 
  filter(inc) %>% 
  group_by(inc_win) %>%  summarize(n = n())

races_2016_pvi = select(races_2016, `CD`, `I.P.`, `PVI`)

predict_df = list()
inc = list()
dem_seats_pvi = c()
rep_seats_pvi = c()

## The "winners" predicted above are deterministic, just who would win by PVI and incumbent
## Here, we actually run through simulations and figure out how many times each side wins
## It's based on the standard deviations of the generic ballot and the incumbant effects
for (i in seq(1, 5000)) {
  GENERIC_BALLOT_MARGIN = 1.4 + rnorm(1, 0, sd = GEN_BAL_SD)
  races_predict <- mutate(races_2016_pvi, PVI_outcome = PVI + GENERIC_BALLOT_MARGIN) %>% 
    mutate(PVI_adj = ifelse(I.P. == "REP", PVI_outcome + rnorm(1, I.E.REP, I.E.REP.sd), ifelse(I.P. == "DEM", PVI_outcome + rnorm(1, I.E.DEM, I.E.DEM.sd), PVI_outcome + rnorm(1, I.E.OPN, I.E.OPN.sd ))))

  winners_by_incumbent <- races_predict %>% group_by(CD, I.P.) %>% summarize(margin = max(PVI_adj)) %>% 
    mutate(inc = ifelse(I.P. != "OPN", T, F)) %>% 
    mutate(inc_win = ifelse(inc, ifelse(I.P. == "REP", (margin < 0), (margin > 0)), F)) %>% 
    filter(inc) %>% 
    group_by(inc_win) %>%  summarize(n = n())
  inc[[i]] = winners_by_incumbent
  
  results_PVI <- select(races_predict, `CD`, `PVI_adj`) %>% 
    unique()
  dem_seats_pvi[i] = sum(results_PVI$PVI_adj > 0, na.rm = T)
  rep_seats_pvi[i] = sum(results_PVI$PVI_adj < 0, na.rm = T)
  
  predict_df[[i]] = races_predict
}
mean_dem_seats_pvi = mean(dem_seats_pvi)
mean_rep_seats_pvi = mean(rep_seats_pvi)

all_runs <- do.call("rbind", predict_df)

# For each race, get the percent of the time the predicted Dem margin is > than 0
outcome_odds <- group_by(all_runs, CD, I.P.) %>% summarize(D_win = sum(PVI_adj > 0) / n())

outcome_odds <- outcome_odds %>%
  select(`CD`, `D_win`) %>% 
  unique()

## The second factor under consideration is how much money each candidate has raised
## The fundraising file from the FEC is really messy, requires a lot of cleaning up
FEC_2016 <- read.csv("2016CandidateSummaryAction.csv", stringsAsFactors = F, row.names = NULL) %>% 
  filter(can_off == "H") %>% 
  rename(ST = can_off_sta) %>% 
  rename(D = can_off_dis) %>% 
  rename(PARTY = can_par_aff) %>% 
  mutate(D = str_pad(as.character(D), 2, side = "left", "0")) %>% 
  select(`can_id`, `ST`, `D`, `tot_con`, `can_con`, `cas_on_han_clo_of_per`, `PARTY`) %>% 
  mutate(can_id = gsub(" ", "", can_id)) %>% 
  mutate(PARTY = ifelse(PARTY %in% c("DFL", "DNL"), "DEM", PARTY)) %>% 
  filter(PARTY %in% c("DEM", "REP"))

tmp <- select(races_2016, `FEC.ID.`)
FEC_2016 <- inner_join(tmp, FEC_2016, by = c("FEC.ID." = "can_id"))

# just losts of cleaning stuff up going on here
FEC_2016 <- FEC_2016 %>% 
  mutate(tot_con_cp = tot_con) %>% 
  mutate(tot_con = gsub("\\$", "", tot_con)) %>% 
  mutate(tot_con = ifelse(tot_con == "", "0", tot_con)) %>% 
  mutate(tot_con = as.numeric(gsub(",","", tot_con))) %>% 
  mutate(can_con = gsub("\\$", "", can_con)) %>%
  mutate(can_con = ifelse(can_con == "", "0", can_con)) %>% 
  mutate(can_con = as.numeric(gsub(",", "", can_con))) %>% 
  mutate(cas_on_han_clo_of_per = gsub("\\$", "", cas_on_han_clo_of_per)) %>% 
  mutate(cas_on_han_clo_of_per = ifelse(cas_on_han_clo_of_per == "", "0", cas_on_han_clo_of_per)) %>% 
  mutate(cas_on_han_clo_of_per = gsub(",", "", cas_on_han_clo_of_per)) %>% 
  mutate(ind_con = tot_con - can_con) %>% 
  mutate(CD = paste(ST, D, sep = "-"))

# Gets the contributions to each major party candidate
FEC_ind_con_2016 <- group_by(FEC_2016, CD, PARTY) %>% summarize(ind_con = max(ind_con)) %>% 
  spread(PARTY, ind_con) %>% 
  mutate(DEM = ifelse(is.na(DEM), 0, DEM)) %>% 
  mutate(REP = ifelse(is.na(REP), 0, REP)) %>% 
  mutate(ind_con_diff = DEM - REP)

# These were generated in money_in_elections.r
# They're based off of a linear model between contributinos and Dem margin of victory
# A major inaccuracy of this model is that the FEC filing doesn't differentiate between
# money raised in primaries and the general election
FEC_ind_con_factor = 1.342e-05
FEC_ind_con_intercept = 3.705e+00

FEC_ind_con_2016 <- mutate(FEC_ind_con_2016, ind_con_predict = (ind_con_diff * FEC_ind_con_factor) + FEC_ind_con_intercept) %>% 
  full_join(races_2016_pvi, by = "CD") %>% 
  unique()

winners_funding <- group_by(FEC_ind_con_2016, CD) %>% summarise(diff = max(ind_con_predict, na.rm = T)) %>% 
  mutate(winner = ifelse(diff > 0, "DEM", "REP")) %>% 
  group_by(winner) %>% summarize(n = n())
  
CON_SD = 28.72477

con_results_list = list()

dem_seats_con = c()
rep_seats_con = c()

# This handles the simulation based on campaign contributions raised
for (i in seq(1,5000)) {
  results_contributions <- select(FEC_ind_con_2016, `CD`, `ind_con_predict`, `I.P.`) %>% 
    mutate(result = ind_con_predict + rnorm(1, mean = 0, sd = CON_SD))
  dem_seats = sum(results_contributions$result > 0, na.rm = T)
  rep_seats = sum(results_contributions$result < 0, na.rm = T)
  dem_seats_con[i] <- dem_seats
  rep_seats_con[i] = rep_seats
  
  con_results_list[[i]] = results_contributions
}

mean_dem_seats_con = mean(dem_seats_con)
mean_rep_seats_con = mean(rep_seats_con)

outcome_odds_contributions = do.call("rbind", con_results_list) %>% 
  group_by(CD) %>% summarize(D_win = sum(result > 0) / n())

## The weights are only half emperical.
# I didn't have time to see the correlation between PVI/Incumbent and outcome
# but contributions R^2 was 0.3319 so I just gave the rest to PVI/Incumbent.
PVI_WEIGHT = 0.67
CONTRIBUTIONS_WEIGHT = 0.33

# There's a separate weighting for races that aren't expected to be close
# Contributions become less meaningful, as in not-close rases no one is raising much money
PVI_WEIGHT_notclose = 0.87
CON_WEIGHT_notclose = 0.13 # thse aren't based in r^2 like the others, but observation of the ind_con_diff x diff plot suggests this is more accurate
outcome_odds <- inner_join(outcome_odds, outcome_odds_contributions, by = "CD") %>% 
  mutate(final_odds = ifelse(D_win.x > .90 | D_win.x < .1, (D_win.x * PVI_WEIGHT_notclose) + (D_win.y * CON_WEIGHT_notclose), (D_win.x * PVI_WEIGHT) + (D_win.y * CONTRIBUTIONS_WEIGHT)))
    
outcome_odds_write <- select(outcome_odds, `CD`, `final_odds`) %>% 
  rename(D_win = final_odds)
write.csv(outcome_odds_write, "outcome_odds.csv")
  
dem_seats_con_wt = dem_seats_con * CONTRIBUTIONS_WEIGHT
dem_seats_pvi_wt = dem_seats_pvi * PVI_WEIGHT
dem_seats_both = dem_seats_con_wt + dem_seats_pvi_wt
dem_odds = sum(dem_seats_both > 218) / length(dem_seats_both)

mean_dem_seats = (mean_dem_seats_con * CONTRIBUTIONS_WEIGHT + mean_dem_seats_pvi * PVI_WEIGHT)
mean_rep_seats = (mean_rep_seats_con * CONTRIBUTIONS_WEIGHT + mean_rep_seats_pvi * PVI_WEIGHT)


