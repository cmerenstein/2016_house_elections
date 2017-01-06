library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)

#races <- read.csv("CandidateSummaryAction.csv", quote = "", row.names = NULL) %>% 
#  filter(can_off == "\"H\"") %>% 
#  filter(can_par_aff %in% c("\"REP\"", "\"DEM\""))
#races <- as.data.frame(apply(races,2,function(x)gsub('\"', '',x)))

non_dem_D = c("DCG", "IDA", "IDE", "IDP", "IND", "PPD") #easiest way to get to just rep/dem

GENERIC_BALLOT_2012 = 1.2 #source RCP

races_2012 <- read.csv("2012congresults.csv") %>%
filter(GENERAL.VOTES != "") %>%
  mutate(PARTY = sub("*", "", PARTY)) %>%
  filter((PARTY %in% non_dem_D) == F) %>%
  mutate(PARTY = ifelse(grepl("D",PARTY), "DEM", "REP")) %>%
  filter(PARTY %in% c("DEM", "REP")) %>%
  mutate(GENERAL.. = as.numeric(sub("%", "", GENERAL..))) %>%
  mutate(GENERAL.. = ifelse(GENERAL.VOTES == "Unopposed", 100, GENERAL..)) %>%
  filter(TOTAL.VOTES == "") %>%
  filter(FEC.ID. != "n/a") %>%
  filter(D != "S") %>%
  select(`D`, `STATE`, `STATE.ABBREVIATION`, `PARTY`, `GENERAL..`, `X.I.`) %>%
  mutate(key = paste(D, STATE.ABBREVIATION, PARTY, sep = "-"))

incumbent_party_2012 <- filter(races_2012, GENERAL.. > 0) %>%
  group_by(D, STATE) %>%
  mutate(I.P.2012 = ifelse(X.I. == "(I)", PARTY, "")) %>%
  filter(I.P.2012 != "") %>%
  select(`I.P.2012`, `D`, `STATE`)

multiple_i_2012 <- group_by(incumbent_party_2012, D, STATE) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  mutate(key = paste(D, STATE, sep = "-"))

incumbent_party_2012 <- mutate(incumbent_party_2012, I.P.2012 = ifelse(paste(D, STATE, sep="-") %in% multiple_i_2012$key, NA, I.P.2012))

races_2012_diff <- group_by(races_2012, D, STATE, PARTY) %>% summarise(general = max(GENERAL..)) %>%
  spread(PARTY, general) %>%
  filter(!is.na(DEM)) %>%
  filter(!is.na(REP)) %>%
  mutate(diff_2012 = DEM - REP ) %>%
  mutate(national_diff_2012 = diff_2012 - GENERIC_BALLOT_2012) %>%
  mutate(dist_state = paste(D, STATE, sep = "_")) %>%
  ungroup() %>%
  left_join(incumbent_party_2012, by=c("D" = "D", "STATE" = "STATE"))

## it's maybe dumb that I duplicated this. I should have just made year a variable, but I didn't and now I don't have time to fix it.
## it made it easier to do some things but makes it less neat. If I had more than 2 years it would definitely be necessary

GENERIC_BALLOT_2014 = -5.7 #source RCP

races_2014 <- read.csv("2014congresults.csv") %>%
  filter(GENERAL.VOTES != "") %>%
  mutate(PARTY = sub("*", "", PARTY)) %>%
  filter((PARTY %in% non_dem_D) == F) %>%
  mutate(PARTY = ifelse(grepl("D",PARTY), "DEM", "REP")) %>%
  filter(PARTY %in% c("DEM", "REP")) %>%
  mutate(GENERAL.. = as.numeric(sub("%", "", GENERAL..))) %>%
  mutate(GENERAL.. = ifelse(GENERAL.VOTES == "Unopposed", 100, GENERAL..)) %>%
  filter(TOTAL.VOTES == "") %>%
  filter(FEC.ID. != "n/a") %>%
  filter(D != "S") %>%
  select(`D`, `STATE`, `STATE.ABBREVIATION`, `PARTY`, `GENERAL..`, `X.I.`) %>%
  mutate(key = paste(D, STATE.ABBREVIATION, PARTY, sep = "-"))

incumbent_party_2014 <- filter(races_2014, GENERAL.. > 0) %>%
  group_by(D, STATE) %>%
  mutate(I.P.2014 = ifelse(X.I. == "(I)", PARTY, "")) %>%
  filter(I.P.2014 != "") %>%
  select(`I.P.2014`, `D`, `STATE`)

multiple_i_2014 <- group_by(incumbent_party_2014, D, STATE) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  mutate(key = paste(D, STATE, sep = "-"))

incumbent_party_2014 <- mutate(incumbent_party_2014, I.P.2014 = ifelse(paste(D, STATE, sep="-") %in% multiple_i_2014$key, NA, I.P.2014))


races_2014_diff <- group_by(races_2014, D, STATE, PARTY) %>% summarise(general = max(GENERAL..)) %>%
  spread(PARTY, general) %>%
  filter(!is.na(DEM)) %>%
  filter(!is.na(REP)) %>%
  mutate(diff_2014 = DEM - REP ) %>%
  mutate(national_diff_2014 = diff_2014 - GENERIC_BALLOT_2014) %>%
  mutate(dist_state = paste(D, STATE, sep = "_")) %>%
  ungroup() %>%
  left_join(incumbent_party_2014, by=c("D" = "D", "STATE" = "STATE"))

past_races <- full_join(races_2012_diff, races_2014_diff, by = "dist_state") %>%
  mutate(past_national_diff = ifelse(is.na(national_diff_2012), national_diff_2014, ifelse(is.na(national_diff_2014), national_diff_2012, (national_diff_2014 + national_diff_2012)/2)))


races_2016 <- read.csv("2016_house_race_general.csv") %>% 
  filter(STATE.ABBREVIATION != "" & STATE.ABBREVIATION != "STATE ABBREVIATION") %>% 
  filter(D != "S") %>% 
  mutate(CD = paste(STATE.ABBREVIATION, D, sep = "-"))


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
# simple sub() would have replaced AL in alabama. whatever
single_district <- group_by(PVI_08_12, ST) %>% summarize(n = n()) %>% 
  filter(n == 1)
single_district <- single_district$ST

PVI_08_12 <- mutate(PVI_08_12, D = ifelse(ST %in% single_district, "00", D)) %>% 
  mutate(CD = paste(ST, D, sep="-"))
PVI_only <- select(PVI_08_12, CD, PVI)

RCP_GENERIC_BALLOT_2016 = 1.4
polling_errors = c(3.3, 1.4, 2.6, 1.7, 3.6, 2.6, 2.9)
GEN_BAL_SD <- sd(polling_errors) # polling errors are symetric, so mean is set to 0

races_2016 <- left_join(races_2016, PVI_only, by="CD") 
races_2016$PVI_outcome = races_2016$PVI + RCP_GENERIC_BALLOT_2016

state_abbreviations = cbind(state.abb, state.name) %>%
  data.frame() %>% 
  mutate(state.abb = as.character(state.abb)) %>% 
  mutate(state.name = as.character(state.name))

past_races <- inner_join(past_races, state_abbreviations, by=c("STATE.x" = "state.name")) %>%
  mutate(CD = paste(state.abb, D.x, sep = "-")) %>%
  mutate(CD = sub(" ", "", CD)) %>%
  left_join(select(PVI_08_12, `PVI`, `CD`), by="CD") %>%
  unique()

tmp_2012 <- select(past_races, `PVI`, `diff_2012`, `I.P.2012`) %>%
  rename(diff = diff_2012) %>%
  rename(I.P. = I.P.2012) %>%
  mutate(year = 2012)

tmp_2014 <- select(past_races, `PVI`, `diff_2014`, `I.P.2014`) %>%
  rename(diff = diff_2014) %>%
  rename(I.P. = I.P.2014) %>%
  mutate(year = 2014)

incumbent_effect <- do.call("rbind", list(tmp_2012, tmp_2014)) %>% 
  mutate(PVI_expect = ifelse(year == 2012, PVI + GENERIC_BALLOT_2012, PVI + GENERIC_BALLOT_2014)) %>% 
  mutate(PVI_diff = diff - PVI_expect) %>%
  filter(!is.na(PVI_diff )) %>% 
  mutate(I.P. = ifelse(is.na(I.P.), "none", I.P.)) 

incumbent_by_party <- group_by(incumbent_effect, I.P.) %>% summarize(median = median(PVI_diff), n = n())

open_mean_diff = as.numeric(incumbent_by_party[2,2])

incumbent_by_party <- incumbent_by_party %>% filter(I.P. != "none") %>% 
  mutate(diff_from_open = abs(median - open_mean_diff))

total_incumbent_effect = weighted.mean(incumbent_by_party$diff_from_open, incumbent_by_party$n)

incumbent_party_2016 <- races_2016 %>% 
  mutate(I.P. = ifelse(X.I. == "(I)", as.character(PARTY), "")) %>% 
  filter(I.P. != "") %>% 
  select(`I.P.`, `CD`)

races_2016 <- left_join(races_2016, incumbent_party_2016, by = "CD") 


I.E.DEM = 6.36
I.E.DEM.sd = 13.79601
I.E.REP = -6.12
I.E.REP.sd = 13.79601
I.E.OPN = 0 # this is sort of conjecture? actual is -0.48
I.E.OPN.sd = 19.36249
# sd based on full history, mean based just on close races

races_2016 <- mutate(races_2016, I.P. = substr(I.P., 1, 3)) %>% 
  mutate(I.P. = ifelse(is.na(I.P.), "OPN", I.P.)) %>% 
  mutate(PVI_outcome_i_adj = ifelse(I.P. == "REP", PVI_outcome + I.E.REP, ifelse(I.P. == "DEM", PVI_outcome + I.E.DEM, PVI_outcome + I.E.OPN))) %>% 
  mutate(FEC.ID. = gsub(" ", "", as.character(FEC.ID.)))

winners <- races_2016 %>% filter(!is.na(PVI)) %>% 
  group_by(CD) %>% summarize(margin = mean(PVI_outcome_i_adj)) %>% 
  mutate(winner = ifelse(margin > 0, "DEM", "REP")) %>% 
  group_by(winner) %>% summarize(seats = n())

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

for (i in seq(1, 5000)) {
  GENERIC_BALLOT_MARGIN = 1.4 + rnorm(1, 0, sd = GEN_BAL_SD)
  #cat(GENERIC_BALLOT_MARGIN)
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

outcome_odds <- group_by(all_runs, CD, I.P.) %>% summarize(D_win = sum(PVI_adj > 0) / n())

outcome_odds <- outcome_odds %>%
  select(`CD`, `D_win`) %>% 
  unique()


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

#major_party_can <- mutate(races_2016, PARTY = substr(PARTY, 1,3)) %>% 
#  mutate(PARTY = ifelse(PARTY %in% c("DFL", "DNL"), "DEM", PARTY)) %>% 
#  filter(PARTY %in% c("REP", "DEM"))
#major_party_can <- major_party_can$FEC.ID.


tmp <- select(races_2016, `FEC.ID.`)
FEC_2016 <- inner_join(tmp, FEC_2016, by = c("FEC.ID." = "can_id"))

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

FEC_ind_con_2016 <- group_by(FEC_2016, CD, PARTY) %>% summarize(ind_con = max(ind_con)) %>% 
  spread(PARTY, ind_con) %>% 
  mutate(DEM = ifelse(is.na(DEM), 0, DEM)) %>% 
  mutate(REP = ifelse(is.na(REP), 0, REP)) %>% 
  mutate(ind_con_diff = DEM - REP)

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

PVI_WEIGHT = 0.67
CONTRIBUTIONS_WEIGHT = 0.33

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


