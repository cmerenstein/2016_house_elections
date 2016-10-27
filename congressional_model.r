library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

polls <- read.csv("president_general_polls_2016.csv") %>% 
  mutate(enddate = as.Date(enddate, "%m/%d/%Y")) %>% 
  mutate(adjpoll_diff = (adjpoll_clinton - adjpoll_trump))
natl_polls <- polls %>% filter(state == "U.S.") 

clinton_natl <- ggplot(natl_polls, aes(x = enddate, y = adjpoll_diff)) +
  geom_point() + scale_x_date(date_labels = "%b %d") +
  geom_smooth()
clinton_natl

weighted.mean(natl_polls$adjpoll_diff, natl_polls$poll_wt)

state_polling_diff <- group_by(polls, state) %>%  summarize(wt_mean = weighted.mean(adjpoll_diff, poll_wt))

ggplot(state_polling_diff, aes(x = state, y = wt_mean)) +
  geom_bar(stat = "identity") + coord_flip()
  
races <- read.csv("CandidateSummaryAction.csv", quote = "", row.names = NULL) %>% 
  filter(can_off == "\"H\"") %>% 
  filter(can_par_aff %in% c("\"REP\"", "\"DEM\""))
races <- as.data.frame(apply(races,2,function(x)gsub('\"', '',x)))

non_dem_D = c("DCG", "DFL", "IDA", "IDE", "IDP", "IND", "PPD") #easiest way to get to just rep/dem

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

races_2012_diff <- group_by(races_2012, D, STATE, PARTY) %>% summarise(general = max(GENERAL..)) %>% 
  spread(PARTY, general) %>% 
  filter(!is.na(DEM)) %>% 
  filter(!is.na(REP)) %>% 
  mutate(diff_2012 = DEM - REP ) %>% 
  mutate(national_diff_2012 = diff_2012 - GENERIC_BALLOT_2012) %>% 
  mutate(dist_state = paste(D, STATE, sep = "_")) %>% 
  ungroup() %>% 
  left_join(incumbent_party_2012, by=c("D" = "D", "STATE" = "STATE"))

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
  select(`D`, `STATE`, `STATE.ABBREVIATION`, `PARTY`, `GENERAL..`) %>% 
  mutate(key = paste(D, STATE.ABBREVIATION, PARTY, sep = "-")) 

races_2014_diff <- group_by(races_2014, D, STATE, PARTY) %>% summarise(general = max(GENERAL..)) %>% 
  spread(PARTY, general) %>% 
  filter(!is.na(DEM)) %>% 
  filter(!is.na(REP)) %>% 
  mutate(diff_2014 = DEM - REP ) %>% 
  mutate(national_diff_2014 = diff_2014 - GENERIC_BALLOT_2014) %>% 
  mutate(dist_state = paste(D, STATE, sep = "_")) %>% 
  ungroup()

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

# this replaces "AL" with "00" for at large districts
single_district <- group_by(PVI_08_12, ST) %>% summarize(n = n()) %>% 
  filter(n == 1)
single_district <- single_district$ST

PVI_08_12 <- mutate(PVI_08_12, D = ifelse(ST %in% single_district, "00", D)) %>% 
  mutate(CD = paste(ST, D, sep="-"))
PVI_only <- select(PVI_08_12, CD, PVI)

RCP_GENERIC_BALLOT_2016 = 5.0

races_2016 <- left_join(races_2016, PVI_only, by="CD") 
races_2016$PVI_outcome = races_2016$PVI + RCP_GENERIC_BALLOT_2016

dems = select(races_2016, `CD`, `PVI_outcome`) %>% 
  unique() %>% 
  filter(PVI_outcome > 0) %>% 
  nrow()
rep = select(races_2016, `CD`, `PVI_outcome`) %>% 
  unique() %>% 
  filter(PVI_outcome < 0) %>% 
  nrow()

incumbents = 

