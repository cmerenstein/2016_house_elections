library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## The theory in here was that contributions were a proxy for support, more than
## spending money would win votes. Both are at play though. I had hoped to
## include outside spending but didn't have time.

## There's a lot of junk in here because I did a bunch of stuff at the start to
## see if it was going to be worthwhile.
## It replicates the whole PVI_expect thing all over again, as is done in incumbent_effect.r

## As a warning, I never figured out a way to separate out primary and general
## election contributions. This makes it so the challenger raises a lot more
## than the incumbent sometimes, especially in competetive districts, because
## they needed to win a primary.

## Also, as an aside: some people spend so much money and still lose!

OBAMA_MARGIN_2008 = 7.2
KERRY_MARGIN_2004 = -2.4

PVI_04_08 <- read.csv("president_by_CD_2000_2008.csv") %>% ## only going to use 04-08, can go back an do 00-04 if need be
  mutate(diff_08 = Obama - McCain) %>% 
  mutate(diff_04 = Kerry - Bush..04) %>% 
  mutate(PVI_08 = diff_08 - OBAMA_MARGIN_2008) %>% 
  mutate(PVI_04 = diff_04 - KERRY_MARGIN_2004) %>% 
  mutate(PVI = (PVI_08 + PVI_04)/2) %>% 
  rename(D = CD) %>% 
  mutate(D = str_pad(as.character(D), 2, side = "left", "0")) %>% 
  mutate(D = ifelse(D == "AL", "00", D)) %>% 
  mutate(CD = paste(State, D, sep="-"))

OBAMA_MARGIN_2008 = 7.2
OBAMA_MARGIN_2012 = 3.9

PVI_08_12 <- read.csv("2008_2012_pres_by_2012_districts.csv") %>% 
  mutate(diff_2012 = Obama.2012 - Romney.2012) %>% 
  mutate(diff_2008 = Obama.2008 - McCain.2008) %>% 
  mutate(PVI_2008 = diff_2008 - OBAMA_MARGIN_2008) %>% 
  mutate(PVI_2012 = diff_2012 - OBAMA_MARGIN_2012) %>% 
  mutate(PVI = (PVI_2008 + PVI_2012)/2) %>% 
  separate(CD, into = c("ST", "D"), sep = "-") %>% 
  mutate(D = ifelse(D == "AL", "00", D)) %>% 
  unite(CD, ST, D, sep = "-")

PVI_list = list()
PVI_list[[2004]] = PVI_04_08
PVI_list[[2006]] = PVI_04_08
PVI_list[[2008]] = PVI_04_08
PVI_list[[2010]] = PVI_04_08
PVI_list[[2012]] = PVI_08_12
PVI_list[[2014]] = PVI_08_12

generic_ballots = list()
generic_ballots[["2014"]] = -5.7
generic_ballots[["2012"]] = 1.2
generic_ballots[["2010"]] = -6.8
generic_ballots[["2008"]] = 10.7
generic_ballots[["2006"]] = 7.9
generic_ballots[["2004"]] = -2.6

state_abbreviations = cbind(state.abb, state.name) %>%
  data.frame() %>% 
  mutate(state.abb = as.character(state.abb)) %>% 
  mutate(state.name = as.character(state.name))

non_dem_D = c("DCG", "IDA", "IDE", "IDP", "IND", "PPD") #easiest way to get to just rep/dem

races_04_08 <- list.files(path = "past_elections/") #actually 04-14
FEC_08_14 <- list.files(path = "FEC_files/")

FEC_df_list = list()
for (csv in FEC_08_14){
  year = substr(csv, 1, 4)
  df <- read.csv(paste("FEC_files/", csv, sep=""), sep = ",", stringsAsFactors = F, row.names = NULL) %>% 
    filter(can_off == "H") %>% 
    rename(ST = can_off_sta) %>% 
    rename(D = can_off_dis) %>% 
    rename(PARTY = can_par_aff) %>% 
    mutate(D = str_pad(as.character(D), 2, side = "left", "0")) %>% 
    select(`can_id`, `ST`, `D`, `tot_con`, `can_con`, `cas_on_han_clo_of_per`, `PARTY`)
    
  FEC_df_list[[year]] = df
}

race_df_list = list()


for (csv in races_04_08){
  year = substr(csv, 1,4)
  PVI_df <- PVI_list[[as.numeric(year)]]
  GENERIC_BALLOT = generic_ballots[[year]]
  if (year >= as.numeric(2007)){
    FEC_df <- FEC_df_list[[year]]  
    
    races_df_full <- read.csv(paste("past_elections/", csv, sep="")) %>% 
      filter(GENERAL != "") %>% 
      mutate(PARTY = sub("*", "", PARTY)) %>% 
      filter((PARTY %in% non_dem_D) == F) %>% 
      mutate(PARTY = ifelse(grepl("D",PARTY), "DEM", "REP")) %>% 
      filter(PARTY %in% c("DEM", "REP")) %>% 
      mutate(GENERAL.. = as.numeric(sub("%", "", GENERAL..))) %>% 
      mutate(GENERAL.. = ifelse(is.na(GENERAL..), 0, GENERAL..)) %>% 
      filter(GENERAL != "Unopposed") %>% 
      filter(TOTAL.VOTES == "") %>%
      filter(FEC.ID. != "n/a") %>% 
      rename(X.I. = INCUMBENT.INDICATOR..I.) %>% 
      rename(D = DISTRICT) %>% 
      filter(D != "S") %>%
      mutate(D = str_pad(as.character(D), 2, side = "left", "0"))
    
    races_df <- races_df_full %>% select(`D`, `STATE`, `STATE.ABBREVIATION`, `PARTY`, `GENERAL..`, `X.I.`, `FEC.ID.`) %>% 
      mutate(key = paste(D, STATE.ABBREVIATION, PARTY, sep = "-")) 
    
    incumbent_party <- filter(races_df, GENERAL.. > 0) %>% 
      group_by(D, STATE) %>%
      mutate(I.P. = ifelse(X.I. == "(I)", PARTY, "")) %>% 
      filter(I.P.!= "") %>% 
      select(`I.P.`, `D`, `STATE`)
    
    multiple_i <- group_by(incumbent_party, D, STATE) %>% 
      summarize(n = n()) %>% 
      filter(n > 1) %>% 
      mutate(key = paste(D, STATE, sep = "-"))
    
    incumbent_party <- mutate(incumbent_party, I.P. = ifelse(paste(D, STATE, sep="-") %in% multiple_i$key, NA, I.P.))
    
    races_with_FEC <- group_by(races_df, D, STATE, PARTY) %>% filter(GENERAL.. == max(GENERAL..))
    FEC_ids <- races_with_FEC$FEC.ID.
    
    races_diff <- group_by(races_df, D, STATE, PARTY) %>% summarise(general = max(GENERAL..)) %>% 
      spread(PARTY, general) %>% 
      filter(!is.na(DEM)) %>% 
      filter(!is.na(REP)) %>% 
      mutate(diff = DEM - REP ) %>% 
      filter(abs(diff) < 95) %>%  #  filters out omst unopposed
      ungroup() %>% 
      left_join(incumbent_party, by=c("D" = "D", "STATE" = "STATE"))
    
    races_diff <- inner_join(races_diff, state_abbreviations, by=c("STATE" = "state.name")) %>% 
      mutate(CD = paste(state.abb, D, sep = "-")) %>% 
      mutate(CD = sub(" ", "", CD)) %>% 
      left_join(select(PVI_df, `PVI`, `CD`), by="CD") %>% 
      unique()
    
    race_PVI <- races_diff %>% 
      select(`CD`, `I.P.`, `diff`, `PVI`) %>% 
      mutate(year = substr(csv, 1,4)) %>% 
      mutate(I.P. = ifelse(is.na(I.P.), "OPN", I.P.)) %>% 
      mutate(PVI_expect = PVI + GENERIC_BALLOT)

  
    FEC_df <- filter(FEC_df, can_id %in% FEC_ids) %>% 
      mutate(PARTY = sub("*", "", PARTY)) %>% 
      filter((PARTY %in% non_dem_D) == F) %>% 
      mutate(PARTY = ifelse(grepl("D",PARTY), "DEM", "REP")) %>% 
      filter(PARTY %in% c("DEM", "REP")) %>% 
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
      mutate(YEAR = year) %>% 
      mutate(CD = paste(ST, D, sep = "-"))
    
    FEC_ind_con <- group_by(FEC_df, CD, PARTY) %>% summarize(ind_con = max(ind_con)) %>% 
        spread(PARTY, ind_con) %>% 
        mutate(DEM = ifelse(is.na(DEM), 0, DEM)) %>% 
        mutate(REP = ifelse(is.na(REP), 0, REP)) %>% 
        mutate(ind_con_diff = DEM - REP)
    
    FEC_tot_con <- group_by(FEC_df, CD, PARTY) %>% summarize(tot_con = max(tot_con)) %>% 
      spread(PARTY, tot_con) %>% 
      mutate(DEM = ifelse(is.na(DEM), 0, DEM)) %>% 
      mutate(REP = ifelse(is.na(REP), 0, REP)) %>% 
      mutate(tot_con_diff = DEM - REP)
    
    race_PVI <- left_join(race_PVI, FEC_ind_con, by = "CD") %>% 
      left_join(FEC_tot_con, by = "CD")
    
    race_df_list[[csv]] <- race_PVI    
  }
}


combined_2008_2014 <- do.call("rbind", race_df_list) %>% 
  mutate(PVI_miss = diff - PVI_expect) 

p1 <- ggplot(combined_2008_2014, aes(x = ind_con_diff, y = diff)) + geom_point() +
  stat_smooth(method = "lm")
p1

lm_ind <- lm(diff ~ ind_con_diff, data=combined_2008_2014)
summary(lm_ind)

lm_ind <- lm(diff ~ tot_con_diff, data=combined_2008_2014)
summary(lm_ind)
residuals_ind_con <- resid(lm_ind)
mean(residuals_ind_con)
sd(residuals_ind_con)
#hist(residuals_ind_con)

