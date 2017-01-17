library(dplyr)
library(tidyr)
library(stringr)

OBAMA_MARGIN_2008 = 7.2
KERRY_MARGIN_2004 = -2.4

## Calculate Partisan Voter Index of each congressional district
## Source: Daily Kos
## The CSV contains the results of elections 2000, 2004, and 2008 by congressional district
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
## Source: RCP
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

races_04_08 <- list.files(path = "past_elections/")

race_df_list = list()
for (csv in races_04_08){
  year = substr(csv, 1,4)
  PVI_df <- PVI_list[[as.numeric(year)]]
  GENERIC_BALLOT = generic_ballots[[year]]
  
  races_df <- read.csv(paste("past_elections/", csv, sep="")) %>% 
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
    mutate(D = str_pad(as.character(D), 2, side = "left", "0")) %>% 
    select(`D`, `STATE`, `STATE.ABBREVIATION`, `PARTY`, `GENERAL..`, `X.I.`) %>% 
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
  
  race_df_list[[csv]] <- race_PVI
}


combined_2004_2014 <- do.call("rbind", race_df_list) %>% 
  mutate(PVI_miss = diff - PVI_expect) %>% 
  filter(abs(diff) < 20) %>% 
  filter(abs(PVI_expect) < 20)

winners_by_incumbent <- combined_2004_2014 %>% group_by(CD, I.P., year) %>% summarize(margin = max(PVI_expect)) %>% 
  mutate(inc = ifelse(I.P. != "OPN", T, F)) %>% 
  mutate(inc_win = ifelse(inc, ifelse(I.P. == "REP", (margin < 0), (margin > 0)), F)) %>% 
  filter(inc) %>% 
  group_by(inc_win) %>%  summarize(n = n())


p <- ggplot(combined_2004_2014, aes(x = PVI_expect, y = diff, color = I.P.)) + geom_point()
p <- p + stat_smooth(method = "lm")
p

pvi_expect_fit <- lm(diff ~ PVI_expect, data = combined_2004_2014)
summary(pvi_expect_fit)

pvi_effect_open <- lm(diff ~ PVI_expect, data = filter(combined_2004_2014, I.P. == "OPN"))
summary(pvi_effect_open)

pvi_effect_dem <- lm(diff ~ PVI_expect, data = filter(combined_2004_2014, I.P. == "DEM"))
summary(pvi_effect_dem)

pvi_effect_rep <- lm(diff ~ PVI_expect, data = filter(combined_2004_2014, I.P. == "REP"))
summary(pvi_effect_rep)

group_by(combined_2004_2014, year, I.P.) %>%  summarize(med = median(PVI_miss, na.rm = T))
group_by(combined_2004_2014, I.P.) %>%  summarize(med = median(PVI_miss, na.rm = T), sd = sd(PVI_miss, na.rm=T), n = n())

wt_inc_sd_mean = weighted.mean(c(15.80872, 11.86640), c(952, 993))






