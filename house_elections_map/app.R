library(tigris)
library(dplyr)
library(knitr)
library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

cd114 <- congressional_districts(cb = T, resolution = "500k")

outcome_odds <- read.csv("outcome_odds.csv")

categories <- mutate(outcome_odds, cat = ifelse(D_win > 0.4 & D_win < 0.6, "Tossup", ifelse(D_win < 0.4, "REP", "DEM"))) %>% 
  group_by(cat) %>%  summarise(n = n())
dem_seats <- sum(outcome_odds$D_win > 0.5, na.rm = T)
rep_seats <- sum(outcome_odds$D_win < 0.5, na.rm = T)


outcome_odds <- outcome_odds %>% mutate(state = substr(CD, 1, 2)) %>% 
  mutate(D = substr(CD, 4, 5))
states_fc <- fips_codes %>% select(`state`, `state_code`)
outcome_odds <- left_join(outcome_odds, states_fc, by = "state") %>% 
  unique() %>% 
  mutate(GEOID = paste(state_code, D, sep = "")) %>% 
  mutate(winner = ifelse(D_win > 0.5, paste(as.character(round(D_win * 100, 2)), " chance DEM win", sep="%"), paste(as.character((1-D_win) * 100), " chance REP win", sep="%") ))

districts <- geo_join(cd114, outcome_odds, "GEOID", "GEOID")

qpal <- colorNumeric(c("red", "white", "blue"), domain = 0:1)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  headerPanel(HTML("<h3>2016 House Elections Odds Map</h3>")),
  headerPanel(h6("Map takes a minute to load. Mouse over districts to see exact odds")),
  leafletOutput("mymap", height = "90%", width = "100%"),
  p()
)

server <- function(input, output, session) {

#label = ifelse(~D_win > 0.5, paste(as.character(~D_win * 100), " chance DEM win", sep="%"), paste(as.character((1 - ~D_win) * 100), " chance REP win", sep="%") )
  output$mymap <- renderLeaflet({
    leaflet(districts) %>% 
      addPolygons(weight = 1, color = ~qpal(D_win),
                  fillOpacity = 1,
                  label = ~paste(CD, (ifelse(D_win > 0.5, paste(as.character(round(D_win * 100)), " chance DEM win", sep="%"), paste(as.character(round((1 - D_win) * 100)), " chance REP win", sep="%"))), sep=': '),
                  labelOptions = labelOptions(noHide = T, textOnly = FALSE,
                                              style=list(
                                                'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                'font-size' = '12px'
                                              ))) %>% 
      addPolylines(weight = 1, color = "grey") %>% 
      setView(-80, 40, zoom = 6)
    
  })
}

shinyApp(ui, server)

