##### Are Shutouts Team Driven or Strong Individual Goaltender Performance? #####
##### Started: 6/14/17 Last Edited: 7/23/17 #####

### Load Required Packages ###
require(dplyr);require(ggplot2)

### Load Datasets ### #All data comes from Corsica PBP Data
pbpdata <- list()
pbpdatabyseason <- c(
       "./Corsica PBP Data/pbp20072008.RDS",
       "./Corsica PBP Data/pbp20082009.RDS",
       "./Corsica PBP Data/pbp20092010.RDS",
       "./Corsica PBP Data/pbp20102011.RDS",
       "./Corsica PBP Data/pbp20112012.RDS",
       "./Corsica PBP Data/pbp20122013.RDS",
       "./Corsica PBP Data/pbp20132014.RDS",
       "./Corsica PBP Data/pbp20142015.RDS",
       "./Corsica PBP Data/pbp20152016.RDS",
       "./Corsica PBP Data/pbp20162017.RDS")

pbpdata <- lapply(pbpdatabyseason, function (x) readRDS(x))

names(pbpdata) <- c("pbp20072008","pbp20082009","pbp20092010","pbp20102011","pbp20112012",
                    "pbp20122013","pbp20132014","pbp20142015","pbp20152016","pbp20162017")

pbp20162017 <- pbpdata$pbp20162017 #slow computer so read in one at a time
rm(pbpdata);rm(pbpdatabyseason)

### Shot Attempts Data Frame ###
ShotAttemptsSummary <- pbp20162017 %>% #change data source depending on year of analysis
  filter(Event == "GOAL" | Event == "SHOT" | Event == "BLOCK" | Event == "MISS",
         Period <= "4") %>%
  mutate(H.Event = ifelse(Home.Team == ev.team,1,0),
         A.Event = ifelse(Away.Team == ev.team,1,0),
         Duration = as.numeric(Duration),
         Game.ID = as.numeric(Game.ID),
         Seconds = as.numeric(Seconds),
         Season = as.numeric(Season),
         xG = ifelse(is.na(xG),0,xG))

### TOI Data Frame ###
TOISummary <- pbp20162017 #change data source depending on year of analysis

TOISummary <- TOISummary %>%
  filter(Event == "ON") %>%
  mutate(Duration = as.numeric(Duration),
         Game.ID = as.numeric(Game.ID),
         Seconds = as.numeric(Seconds),
         Season = as.numeric(Season)) %>%
  arrange(Season,Game.ID,Seconds) %>%
  group_by(Season,Game.ID,p1) %>%
  rename(Goalie = p1) %>%
  summarise(TOI = sum(Duration)/60) %>%
  data.frame -> TOISummary

TOISummary <- TOISummary %>%
  mutate(SeasonGameIDGoalie = paste(Season,Game.ID,Goalie, sep = "")) %>%
  select(5,4)

### Summarise Goalie Data ###

rbind(
  ShotAttemptsSummary %>%
   filter(A.Event == 1) %>%
   group_by(Season,Season.Type,Game.ID,Home.Team,Home.Goalie,Away.Team) %>% #Optional: Add Strength.State
   rename(Goalie = Home.Goalie, Team = Home.Team, Opp = Away.Team) %>%
   summarise(Venue = "H",
            G.A = sum(Event == "GOAL" & H.Event == 0),
            SHOT.A = sum((Event == "SHOT" & H.Event == 0), G.A),
            MISS.A = sum(Event == "MISS" & H.Event == 0),
            BLOCK.A = sum(Event == "BLOCK" & H.Event == 0),
            CORSI.A = sum(SHOT.A, MISS.A, BLOCK.A),
            xG = sum(xG)),
  ShotAttemptsSummary %>%
    filter(H.Event == 1) %>%
    group_by(Season,Season.Type,Game.ID,Away.Team,Away.Goalie,Home.Team) %>% #Optional: Add Strength.State
    rename(Goalie = Away.Goalie, Team = Away.Team,Opp = Home.Team) %>%
    summarise(Venue = "A",
              G.A = sum(Event == "GOAL" & A.Event == 0),
              SHOT.A = sum((Event == "SHOT" & A.Event == 0), G.A),
              MISS.A = sum(Event == "MISS" & A.Event == 0),
              BLOCK.A = sum(Event == "BLOCK" & A.Event == 0),
              CORSI.A = sum(SHOT.A, MISS.A, BLOCK.A),
              xG = sum(xG))
) %>%
  data.frame() %>%
  group_by(Season,Season.Type,Game.ID,Team,Goalie,Venue,Opp) %>%
  filter(!is.na(Goalie)) %>%
  summarise(G.A = sum(G.A),
            SHOT.A = sum(SHOT.A),
            MISS.A = sum(MISS.A),
            BLOCK.A = sum(BLOCK.A),
            CORSI.A = sum(CORSI.A),
            xG = sum(xG)) %>%
  mutate(SeasonGameIDGoalie = paste(Season,Game.ID,Goalie, sep = "")) %>%
  data.frame -> GoalieSummary

GoalieSummary <- inner_join(x = GoalieSummary, y = TOISummary)

GoalieSummary <- GoalieSummary %>%
  select(1:7,15,8:13) %>%
  filter(Season.Type == "Regular", TOI >= 50) %>%
  mutate(xGper60 = xG/(TOI/60),
         GAper60 = G.A/(TOI/60)) %>%
  arrange(Game.ID)

Summary1617 <- GoalieSummary #change name depending on year of analysis

save(Summary1617,file = "C:/Users/Steve/Desktop/Hockey Analytics/R Data/Shutout Summaries/Summary1617.Rdata") #change name depending on year of analysis

### Combine Data ###

GoalieSummary <- rbind(
  Summary0708,
  Summary0809,
  Summary0910,
  Summary1011,
  Summary1112,
  Summary1213,
  Summary1314,
  Summary1415,
  Summary1516,
  Summary1617)

save(Summary1617,file = "C:/Users/Steve/Desktop/Hockey Analytics/R Data/Shutout Summaries/Summary1617.Rdata")

GoalieSummary$xGA.60Difference = GoalieSummary$xG.A - GoalieSummary$G.A

### Analysis ###

xGASummary <- rbind( #Can substitute xGA for SHOT.A or CORSI.A
  GoalieSummary %>%
    group_by(Season) %>%
    summarise(xGA.60 = mean(xG.Aper60),
              SOxGA.60 = 0),
  GoalieSummary %>%
    group_by(Season) %>%
    filter(G.A == 0) %>%
    summarise(xGA.60 = 0,
              SOxGA.60 = mean(xG.Aper60))
  ) %>%
  data.frame() %>%
  group_by(Season) %>%
  summarise(xGA.60 = sum(xGA.60),
            SOxGA.60 = sum(SOxGA.60)) %>%
  mutate(Difference = (xGA.60 - SOxGA.60)) %>%
  data.frame -> xGASummary

mean(xGASummary$Difference)
      
### Top Shutouts ###

EasiestShutout <- GoalieSummary %>%
  filter(G.A == 0) %>%
  arrange(xG.A) %>%
  select(1,3,5,7,9,10,13,14) %>%
  head(10) %>%
  mutate(avgxGA.Shot.A = xG.A/SHOT.A)

ToughestShutout <- GoalieSummary %>%
  filter(G.A == 0) %>%
  arrange(desc(xG.A)) %>%
  select(1,3,5,7,9,10,13,14) %>%
  head(10) %>%
  mutate(avgxGA.Shot.A = xG.A/SHOT.A)

BottomNonShutouts <- GoalieSummary %>% #Additional fun exercise
  filter(G.A > 0) %>%
  arrange(xGA.60Difference) %>%
  select(1,3,5,7,9,10,13,14) %>%
  mutate(Diff = xG.A - G.A) %>%
  head(10)
