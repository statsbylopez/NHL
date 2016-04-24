
nhl.data <- filter(grand.data, etype=="GOAL"|etype =="PENL")
nhl.data2 <- nhl.data
nhl.data2 <- mutate(nhl.data2, id = paste(season, gcode, seconds))


## Drop matching penalties
pens.freq <- nhl.data2 %>%
  filter(etype!="GOAL") %>%
  group_by(id) %>%
  summarize(id.pen = length(id)) %>%
  filter(id.pen > 1)
nhl.data2 <- filter(nhl.data2, !id %in% pens.freq$id)

## Arrange by game, identify if there was a power play goal 
nhl.data3 <- nhl.data2 %>%
  arrange(seas, gcode) %>%
  group_by(seas, gcode) %>%
  mutate(f.etype = lead(etype, 1), f.home.skaters = lead(home.skaters, 1), 
         f.away.skaters = lead(away.skaters, 1), f.seconds = lead(seconds, 1), 
         f.ev.team = lead(ev.team, 1), time.diff = f.seconds - seconds, 
         f.score.tied = (lead(home.score, 1) == lead(away.score, 1)),
         PPgoal = (time.diff <=120 & (f.ev.team!= ev.team) & 
                etype=="PENL" & f.etype=="GOAL")) 

## Drop penalties with missing future data (these are the last penalty of each game)
nhl.data3 <- nhl.data3[is.na(nhl.data3$time.diff)=="FALSE",]

## Check overall PP goal rate
tally(PPgoal ~ etype, format = "proportion", data = nhl.data3)

## Identify if future penalty was a make-up call (this data set includes only penalties)
nhl.data4 <- nhl.data3 %>%
  filter(etype =="PENL") %>%
  arrange(seas, gcode, etype) %>%
  group_by(seas, gcode, etype) %>%
  mutate(f.ev.team2 = lead(ev.team, 1), id.pen = row_number(), 
         reverse.call = (f.ev.team2 !=ev.team)) %>%
  filter(id.pen <=1, is.na(reverse.call)==FALSE)

## Overall rate
tally(reverse.call ~ PPgoal, data = nhl.data4, 
      format = "proportion")

## Check for PS differences
tally(reverse.call ~ PPgoal +as.factor(gcode > 30000), data = nhl.data4, 
      format = "proportion")
