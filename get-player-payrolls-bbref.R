
# shared by bill petti in FG slack chat on 11.25.2016
# modified by ryan pollack on 1.3.2017 to get detailed payroll information 

getTeamabbr <- function(year) {
  team_tbl <- read_html(paste0("http://www.baseball-reference.com/leagues/MLB/", year, ".shtml"), stringsAsFactors=FALSE)
  tms <- team_tbl %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() %>%
    select(Tm) %>%
    filter(Tm != "LgAvg", 
           Tm != "", 
           Tm != "Tm")
  tms$year <- year
  return(tms)
}

teamAbbr <- getTeamabbr(2016)

getPayrolls <- function(Tm) {
  url <- paste0("http://www.baseball-reference.com/teams/", Tm, "/2016-payroll-salaries.shtml")
  tms <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[length(.)]] %>%
    html_table(header=TRUE)
  
  if (!"2021" %in% names(tms)) {
    tms$`2021` <- NA
    
  }
  
  tms <- tms %>%
    
    #remove unused cells in the 'Name' column
    filter(!Name %in% c("Name","","Arb Costs", "Arb Eligible", "Contract Options", 
                        "Dollars Committed", "Other Costs", "Option Values", "Other Players", 
                        "Payroll (no options)", "Payroll (options)", "Signed")) 
  
  #for each player, create a single row, one for each "Year", where "value" is either their salary that year, their arbitration status, or their FA status, or blank if the player won't be on the payroll.
  tms_melt <- gather(tms, key = Year, value, -Name, -Age, -Yrs, -Acquired, -SrvTm, -Agent, -`Contract Status`)
  
  tms_melt <- tms_melt %>%
    filter(value != "") %>%
    mutate(Team = Tm) %>%
    
    #give player minimum salary if they are pre-arb. else extract the actual or projected salary using a regex.
    mutate(value = ifelse(grepl("Pre-Arb", value), 0.535, value),
           value = ifelse(grepl("M", value), as.double(str_extract(value, "(?<=\\$)[:digit:]+\\.*[:digit:]*")), value),
           value = ifelse(grepl("k", value), as.double(str_extract(value, "(?<=\\$)[:digit:]+")) / 1000, value)) %>%
    arrange(Name, Year) %>%
    
    #project future arb years based on 25-40-60% formula located here: http://www.thepointofpittsburgh.com/calculating-mlb-arbitration-percentages/
    #And project Arb-4 salaries at 80% of FA value per my own whims
    mutate(value = ifelse(value == "Arb-1", 0.535, value),
           value = ifelse(value == "Arb-2" | value == "Arb", (as.double(lag(value)) / .25) * .4, value),
           value = ifelse(value == "Arb-3", (as.double(lag(value)) / .4) * .6, value),
           value = ifelse(value == "Arb-4", (as.double(lag(value)) / .6) * .8, value))
}

all_teams_payroll <- teamAbbr %>% 
  group_by(Tm) %>%
  do(getPayrolls(.$Tm)) %>%
  select(Team, Name, Age, Year, value)
