
#shared by bill petti in FG slack chat on 11.25.2016


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
    filter(!Name %in% c("Name","","Arb Costs", "Arb Eligible", "Contract Options", 
                        "Dollars Committed", "Other Costs", "Option Values", "Other Players", 
                        "Payroll (no options)", "Payroll (options)", "Signed")) 
  
  
  #can get the '2017' column and extract any integers in there! maybe?
  
  tms_melt <- gather(tms, key = Year, value, -Name, -Age, -Yrs, -Acquired, -SrvTm, -Agent, -`Contract Status`)
  tms_melt <- tms_melt %>%
    filter(value != "") %>%
    mutate(Team = Tm) %>%
    group_by(Team, Name) %>%
    mutate(rank = row_number()) %>%
    filter(rank == max(rank)) %>%
    select(Team, Name, everything(), -rank)
  
  print(tms_melt)
}

all_teams_payroll <- teamAbbr %>% 
  group_by(Tm) %>%
  do(getPayrolls(.$Tm))
  
  #write_csv(all_teams_payroll, "all_teams_payroll.csv")