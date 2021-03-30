glicko_ratings <- function(match){
  ##########----- Make Glicko Ratings -----########## 
  ratings <- match %>%
    filter(Status == 'Home') %>%
    select(Date, Team, Opposition, results)%>%
    mutate(results = ifelse(results == 2, 0.5, results))
  
  ratings$date <- as.integer(format(ratings$Date, "%Y%m%d"))
  ratings$match <- rank(ratings$date)
  
  ratings %<>%
    select(match, Team, Opposition, results)
  
  ratings$Match_id<-NULL
  
  glicko_rate<-glicko2(ratings, history = T)
  
  #make dataframe with history ratings
  glicko <- as.data.frame(glicko_rate$history)
  setDT(glicko, keep.rownames = TRUE)[]
  glicko <- melt(glicko)
  glicko$variable <- as.character(glicko$variable)
  var <-data.frame(do.call('rbind', strsplit(as.character(glicko$variable),'.',fixed=TRUE)))
  glicko<-cbind(glicko, var)
  names(glicko)[1] <- "Team"
  names(glicko)[4] <- "match"
  names(glicko)[5] <- "var"
  glicko %<>%
    filter(var == "Rating")
  #rate$match_num <- with(rate, match(match, unique(Date)))
  
  ## See Glicko Prediction.R for ratings predictions ##
  
  #prepare data for merging with player stats
  glicko %<>% 
    group_by(Team) %>%
    mutate(rate_change = (value) - lag(value),
           rate_change = ifelse(is.na(rate_change), 2200 - value, rate_change)) %>% 
    ungroup()
  
  glicko_clean<-glicko[apply(glicko!=0, 1, all),]
  glicko_clean %<>% filter(var == "Rating")
  glicko_clean$match <- as.integer(glicko_clean$match)
  
  glicko_clean %<>%
    group_by(Team) %>%
    mutate(match_num = order(order(match, decreasing=F))) %>%
    select(Team, match_num, value, rate_change) %>% 
    ungroup()
  return(glicko_clean)
}