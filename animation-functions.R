library(dplyr)
library(gganimate)
library(stringr)
computeRunSR <- function(dateRange1,dateRange2,year, path,type) {

    battingDetails <- paste(path,"/",type,"-BattingDetails.RData",sep="")
    load(battingDetails)
    df3 <- NULL
    for (i in 1:12){
        date1=as.Date(dateRange1[i])
        date2=as.Date(dateRange2[i])
        df=battingDF %>% filter(date >= date1  & date <= date2)
        df1 <- select(df,batsman,runs,strikeRate)
        df2 <- summarise(group_by(df1, batsman), totalRuns=sum(runs),meanSR=mean(strikeRate))
        df2 <- df2[complete.cases(df2),]
        df2$year=year1[i]
        df3 <- rbind(df3,df2)

    }
    df3 <- df3 %>% filter(totalRuns > quantile(totalRuns,prob=0.70))
    IPLBattingPerf <- df3 %>%
        group_by(year) %>%
        arrange(year, desc(totalRuns,meanSR)) %>%
        mutate(ranking = row_number()) %>%
        filter(ranking <=20)

    a <- paste(type, " batting performance (sliding window of 3 years)")
    animation <- IPLBattingPerf %>%
        ggplot() +
        geom_col(aes(ranking, totalRuns, fill = batsman)) +
        geom_text(aes(ranking, totalRuns, label = totalRuns), hjust=-0.1) +
        geom_text(aes(ranking, y=0 , label = batsman), hjust=1.1) +
        ggtitle(a) + theme(plot.title = element_text(size = 24, face = "bold")) +
        geom_text(aes(x=15, y=max(meanSR) , label = as.factor(year)), hjust=-3, vjust = 2, alpha = 0.5,  col = "gray", size = 20) +
        coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
        theme_minimal() + theme(
            panel.grid = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = margin(1, 4, 1, 3, "cm")
        ) +
        transition_states(year, state_length = 0, transition_length = 2) +
        enter_fade() +
        exit_fade() +
        ease_aes('quadratic-in-out')
    animate(animation, width = 700, height = 432, fps = 15, duration = 15, rewind = FALSE)

}

computewicketsER <- function(dateRange1,dateRange2,year, path,type) {

    bowlingDetails <- paste(path,"/",type,"-BowlingDetails.RData",sep="")
    load(bowlingDetails)
    df1 <- NULL
    for (i in 1:12){
        date1=as.Date(dateRange1[i])
        date2=as.Date(dateRange2[i])
        df=bowlingDF %>% filter(date >= date1  & date <= date2)
        # Compute number of matches played
        a=df %>% select(bowler,date) %>% unique()
        b=summarise(group_by(a,bowler),matches=n())

        # Compute wickets
        c <- filter(df,wicketPlayerOut != "nobody")
        d <- select(c,bowler,wicketPlayerOut,economyRate,date,opposition,venue)
        e <- summarise(group_by(d,bowler,date,economyRate),wickets=length(unique(wicketPlayerOut)))
        f=summarise(group_by(e,bowler), totalWickets=sum(wickets),meanER=mean(economyRate))

        f <- f[complete.cases(f),]
        f$year=year1[i]

        df1 <- rbind(df1,f)

    }
    IPLBowlingPerf <- df1 %>%
        group_by(year) %>%
        arrange(year, desc(totalWickets)) %>%
        mutate(ranking = row_number()) %>%
        filter(ranking <=20)

    a <- paste(type, " bowling performance (sliding window of 3 years)")
    animation <- IPLBowlingPerf %>%
        ggplot() +
        geom_col(aes(ranking, totalWickets, fill = bowler)) +
        geom_text(aes(ranking, totalWickets, label = totalWickets), hjust=-0.1) +
        geom_text(aes(ranking, y=0 , label = bowler), hjust=1) +
        geom_text(aes(x=15, y=max(meanER) , label = as.factor(year)), hjust=-3, vjust =2, alpha = 0.5,  col = "gray", size = 20) +
        coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
        ggtitle(a) + theme(plot.title = element_text(size = 24, face = "bold")) +
        theme_minimal() + theme(
            panel.grid = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = margin(1, 4, 1, 3, "cm")
        ) +
        transition_states(year, state_length = 0, transition_length = 2) +
        enter_fade() +
        exit_fade() +
        ease_aes('quadratic-in-out')
    animate(animation, renderer = gifski_renderer(),width = 700, height = 432, fps = 15, duration = 15, rewind = FALSE)
    #anim_save(filename, animation = last_animation(), path = ".")
}

computeRunsSRPhase <- function(dateRange1,dateRange2,year, path,type,phase) {
    battingDF <- paste(path,"/",type,"-MatchesDataFrame.RData",sep="")
    load(battingDF)

    a5 <- NULL
    # Filter by date range
    if(phase =="pp"){
        phs="Power play"
        for (i in 1:12){
            date1=as.Date(dateRange1[i])
            date2=as.Date(dateRange2[i])
            df=t20MDF %>% filter(date >= date1  & date <= date2)


            a1 <-  df%>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
            a2 <- select(a1,ball,totalRuns,batsman,date)
            a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SR=runs/count*100)
            a4 <- a3[complete.cases(a3),]
            a4$year=year1[i]
            a5 <- rbind(a5,a4)
        }
    } else if(phase=="mo"){
      phs="Middle overs"
        for (i in 1:12){
            date1=as.Date(dateRange1[i])
            date2=as.Date(dateRange2[i])
            df=t20MDF %>% filter(date >= date1  & date <= date2)


            a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
            a2 <- select(a1,ball,totalRuns,batsman,date)
            a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SR=runs/count*100)
            a4 <- a3[complete.cases(a3),]
            a4$year=year1[i]
            a5 <- rbind(a5,a4)
        }
    } else if (phase == "do"){
        phs="Death overs"
        for (i in 1:12){
            date1=as.Date(dateRange1[i])
            date2=as.Date(dateRange2[i])
            df=t20MDF %>% filter(date >= date1  & date <= date2)


            a1 <- df  %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
            a2 <- select(a1,ball,totalRuns,batsman,date)
            a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SR=runs/count*100)
            a4 <- a3[complete.cases(a3),]
            a4$year=year1[i]
            a5 <- rbind(a5,a4)
        }
    }

    IPLBattingPerf <- a5 %>%
        group_by(year) %>%
        arrange(year, desc(runs)) %>%
        mutate(ranking = row_number()) %>%
        filter(ranking <=20)

    a <- paste(type, " batting performance in ",phs," (sliding window of 3 years)")
    animation <- IPLBattingPerf %>%
        ggplot() +
        geom_col(aes(ranking, runs, fill = batsman)) +
        geom_text(aes(ranking, runs, label = runs), hjust=-0.1) +
        geom_text(aes(ranking, y=0 , label = batsman), hjust=1.1) +
        geom_text(aes(x=15, y=max(SR) , label = as.factor(year)),hjust=-1, vjust =1.5, alpha = 0.5,  col = "gray", size = 20) +
        coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
        ggtitle(a) + theme(plot.title = element_text(size = 24, face = "bold")) +
        theme_minimal() + theme(
            panel.grid = element_blank(),
            legend.position = "none",
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = margin(1, 4, 1, 3, "cm")
        ) +
        transition_states(year, state_length = 0, transition_length = 2) +
        enter_fade() +
        exit_fade() +
        ease_aes('quadratic-in-out')

    animate(animation, renderer = gifski_renderer(),width = 700, height = 432, fps = 15, duration = 15, rewind = FALSE)

}

computeWicketsERPhase <- function(dateRange1,dateRange2,year, path,type,phase) {
  bowlingDF <- paste(path,"/",type,"-MatchesDataFrame.RData",sep="")
  load(bowlingDF)

  a5 <- NULL
  # Filter by date range
  if(phase =="pp"){
    phs="Power play"
    for (i in 1:12){
      date1=as.Date(dateRange1[i])
      date2=as.Date(dateRange2[i])
      df=t20MDF %>% filter(date >= date1  & date <= date2)

      # Power play
      a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
      a2 <- select(a1,date,bowler,wicketPlayerOut)
      a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
      a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(totalWickets=sum(wickets))

      a21 <- select(a1,team,bowler,date,totalRuns)
      a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ER=total/count *6)
      a41 <- a31 %>% select(bowler,ER)
      a42=inner_join(a4,a41,by="bowler")
      a42 <- a42[complete.cases(a42),]
      a42$year=year1[i]
      a5 <- rbind(a5,a42)
    }
  } else if (phase == "mo"){
    phs="Middle overs"
    for (i in 1:12){
      date1=as.Date(dateRange1[i])
      date2=as.Date(dateRange2[i])
      df=t20MDF %>% filter(date >= date1  & date <= date2)

      # Middle overs
      a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
      a2 <- select(a1,date,bowler,wicketPlayerOut)
      a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
      a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(totalWickets=sum(wickets))

      a21 <- select(a1,team,bowler,date,totalRuns)
      a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ER=total/count *6)
      a41 <- a31 %>% select(bowler,ER)
      a42=inner_join(a4,a41,by="bowler")
      a42 <- a42[complete.cases(a42),]
      a42$year=year1[i]
      a5 <- rbind(a5,a42)
    }
  } else if (phase == "do"){
    phs="Death overs"
    for (i in 1:12){
      date1=as.Date(dateRange1[i])
      date2=as.Date(dateRange2[i])
      df=t20MDF %>% filter(date >= date1  & date <= date2)

      # Death overs
      a1 <- df  %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
      a2 <- select(a1,date,bowler,wicketPlayerOut)
      a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
      a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(totalWickets=sum(wickets))

      a21 <- select(a1,team,bowler,date,totalRuns)
      a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ER=total/count *6)
      a41 <- a31 %>% select(bowler,ER)
      a42=inner_join(a4,a41,by="bowler")
      a42 <- a42[complete.cases(a42),]
      a42$year=year1[i]
      a5 <- rbind(a5,a42)
    }
  }
  IPLBowlingPerf <- a5 %>%
    group_by(year) %>%
    arrange(year, desc(totalWickets)) %>%
    mutate(ranking = row_number()) %>%
    filter(ranking <=20)

  a <- paste(type, " bowling performance in ",phs," (sliding window of 3 years)")
  animation <- IPLBowlingPerf %>%
    ggplot() +
    geom_col(aes(ranking, totalWickets, fill = bowler)) +
    geom_text(aes(ranking, totalWickets, label = totalWickets), hjust=-0.1) +
    geom_text(aes(ranking, y=0 , label = bowler), hjust=1) +
    geom_text(aes(x=15, y=max(ER) , label = as.factor(year)), hjust=-1.5, vjust =1.5, alpha = 0.5,  col = "gray", size = 20) +
    coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
    ggtitle(a) + theme(plot.title = element_text(size = 24, face = "bold")) +
    theme_minimal() + theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      plot.margin = margin(1, 4, 1, 3, "cm")
    ) +
    transition_states(year, state_length = 0, transition_length = 2) +
    enter_fade() +
    exit_fade() +
    ease_aes('quadratic-in-out')
  animate(animation, renderer = gifski_renderer(),width = 700, height = 432, fps = 15, duration = 15, rewind = FALSE)
}
