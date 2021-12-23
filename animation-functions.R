library(dplyr)
library(gganimate)
computeRunSR <- function(dateRange1,dateRange2,year, path,type,filename) {

    battingDetails <- paste(path,"/",type,"-BattingDetails.RData",sep="")
    load(battingDetails)
    df3 <- NULL
    for (i in 1:12){
        date1=as.Date(dateRange1[i])
        date2=as.Date(dateRange2[i])
        df=battingDF %>% filter(date >= date1  & date <= date2)
        df1 <- select(df,batsman,runs,strikeRate)
        df2 <- summarise(group_by(df1, batsman), meanRuns=mean(runs),meanSR=mean(strikeRate))
        df2 <- df2[complete.cases(df2),]
        df2$year=year1[i]
        df3 <- rbind(df3,df2)

    }
    IPLBattingPerf <- df3 %>%
        group_by(year) %>%
        arrange(year, desc(meanRuns,meanSR)) %>%
        mutate(ranking = row_number()) %>%
        filter(ranking <=20)

    animation <- IPLBattingPerf %>%
        ggplot() +
        geom_col(aes(ranking, meanRuns, fill = batsman)) +
        geom_text(aes(ranking, meanRuns, label = meanRuns), hjust=-0.1) +
        geom_text(aes(ranking, y=0 , label = batsman), hjust=1.1) +
        geom_text(aes(x=15, y=max(meanSR) , label = as.factor(year)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
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
    #anim_save(filename, animation = last_animation(), path = ".")
}

computewicketsER <- function(dateRange1,dateRange2,year, path,type,filename) {

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

    animation <- IPLBowlingPerf %>%
        ggplot() +
        geom_col(aes(ranking, totalWickets, fill = bowler)) +
        geom_text(aes(ranking, totalWickets, label = totalWickets), hjust=-0.1) +
        geom_text(aes(ranking, y=0 , label = bowler), hjust=1) +
        geom_text(aes(x=15, y=max(round(meanER)) , label = as.factor(year)), hjust=-3, vjust =2, alpha = 0.5,  col = "gray", size = 20) +
        coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
        ggtitle("IPL Bowler performance animation") +
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
    #anim_save(filename, animation = last_animation(), path = ".")
}

computeRunsSRPowerPlay <- function(dateRange1,dateRange2,year, path,type,filename) {
    battingDF <- paste(path,"/",type,"-MatchesDataFrame.RData",sep="")
    load(battingDF)
    load(fl)

    a5 <- NULL
    # Filter by date range
    for (i in 1:12){
        date1=as.Date(dateRange1[i])
        date2=as.Date(dateRange2[i])
        df=t20MDF %>% filter(date >= date1  & date <= date2)


        a1 <-  df%>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
        a2 <- select(a1,ball,totalRuns,batsman,date)
        a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRPowerPlay=runs/count*100)
        a4 <- a3[complete.cases(a3),]
        a4$year=year1[i]
        a5 <- rbind(a5,a4)
    }

}
