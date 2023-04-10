
# habitat_work up ---------------------------------------------------------

habitat.short<-saw_field$habitat

#select relevant columns
habitat.short<-habitat.short %>%
  select(HFDH_EVENT_SMAS_HISTORY_ID,HFDH_EVENT_SMAS_SAMPLE_DATE
         ,HFDH_GRADIENT,HFDH_EPIFAUNAL_COVER,HFDH_EMBEDDEDNESS_POOLING,
         HFDH_VELOCITY_DEPTH_REGIME,
         HFDH_SEDIMENT_DEPOSITION,HFDH_FLOW_STATUS,
         HFDH_CHANNEL_ALTERATION, HFDH_RIFFLE_BEND_FREQUENCY,
         HFDH_LEFT_BANK_STABILITY,HFDH_RIGHT_BANK_STABILITY,
         HFDH_LEFT_BANK_VEG,HFDH_RIGHT_BANK_VEG,
         HFDH_LEFT_BANK_VEG_ZONE,HFDH_RIGHT_BANK_VEG_ZONE
  )
#create year column

habitat.short<-habitat.short %>%
  mutate(year=format(HFDH_EVENT_SMAS_SAMPLE_DATE,"%Y"))

#limit to time period specified

habitat.short<-habitat.short %>%
  filter(year >= 2019) %>%
  filter(year<=2022)


habitat.short<-merge(habitat.short,sites,
                     by.y="eventSMASHistoryId",
                     by.x="HFDH_EVENT_SMAS_HISTORY_ID")

habitat.short<-merge(habitat.short,sites_saw,
                     by.y="SITE_ID",
                     by.x="HFDH_EVENT_SMAS_HISTORY_ID")

habitat.short<-habitat.short %>%
  arrange(order)

habitat.short$pwlID<-factor(habitat.short$pwlID, ordered = TRUE)

habitat.short[4:16] <- sapply(habitat.short[4:16],as.numeric)

habitat.short[habitat.short==-9999]<-NA

habitat.short<-habitat.short %>%
  mutate(HFDH_GRADIENT=case_when(is.na(HFDH_GRADIENT)~"High",
                                 HFDH_GRADIENT=="Low"~"Low",
                                 TRUE~HFDH_GRADIENT))

#average all aspects by site
habitat.df<-habitat.short%>%
  group_by(HFDH_EVENT_SMAS_HISTORY_ID, HFDH_GRADIENT,HFDH_EVENT_SMAS_SAMPLE_DATE)%>%
  summarize(`Epi.\n Cover`=mean(HFDH_EPIFAUNAL_COVER, na.rm=TRUE),
            `Embed. \n Pool.`=mean(HFDH_EMBEDDEDNESS_POOLING, na.rm=TRUE),
            `Vel/Dep. \n Reg.`=mean(HFDH_VELOCITY_DEPTH_REGIME, na.rm=TRUE),
            `Sed. \n Dep.`=mean(HFDH_SEDIMENT_DEPOSITION, na.rm=TRUE),
            `Flow \n Status`=mean(HFDH_FLOW_STATUS, na.rm=TRUE),
            `Chan. \n Alt`=mean(HFDH_CHANNEL_ALTERATION, na.rm=TRUE),
            `Rif. \n Freq`=mean(HFDH_RIFFLE_BEND_FREQUENCY, na.rm=TRUE),
            `L.B. \n Stability`=mean(HFDH_LEFT_BANK_STABILITY, na.rm=TRUE),
            `R.B. \n Stability`=mean(HFDH_RIGHT_BANK_STABILITY, na.rm=TRUE),
            `L.B. \n Veg`=mean(HFDH_LEFT_BANK_VEG, na.rm=TRUE),
            `R.B.\n Veg`=mean(HFDH_RIGHT_BANK_VEG, na.rm=TRUE),
            `L.B. \n Veg Zone`=mean(HFDH_LEFT_BANK_VEG_ZONE, na.rm=TRUE),
            `R.B. \n Veg Zone`=mean(HFDH_RIGHT_BANK_VEG_ZONE, na.rm=TRUE))




#################################################################################################
# Add HMA model values to the table, add columns to see whether the model or stream score is lower, calculate HMA, add column to interpret HMA score
habit.hma<-habitat.df%>%
  mutate(EpiCoverM=case_when(
    HFDH_GRADIENT=="High" ~ 17,
    TRUE ~ 14),
    EmbedM=case_when(
      HFDH_GRADIENT=="High" ~ 17,
      TRUE ~ 13),
    VelDepM=case_when(
      HFDH_GRADIENT=="High" ~ 19,
      TRUE~10),
    SedDepM=case_when(
      HFDH_GRADIENT=="High" ~ 18,
      TRUE ~ 14),
    FlowM=case_when(
      HFDH_GRADIENT=="High" ~19,
      TRUE ~17),
    ChanM=case_when(
      HFDH_GRADIENT=="High"~ 18,
      TRUE ~ 17),
    RiffleM=case_when(
      HFDH_GRADIENT=="High"~ 19,
      TRUE~ 14),
    StabilityM=18,
    VegM=case_when(
      HFDH_GRADIENT=="High" ~ 18,
      TRUE ~ 17),
    VegZoneM=case_when(
      HFDH_GRADIENT=="High" ~ 18,
      TRUE ~ 15))

habit.hma<-habit.hma%>%
  mutate(EpiCover.Low=case_when(
    `Epi.\n Cover`<=EpiCoverM~`Epi.\n Cover`,
    TRUE~EpiCoverM),
    Embed.Low=case_when(
      `Embed. \n Pool.`<=EmbedM ~ `Embed. \n Pool.`,
      TRUE~EmbedM),
    VelDep.Low=case_when(
      `Vel/Dep. \n Reg.`<=VelDepM ~ `Vel/Dep. \n Reg.`,
      TRUE~ VelDepM),
    SedDep.Low=case_when(
      `Sed. \n Dep.`<=SedDepM ~ `Sed. \n Dep.`,
      TRUE~ SedDepM),
    Flow.Low=case_when(
      `Flow \n Status`<=FlowM ~ `Flow \n Status`,
      TRUE ~ FlowM),
    Chan.Low=case_when(
      `Chan. \n Alt`<=ChanM ~ `Chan. \n Alt`,
      TRUE~ ChanM),
    Riffle.Low=case_when(
      `Rif. \n Freq`<=RiffleM ~ `Rif. \n Freq`,
      TRUE ~ RiffleM),
    Stability.Low=case_when(
      (`L.B. \n Stability`+ `R.B. \n Stability`)<=StabilityM ~ (`L.B. \n Stability`+ `R.B. \n Stability`),
      TRUE~ StabilityM),
    Veg.Low=case_when(
      (`L.B. \n Veg`+`R.B.\n Veg`)<=VegM ~ (`L.B. \n Veg`+`R.B.\n Veg`),
      TRUE ~ VegM),
    Veg.Zone.Low=case_when(
      (`L.B. \n Veg Zone`+ `R.B. \n Veg Zone`)<= VegZoneM ~ (`L.B. \n Veg Zone`+ `R.B. \n Veg Zone`),
      TRUE~VegZoneM),
    `HMA \n Score`= case_when(
      HFDH_GRADIENT=="High" ~(EpiCover.Low+ Embed.Low+ VelDep.Low+ SedDep.Low+ Flow.Low+ Chan.Low+ Riffle.Low+ Stability.Low+Veg.Low+ Veg.Zone.Low)/181*100,
      TRUE ~ (EpiCover.Low+ Embed.Low+ VelDep.Low+ SedDep.Low+ Flow.Low+ Chan.Low+ Riffle.Low+ Stability.Low+Veg.Low+ Veg.Zone.Low)/149*100),
    `HMA \n Assess.`=case_when(
      `HMA \n Score`>=80 ~ "Natural",
      `HMA \n Score`>=70&`HMA \n Score`<80 ~ "Altered",
      `HMA \n Score`>=60 & `HMA \n Score` <70 ~ "Moderate",
      TRUE~ "Severe"
    ))

#rename columns
habit.hma<-habit.hma %>%
  rename(Site=HFDH_EVENT_SMAS_HISTORY_ID,
         Gradient=HFDH_GRADIENT)
habit.hma<-habit.hma %>%
  mutate_if(is.numeric,round, 1)

#add PWL to the column
habit.hma<-merge(habit.hma,sites, by.x="Site",by.y="eventSMASHistoryId")
habit.hma<-habit.hma %>%
  rename(PWL=pwlID)

HMA_counts<-habit.hma%>%
  rename(Assessment="HMA \n Assess.")

