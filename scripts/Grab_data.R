#grab bug data
library(ggplot2)
library(dplyr)
saw_bugs<-fetch::fetch_bugs(
  path = "L:/BWAM Share/SMAS/data/cleaned_files",
  output = "standard"
)
sites<-fetch::fetch_sites(
  path = "L:/BWAM Share/SMAS/data/cleaned_files",
  output = "standard"
)

sites_saw<-read.csv(here::here("data/sawmill_sites.csv"))

saw_bugs<-saw_bugs %>%
  filter( MSSIH_EVENT_SMAS_HISTORY_ID %in% sites$SITE_ID)
write.csv(saw_bugs,here::here("data/saw_bugs.csv"))


# process_bugs ------------------------------------------------------------
saw_bugs<-merge(saw_bugs,sites,
                by.x="MSSIH_EVENT_SMAS_HISTORY_ID",
                by.y="eventSMASHistoryId")

saw_bugs<-merge(saw_bugs,sites_saw,
                by.x = "MSSIH_EVENT_SMAS_HISTORY_ID",
                by.y ="SITE_ID")

saw_bugs_sum<-saw_bugs %>%
  mutate(year = format(MSSIH_EVENT_SMAS_SAMPLE_DATE,"%Y")) %>%
  filter(year>= 2019) %>%
  group_by(MSSIH_EVENT_SMAS_HISTORY_ID,group,order,pwlID) %>%
  rename(BAP = MMDH_BIO_ASMT_PROFILE_SCORE) %>%
  summarise(mean=mean(BAP, na.rm = TRUE),
            N=n(),
            sd=sd(BAP, na.rm = TRUE))%>%
  mutate(se=sd/sqrt(N),
         ci=qt(1 - ((1 - 0.95) / 2), N - 1) * se)

saw_bugs_sum$MSSIH_EVENT_SMAS_HISTORY_ID<-forcats::fct_reorder(saw_bugs_sum$MSSIH_EVENT_SMAS_HISTORY_ID,
                                                               saw_bugs_sum$order)

#set colors for the graph
groups<-as.character(unique(sites_saw$group))

group_colors <- setNames(RColorBrewer::brewer.pal(length(unique(sites_saw$group)), "Set1"),
                         levels(length(unique(sites_saw$group))))

names(group_colors)<-c(groups)

group_colors <- group_colors[!is.na(names(group_colors))]


bap.graph<-function(df){
  l<-length(df$MSSIH_EVENT_SMAS_HISTORY_ID)

  labels.df <- data.frame(
    label = c("Non", "Slight", "Moderate", "Severe"),
    x = max(l)+1,
    y = c(8.75, 6.25, 3.75, 1.25),
    stringsAsFactors = FALSE)

  ggplot(df, aes(x=MSSIH_EVENT_SMAS_HISTORY_ID, y=mean))+
    geom_point(aes(shape=pwlID, color=group), size=4)+
    #geom_point(aes(color=group), size=4)+
   # scale_shape_manual(name="PWL Segment ID",values = 0:max(l.p))+
    scale_color_manual(breaks=names(group_colors),values=group_colors)+
    #scale_color_discrete(limits=group_colors)+
    #geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci, color=group,),  width=.5)+
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),#remove gridlines
      axis.title.y = element_text(
        size = 12,
        family = "serif",
        face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 10,
        family = "serif"), #rotate text angle
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.position = c(.11, .81) + #reduces white space around plot edges
        geom_label(aes(x = .5, y = .5), label = "test")
    )+
    geom_hline(yintercept=2.5,linetype="dashed",color="grey")+
    geom_hline(yintercept=5,linetype="dashed",color="grey")+
    geom_hline(yintercept=7.5,linetype="dashed",color="grey")+
    theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
    ylab("Biological Assessment Profile Score")+xlab("Year")+ geom_text(data = labels.df, aes(max(l)+1, y, label = label), color = "black",angle=90)+
    expand_limits(y=c(0,10),x=c(0:max(l+2)))


}


bap<-bap.graph(saw_bugs_sum)


# grab_insitu -------------------------------------------------------------

saw_field<-fetch::fetch_field(
  path = "L:/BWAM Share/SMAS/data/cleaned_files",
  output = "standard"
)

insitu<-saw_field$insitu

insitu<-insitu %>%
  filter(ISWC_EVENT_SMAS_HISTORY_ID %in% sites_saw$SITE_ID)

insitu<-insitu %>%
  mutate(year = format(ISWC_EVENT_SMAS_SAMPLE_DATE, "%Y")) %>%
  filter(year >=2019)
 #merge with sites table

insitu.short<-merge(insitu,sites,
                    by.x="ISWC_EVENT_SMAS_HISTORY_ID",
                    by.y="eventSMASHistoryId")
