
# statewide_context_chemistry ---------------------------------------------
all_chem<-fetch::fetch_chem(
  path = "L:/BWAM Share/SMAS/data/cleaned_files",
  output = "standard"
)

#quantiles for the statewide context
sbu.chem.statewide<-all_chem%>%
  subset(CHR_VALIDATOR_QUAL!="R") %>%
  subset(CHS_EVENT_SMAS_SAMPLE_DATE >= "2013-01-01") %>%
  group_by(CHEM_PARAMETER_NAME,CHEM_PARAMETER_FRACTION,CHEM_PARAMETER_UNIT) %>%
  summarise_at(vars(CHR_RESULT_VALUE),
               funs(mean(.,na.rm = TRUE),
                    sd(.,na.rm = TRUE),n=n(),
                    q95=quantile(CHR_RESULT_VALUE,.95,na.rm = TRUE),
                    q75=quantile(CHR_RESULT_VALUE,.75,na.rm = TRUE),
                    q25=quantile(CHR_RESULT_VALUE,.25,na.rm = TRUE)))


#summarize for statwide averages- INSITU


sbu.insitu.statewide<-saw_field$insitu %>%
  filter(ISWC_RESULT!=-9999) %>%
  subset(ISWC_EVENT_SMAS_SAMPLE_DATE >= "2013-01-01") %>%
  group_by(CHEM_PARAMETER_NAME,CHEM_PARAMETER_FRACTION,CHEM_PARAMETER_UNIT) %>%
  summarise_at(vars(ISWC_RESULT),
               funs(mean(.,na.rm = TRUE),
                    sd(.,na.rm = TRUE),n=n(),
                    q95=quantile(ISWC_RESULT,.95,na.rm = TRUE),
                    q75=quantile(ISWC_RESULT,.75,na.rm = TRUE),
                    q25=quantile(ISWC_RESULT,.25,na.rm = TRUE)))

write.csv(sbu.insitu.statewide,here::here("data/statewide_insitu.csv"))
write.csv(sbu.chem.statewide,here::here("data/statewide_chem.csv"))
