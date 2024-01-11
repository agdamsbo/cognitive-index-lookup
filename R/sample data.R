# Samlpe data from data base

source("https://raw.githubusercontent.com/agdamsbo/ENIGMAtrial_R/main/src/redcap_api_export_short.R")

df<-redcap_api_export_short(id= c(40:45),
                            instruments= "rbans",
                            event= "3_months_arm_1") %>%
  select(c("record_id",ends_with(c("_version","_age","_rs")))) %>%
  na.omit()|>
  rbind(redcap_api_export_short(id= c(10:15),
                                instruments= "rbans",
                                event= "12_months_arm_1") %>%
          select(c("record_id",ends_with(c("_version","_age","_rs")))) %>%
          na.omit())

colnames(df)<-c("record_id","ab","age","imm","vis","ver","att","del")

## New index numbers
df$record_id<-1:nrow(df)

## Age is changed for obscurity
df$age<-sample(-2:2,nrow(df),TRUE)+df$age

write.csv(df, "sample.csv", row.names = FALSE)
