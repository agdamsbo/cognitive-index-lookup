df<-data.frame(record_id="1",
           imm=35,
           vis=35,
           ver=35,
           att=35,
           del=35,
           stringsAsFactors = FALSE)

source("https://raw.githubusercontent.com/agdamsbo/ENIGMAtrial_R/main/src/index_from_raw.R")

df_c<-index_from_raw(ds=df,
               indx=read.csv("https://raw.githubusercontent.com/agdamsbo/ENIGMAtrial_R/main/data/index.csv"),
               version = "1",
               age = 60,
               raw_columns=c("imm","vis","ver","att","del"),
               mani=TRUE)

plot_index(ds=df_c,id="id",sub_plot = "_is")
