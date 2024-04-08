index_table_raw <- read.csv(here::here("data-raw/index_table.csv"))

index_table <- index_table_raw |> lapply(\(x){
  gsub(",",".",x)
}) |> (\(x)do.call(data.frame,x))() |> 
  dplyr::mutate(index=as.numeric(index))

write.csv(index_table,here::here("data-raw/index_table.csv"),row.names = FALSE)


usethis::use_data(index_table,overwrite = TRUE)
