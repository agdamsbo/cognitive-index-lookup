index_table <- read.csv(here::here("data-raw/index_table.csv"))

index_table <- index_table |> lapply(\(x){
  gsub(",",".",x)
}) |> (\(x)do.call(data.frame,x))() |> 
  dplyr::mutate(index=as.numeric(index))



usethis::use_data(index_table,overwrite = TRUE)