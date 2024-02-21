# Samlpe data from data base

read_instrument <- function(key = "enigma_api_key", instrument = "rbans", raw_label = "raw", records=30:50) {
  REDCapCAST::read_redcap_tables(records = records,
    uri = "https://redcap.au.dk/api/", token = keyring::key_get(key),
    fields = "record_id",
    forms = instrument, raw_or_label = raw_label
  )[[instrument]]
}

rbans_age_calc <- function(data){
  data |> dplyr::mutate(rbans_dob=as.Date(rbans_dob,format="%d-%m-%Y"),
                        rbans_age=floor(lubridate::time_length(rbans_date-rbans_dob,unit="years")))
}

obscure_number <- function(vec){
  vec + sample(c(-3:3),size=length(vec),replace=T)
}

ds <- read_instrument() |> rbans_age_calc()

sample_data <- ds |> dplyr::select(c("record_id",ends_with(c("_version","_age","_rs")))) |> 
  setNames(c("id","ab","age","imm","vis","ver","att","del")) |> 
  dplyr::mutate(age=obscure_number(age)) |> 
  na.omit() |> (\(x){
    x_split <- split(x,x$id)
    x_filt <- x_split[do.call(c,lapply(x_split,nrow))==2] |> (\(x)setNames(x,seq_len(length(x))))()
    x_filt |> purrr::imap(~{
      .x$id <- .y
      .x
    }) |> dplyr::bind_rows()
  })() |> dplyr::mutate(id=as.numeric(id)) #|> 
  # dplyr::filter(ab==1)


usethis::use_data(sample_data, overwrite = TRUE)
write.csv(sample_data, here::here("data/sample.csv"), row.names = FALSE)
openxlsx2::write_xlsx(x = sample_data, file = here::here("data/sample.xlsx"))