# Typical shiny
shiny_index()

shiny::runApp(appDir = here::here("app/"),launch.browser = TRUE)


# Shinylive version
shinylive::export(appdir = "shiny/", destdir = "docs")

httpuv::runStaticServer(dir = "docs")


# Publish on ... free, limited instance (old traditional shiny host)

# Deploy on rsconnect

shiny_deploy <- function(){
  rsconnect::setAccountInfo(
    name = "cognitiveindex",
    token = keyring::key_get(service = "rsconnect_cognitiveindex_token"), 
    secret = keyring::key_get(service = "rsconnect_cognitiveindex_secret")
  )
  
  rsconnect::deployApp(appDir = here::here("app"),appName = "index_app")
}

shiny_deploy()

## Examples

load(here::here("data/sample_data.rda"))

sample_data |> 
  index_from_raw() |> 
  head(10) |> 
  plot_index(id="id",facet.by="version") &
  patchwork::plot_annotation(tag_levels = list(c("A","","B","")))

sample_data |> 
  index_from_raw() |> 
  dplyr::filter(version=="a") |> 
  head(5) |> 
  plot_index2(id="id",facet.by="version") #&
  # patchwork::plot_annotation(tag_levels = list(c("Score","","Percentile","")))
