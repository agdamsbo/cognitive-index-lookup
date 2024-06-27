# Prep for shiny
# system2("cat ./R/index_from_raw.R ./R/plot_index.R ./R/read_file.R > ./R/functions.R")

project.aid::merge_scripts(list.files("R/",full.names = TRUE),dest = here::here("app/functions.R"))

# Typical shiny
cognitive.index.lookup::shiny_index()

# Shinylive version

shinylive::export(appdir = "live-app", destdir = "docs")

httpuv::runStaticServer(dir = "docs")


# Publish on ... free, limited instance (old traditional shiny host)

# Deploy on rsconnect
# pak::pak("agdamsbo/project.aid")
project.aid::deploy_shiny(account.name = "cognitiveindex",
                          name.app = "index_app",
                          name.token = "rsconnect_cognitiveindex_token",
                          name.secret = "rsconnect_cognitiveindex_secret")

## Examples
library(cognitive.index.lookup)
library(patchwork)

sample_data |> 
  index_from_raw() |> 
  head(10) |> 
  plot_index(facet.by="ab") &
  patchwork::plot_annotation(tag_levels = list(c("A","","B","")))

sample_data |> 
  index_from_raw() |> 
  dplyr::filter(id %in% 1:5) |>
  dplyr::filter(ab==1) |>
  plot_index2(facet.by="ab") #&
  # patchwork::plot_annotation(tag_levels = list(c("Score","","Percentile","")))



