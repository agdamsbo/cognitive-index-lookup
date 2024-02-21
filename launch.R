# Typical shiny
cognitive.index.lookup::shiny_index()

# Shinylive version

shinylive::export(appdir = "app", destdir = "docs")

httpuv::runStaticServer(dir = "docs")


# Publish on ... free, limited instance (old traditional shiny host)

# Deploy on rsconnect
# renv::install("agdamsbo/cognitive.index.lookup")
cognitive.index.lookup::shiny_deploy_index()

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
