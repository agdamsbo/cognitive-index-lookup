# Typical shiny
cognitive.index.lookup::shiny_index()

# Shinylive version

shinylive::export(appdir = "app", destdir = "docs")

httpuv::runStaticServer(dir = "docs")


# Publish on ... free, limited instance (old traditional shiny host)

# Deploy on rsconnect

shiny_deploy()

## Examples


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
