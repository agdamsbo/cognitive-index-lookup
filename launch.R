# Typical shiny
source(here::here("R/shiny_index.R"))
shiny_index()


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