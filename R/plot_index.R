library(readr)
index <- read.csv("data/index.csv")

#' Title
#'
#' @param ds data frame
#' @param id colname  of id column. Base for colourin
#' @param sub_plot column subset to plot
#' @param scores name bits of score variables
#' @param dom_names domain names
#' @param facet.by variable to base facet_grid on.
#'
#' @return
#' @export
#'
#' @examples
#' load(here::here("data/sample_data.rda"))
#' ds <- sample_data |> index_from_raw()
#' ds |> plot_index(id="id",facet.by="version")
plot_index <- function(ds,
                       id="record_id",
                       sub_plot="_is",
                       scores=c("_is","_lo","_up","_ci","_per"),
                       dom_names=c("immediate","visuospatial","verbal","attention","delayed","total"),
                       facet.by=NULL){

  # id colname  of id column. Base for colouring
  # ds data frame
  # sub_plot column subset to plot
  # scores name bits of score variables
  # dom_names domain names
  # facet.by variable to base facet_grid on.
 
  plot_prep <- function(data){
    data|>
      dplyr::select(c(id,
                      facet.by,
                      names(ds)[grepl(paste(scores,collapse="|"),names(ds))]))|>
      tidyr::pivot_longer(cols=-c(id,facet.by))|>
      subset(grepl(sub_plot,name))|>
      dplyr::mutate(value=suppressWarnings(as.numeric(value)),
                    name=factor(name,labels = dom_names),
                    id=as.numeric(id))
  }
  
  df_plot<-ds |> head(10) |> plot_prep()
  
  if (!is.null(facet.by)){
    colnames(df_plot)<-c("id","facet","name","value")
  } else {
    colnames(df_plot)<-c("id","name","value")
  }

  index_sub_plot <- function(data){
    data|>
      ggplot2::ggplot(ggplot2::aes(x=name, y=value, color=factor(id), group=factor(id))) + 
      ggplot2::geom_point() +
      ggplot2::geom_path() +
      ggplot2::expand_limits(y=c(40,160)) +
      ggplot2::scale_y_continuous(breaks=seq(40,160,by=10)) +
      ggplot2::ylab(label=NULL) +
      ggplot2::xlab(label = NULL)+
      ggplot2::labs(colour = "ID")
  }
  
  percentile_sub_plot <- function(data){
  data|>
    ggplot2::ggplot(ggplot2::aes(x=name, y=value, fill=factor(id)))+
    ggplot2::geom_col(position = "dodge") +
    ggplot2::expand_limits(y=c(0,100)) +
    ggplot2::scale_y_continuous(breaks=seq(0,100,by=10)) +
      ggplot2::ylab(label=NULL) +
      ggplot2::xlab(label = NULL)+
    ggplot2::labs(fill = "ID")
    }

  plot_stitch <- function(list,fun=index_sub_plot){
    require(patchwork)
    list|> purrr::list_flatten() |> 
      lapply(fun) |> 
      patchwork::wrap_plots(guides="collect",ncol=2,widths=c(5,1),tag_level = "new") +
      patchwork::plot_annotation(tag_levels = list(c("A - Domains","A - Total","B - Domains","B - Total")))
  }
  
  
require(patchwork)
if (sub_plot=="_is"){
  
  if (!is.null(facet.by)){
  df_plot |> (\(x)split(x,x$facet))()|> setNames(c("a","b")) |> 
    lapply(\(x){
        split(x,x$name=="total") |> 
        setNames(c("domains","total"))
  }) |> plot_stitch()
  
  } else {
    df_plot |> (\(x){
        split(x,x$name=="total")
      })()|> 
      setNames(c("domains","total")) |> 
      plot_stitch()
  }
}

if (sub_plot=="_per"){
  
  if (!is.null(facet.by)){
    df_plot |> (\(x)split(x,x$facet))()|> setNames(c("a","b")) |> 
      lapply(\(x){
        split(x,x$name=="total") |> 
          setNames(c("domains","total"))
      }) |> plot_stitch(percentile_sub_plot)
    
  } else {
    df_plot |> (\(x){
      split(x,x$name=="total")
    })()|> 
      setNames(c("domains","total")) |> 
      plot_stitch(percentile_sub_plot)
  }
}
  

if (!is.null(facet.by)){
  index_plot + ggplot2::facet_grid(cols=ggplot2::vars(facet)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
} else {
  index_plot
}

}

plot_index2 <- function(ds, ...){
  require(patchwork)
  plot_index(ds)/plot_index(ds,sub_plot = "_per")
}
