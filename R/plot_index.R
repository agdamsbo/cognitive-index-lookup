#' Plot index data. Assumes first column is id
#'
#' @param ds data frame
#' @param sub_plot column subset to plot
#' @param scores name bits of score variables
#' @param dom_names domain names
#' @param facet.by variable to base facet_grid on.
#'
#' @return ggplot list object
#' @export
#'
#' @examples
#' ds <- sample_data |> dplyr::filter(ab=="1") |> index_from_raw()
#' ds |> plot_index(facet.by="ab")
plot_index <- function(ds,
                       sub_plot="_is",
                       scores=c("_is","_lo","_up","_ci","_per"),
                       dom_names=c("immediate","visuospatial","verbal","attention","delayed","total"),
                       facet.by=NULL){

  plot_prep <- function(data){
    id <- colnames(ds)[1]
    
    data|>
      dplyr::select(tidyselect::all_of(c(id,
                      facet.by,
                      names(ds)[grepl(paste(scores,collapse="|"),names(ds))])))|>
      tidyr::pivot_longer(cols=-c(id,facet.by))|>
      subset(grepl(sub_plot,name))|>
      dplyr::mutate(value=suppressWarnings(as.numeric(value)),
                    name=factor(name,labels = dom_names),
                    id=as.numeric(id))
  }
  
  if (!is.null(facet.by)){
    if (length(unique(ds[[facet.by]]))==1){
      message("Only one level in facet variable. No facetting performed.")
      facet.by <- NULL
    }
  }
  
  df_plot<-ds |> plot_prep()

  
  if (!is.null(facet.by)){
    df_plot <- df_plot |> (\(x) {
      colnames(x)[2] <- "facet"
      x
      })()
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
      patchwork::wrap_plots(guides="collect",ncol=2,widths=c(5,1),tag_level = "new") 
  }
  
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
} else if (sub_plot=="_per"){
  
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
}

#' Plot index and percentile
#'
#' @param ds data set
#' @param ... arguments passed
#'
#' @return ggplot list
#' @export
plot_index2 <- function(ds, ...){
  require(patchwork)
  patchwork::wrap_plots(list(plot_index(ds),plot_index(ds,sub_plot = "_per")),nrow = 2) &
    ggplot2::theme(legend.position = "none")
}
