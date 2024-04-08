#' Data preparation for index score plotting
#'
#' @param data data set
#' @param sub_plot sub plot ("_is" or "_per")
#' @param scores score variable name bits
#' @param grp.color column to color
#' @param facet.by variable to facet by
#' @param dom_names domain names
#'
#' @return data frame
#' @importFrom tidyr pivot_longer
plot_prep <- function(data,sub_plot,scores,grp.color,facet.by,dom_names){
  library(tidyr)
  out <- data|>
    dplyr::select(tidyselect::all_of(c(colnames(dplyr::select(data,tidyselect::matches(grp.color))),
                                       {{facet.by}},
                                       names(dplyr::select(data,tidyselect::ends_with(c(scores,do.call(c,lapply(scores,paste,dom_names,sep = "_"))))))
                                       # names(dplyr::select(data,tidyselect::starts_with("test")))
    )))|>
    tidyr::pivot_longer(cols=-tidyselect::all_of(c(grp.color,facet.by)))|>
    subset(grepl(sub_plot,name))|>
    dplyr::mutate(name=factor(name,labels = dom_names),
                  value=as.numeric(value))
  out[[1]] <- factor(out[[1]])
  out
}

#' Plots index scores
#'
#' @param data data set
#' @param grp.color column to color
#'
#' @return ggplot2 list object
index_sub_plot <- function(data,grp.color){
  grp <- data[[grp.color]]
  
  data|>
    ggplot2::ggplot(ggplot2::aes(x=name, y=value, color=grp, group=grp)) +
    ggplot2::geom_hline(yintercept = 100, alpha=.8)+
    ggplot2::geom_hline(yintercept = 85, alpha=.8, linetype="dashed")+
    ggplot2::geom_hline(yintercept = 70, alpha=.8, linetype="dotdash")+
    ggplot2::geom_path(linewidth=1) +
    ggplot2::geom_point(size=5) +
    ggplot2::expand_limits(y=c(40,160)) +
    ggplot2::scale_y_continuous(breaks=seq(40,160,by=10)) +
    ggplot2::ylab(label=NULL) +
    ggplot2::xlab(label = NULL)+
    ggplot2::labs(colour = grp.color)
}


#' Plots percentile
#'
#' @param data data set
#' @param grp.color column to color
#'
#' @return ggplot2 list object
percentile_sub_plot <- function(data,grp.color){
  grp <- data[[grp.color]]
  
  data|>
    ggplot2::ggplot(ggplot2::aes(x=name, y=value, fill=grp))+
    ggplot2::geom_col(position = "dodge") +
    ggplot2::expand_limits(y=c(0,100)) +
    ggplot2::scale_y_continuous(breaks=seq(0,100,by=10)) +
    ggplot2::ylab(label=NULL) +
    ggplot2::xlab(label = NULL)+
    ggplot2::labs(fill = grp.color)
}

#' Stitch ggplots
#'
#' @param list list of plots
#' @param fun function to use for plotting
#' @param grp.color column to color
#'
#' @return ggplot2 list object
plot_stitch <- function(list,fun=index_sub_plot,grp.color){
  list|> purrr::list_flatten() |>
    lapply(fun,grp.color) |>
    patchwork::wrap_plots(guides="collect",ncol=2,widths=c(5,1),tag_level = "new")
}

#' Plot index data. Assumes first column is id
#'
#' @param sub_plot column subset to plot. Can be "_is" or "_per".
#' @param scores name bits of score variables
#' @param dom_names domain names
#' @param facet.by variable to base facet_grid on.
#' @param data data set
#' @param grp.color color grouping for ggplot
#'
#' @return ggplot list object
#' @export
#'
#' @examples
#' ds <- cognitive.index.lookup::sample_data  |> index_from_raw()
#' ds |> tibble::tibble() |> dplyr::filter(ab=="1") |>  plot_index(facet.by="ab")
#' ds |> tibble::tibble() |>  plot_index()
#' data <- ds
plot_index <- function(data,
                       sub_plot="_is",
                       scores=c("_is","_lo","_up","_per"), # this will do for now. _lo and _up should be included to allow for apron plot. could work with summarise?
                       dom_names=c("immediate","visuospatial","verbal","attention","delayed","total"),
                       facet.by=NULL,
                       grp.color=NULL){
  
  if (is.null(grp.color)){
    grp.color <- colnames(data)[1]
  }
  
  if (!is.null(facet.by)){
    if (length(unique(data[[facet.by]]))==1){
      message("Only one level in facet variable. No facetting performed.")
      facet.by <- NULL
    }
  }
  
  df_plot<-data |> plot_prep(sub_plot,scores,grp.color,facet.by,dom_names)
  
  if (!is.null(facet.by)){
    df_plot <- df_plot |> (\(x) {
      colnames(x)[2] <- "facet"
      x
    })()
  }

  
  if (sub_plot=="_is"){
    
    if (!is.null(facet.by)){
      df_plot |> (\(x)split(x,x$facet))()|> setNames(c("a","b")) |>
        lapply(\(x){
          split(x,x$name=="total") |>
            setNames(c("domains","total"))
        }) |> plot_stitch(grp.color=grp.color)
      
    } else {
      df_plot |> (\(x){
        split(x,x$name=="total")
      })()|>
        setNames(c("domains","total")) |>
        plot_stitch(grp.color=grp.color)
    }
  } else if (sub_plot=="_per"){
    
    if (!is.null(facet.by)){
      df_plot |> (\(x)split(x,x$facet))()|> setNames(c("a","b")) |>
        lapply(\(x){
          split(x,x$name=="total") |>
            setNames(c("domains","total"))
        }) |> plot_stitch(percentile_sub_plot,grp.color=grp.color)
      
    } else {
      df_plot |> (\(x){
        split(x,x$name=="total")
      })()|>
        setNames(c("domains","total")) |>
        plot_stitch(fun=percentile_sub_plot,grp.color=grp.color)
    }
  }
}

#' Plot index and percentile
#'
#' @param data data set
#' @param ... arguments passed
#'
#' @return ggplot list
#' @export
#' 
#' @examples
#' sample_data |> 
#'  index_from_raw() |> 
#'  dplyr::filter(id %in% 1:5) |>
#'  dplyr::filter(ab==1) |>
#'  plot_index2(facet.by="ab")
#' 
plot_index2 <- function(data, ...){
  patchwork::wrap_plots(list(plot_index(data,sub_plot = "_is",...),
                             plot_index(data,sub_plot = "_per",...)),
                        nrow = 2) &
    ggplot2::theme(legend.position = "none")
}


