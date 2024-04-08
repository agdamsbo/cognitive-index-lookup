#' Calculates index scores from raw domain data input. Assumes first column is id
#'
#' @param ds data set
#' @param indx index file
#' @param raw_columns column names of raw data columns
#' @param mani flag to manipulate index data for plotting
#' @param version.col version column name. The function will check formatting
#' @param age.col age column name
#'
#' @return data.frame
#' @export
#'
#' @examples
#' ds <- cognitive.index.lookup::sample_data
#' ds |> index_from_raw()
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv") |> index_from_raw()
index_from_raw <- function(ds,
                           indx = cognitive.index.lookup::index_table,
                           version.col = "ab",
                           age.col = "age",
                           raw_columns = c("imm", "vis", "ver", "att", "del"),
                           mani = TRUE) {
  version <- ds[[{{ version.col }}]] |> tolower()
  age <- ds[[{{ age.col }}]]

  dt <- ds
  
  ds <- ds |> dplyr::select(tidyselect::all_of(c(version.col,age.col,raw_columns)))
  
  if (!any(c("a", "b") %in% version)) {
    version <- dplyr::case_when(version == "1" ~ "a", version ==
      "2" ~ "b")
  }

  if (any(is.na(version))) {
    stop("The version column should be coded A/a/1 and B/b/2 with no missings.
         It seems there are some missing or the data is formatted wrong")
  }

  ## Categorizing age to age interval of index lists
  ndx_nms <- unique(unlist(sapply(strsplit(unique(indx[["grp"]]), "[_]"), "[[", 2)))[1:6]

  ## This is the only non-generalised part. Please be inspired and solve it your own way! :)
  ## Intervals are 20-39, 40-49, 50-59, 60-69, 70-79, 80-89.
  ## Make universal by drawing interval names (needs to be changed to smth like "index_70.79" to use substr) and the use "for loop".

  # index_age<-
  index_age <- dplyr::case_match(
    age, 18:39 ~ ndx_nms[1],
    40:49 ~ ndx_nms[2],
    50:59 ~ ndx_nms[3],
    60:69 ~ ndx_nms[4],
    70:79 ~ ndx_nms[5],
    80:200 ~ ndx_nms[6]
  )

  # Names of the different cognitive domains assigned

  ## Unique group names from index least
  grps <- unique(indx[["grp"]])

  ## getting only domain names, and excluding the last, total, table, and splitting to list of vectors
  grps_split <- strsplit(grps[-length(grps)], "[_]")

  ## Getting the last element from each split vector, ulisting and only keeping unique names
  cinms <- unique(unlist(lapply(grps_split, function(i) {
    i[[length(i)]]
  })))

  ## Relies on these domain labels being in the right order. Problem for future me..
  ## c("immediate","visuospatial","verbal","attention","delayed")

  # Creating relevant colnames for index, CI and percentile
  abc <- paste0("test_", c(letters[1:length(cinms)], "i"))
  col_names_index <- paste0(abc, "_is", c(paste0("_", cinms), "_total"))
  col_names_95pct <- paste0(abc, "_ci")
  col_names_percentile <- paste0(abc, "_per")

  # Creating DF to populate with extracted data from table look-up
  col_names_all <- c(names(ds)[1], col_names_index, col_names_95pct, col_names_percentile)
  df <- data.frame(matrix(1:length(col_names_all), ncol = length(col_names_all), nrow = nrow(ds), byrow = T))
  df[[1]] <- ds[[1]]
  colnames(df) <- col_names_all

  # dt <- ds

  ## Create one function for when data provided is a list and when it is a data.frame. Currently works with data.frame

  for (i in seq_len(nrow(ds))) {
    # i=1

    ## Selecting tables based on index age classification (all ages included from 18 and above, also above 89)
    lst <- list()
    # j=1

    for (j in seq_along(cinms)) {
      lst[[j]] <- indx |>
        dplyr::filter(grepl(cinms[j], grp)) |>
        dplyr::filter(grepl(index_age[i], grp))
    }

    names(lst) <- cinms

    ## Selecting correct test version
    v <- version[i]

    # Converting each raw score to index score
    ndx <- c()
    X95 <- c()
    per <- c()

    ## Populating variables
    for (s in seq_along(lst)) {
      # Index score
      # s=1
      flt <- lst[[s]] |>
        dplyr::filter(raw == dplyr::select(dt,tidyselect::contains(raw_columns[[s]]))[[1]][i]) |>
        dplyr::filter(ver == v)

      ndx[s] <- flt$index
      # 95 % CI
      X95[s] <- flt$pct95
      # Percentile
      per[s] <- flt$perc
    }

    ## Total index score from index sum
    ttl_scale <- indx |> dplyr::filter(grepl("total_", grp))

    ndx_sum <- sum(as.numeric(ndx))
    flt_ttl <- dplyr::filter(ttl_scale, raw == ndx_sum)

    ndx[length(ndx) + 1] <- flt_ttl$index
    X95[length(X95) + 1] <- flt_ttl$pct95
    per[length(per) + 1] <- flt_ttl$perc

    df[i, 2:ncol(df)] <- c(ndx, X95, per)
  }

  if (mani) {
    sel1 <- colnames(dplyr::select(df, dplyr::ends_with("_per")))
    for (i in sel1) {
      df[, i] <- dplyr::if_else(df[, i] %in% c("> 99.9", ">99.9", "> 99,9", ">99,9"), "99.95",
        dplyr::if_else(df[, i] %in% c("< 0.1", "<0.1", "< 0,1", "<0,1"), "0.05",
          df[, i]
        )
      )
      ## Using the dplyr::if_else for a more stringent vectorisation
    }
  }

  dplyr::left_join(dt,dplyr::tibble(dt[1],df), by=c(names(dt)[1],{{version.col}}))
}
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
#' @export
plot_prep <- function(data,sub_plot,scores,grp.color,facet.by,dom_names){
   requireNamespace("tidyr")
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



#' Helper to import files correctly
#'
#' @param filenames file names
#'
#' @return character vector
#' @export
#'
#' @examples
#' file_extension(list.files(here::here(""))[[2]])[[1]]
file_extension <- function(filenames) {
  sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}

#' Flexible file import based on extension
#'
#' @param file file name
#' @param consider.na character vector of strings to consider as NAs
#'
#' @return tibble
#' @export
#' 
#' @examples
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
read_input <- function(file, consider.na= c("NA", '""',"")){
  
  ext <- file_extension(file)
  
  tryCatch(
    {
      if (ext == "csv") {
        df <- readr::read_csv(file,na = consider.na)
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx2::read_xlsx(file, na.strings = consider.na)
      } else {
        stop("Input file format has to be either '.csv', '.xls' or '.xlsx'")
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(shiny::safeError(e))
    }
  )
  
  df
}