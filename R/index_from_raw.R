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
index_from_raw <- function(ds,
                           indx = cognitive.index.lookup::index_table,
                           version.col = "ab",
                           age.col = "age",
                           raw_columns = c("imm", "vis", "ver", "att", "del"),
                           mani = TRUE) {
  version <- ds[[{{ version.col }}]] |> tolower()
  age <- ds[[{{ age.col }}]]

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

  dt <- ds

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
        dplyr::filter(raw == dplyr::select(dt,tidyselect::contains(raw_columns[s]))[i,]) |>
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

  dplyr::left_join(ds,dplyr::tibble(df, ds[{{version.col}}]), by=c(names(ds)[1],{{version.col}}))
}
