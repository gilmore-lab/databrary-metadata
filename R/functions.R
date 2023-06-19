# R/functions.R

#------------------------------------------------------------------------------
get_vol_awards <- function(vol_id = 1,
                           funder_str = ".*",
                           vb = TRUE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(length(vol_id) > 1)
  stopifnot(is.character(funder_str))
  stopifnot(is.logical(vb))
  
  vol_awards <- databraryapi::list_volume_funding(vol_id)
  these_awards <- dplyr::filter(vol_awards,
                                stringr::str_detect(funder_name, funder_str))
  if (dim(these_awards)[1] == 0) {
    NULL
  } else {
    these_awards
  }
}

#------------------------------------------------------------------------------
#' Get Volume Funding Information for Multiple Databrary Volumes
#' 
#' @param vols_ids An array of volume IDs
#' @param funder_str A string (regex) to filter funder names. Default is ".*"
#' but "NSF", "NICHD", "NIH", "NIMH", and "NIDA" should also work.
#' @param vb Show verbose output.
#' @returns A data frame with the funding information shared for the selected 
#' Databrary volume or volumes.
get_vols_awards <-
  function(vols_ids = 1:5,
           funder_str = ".*",
           vb = FALSE) {
    stopifnot(is.numeric(vols_ids))
    stopifnot(vols_ids > 0)
    stopifnot(is.character(funder_str))
    stopifnot(is.logical(vb))
    
    purrr::map(vols_ids, get_vol_awards, funder_str, vb, .progress = TRUE) |>
      purrr::list_rbind()
  }

#------------------------------------------------------------------------------
get_multiple_vol_nsf_awards <- function(vol_ids = 1:25,
                                        funder_str = "NSF",
                                        vb = TRUE) {
  stopifnot(is.numeric(vol_ids))
  stopifnot(is.logical(vb))
  
  purrr::map(vol_ids, get_vol_nsf_awards, .progress = vb) |>
    purrr::list_rbind()
}


#------------------------------------------------------------------------------
# https://www.research.gov/common/webapi/awardapisearch-v1.htm
#
# http://api.nsf.gov/services/v1/awards/1052893.json
get_nsf_data_for_award_id <- function(nsf_award_id = 1052893,
                                      format = "json",
                                      return_array = TRUE,
                                      clean_names = TRUE,
                                      as_tibble = TRUE) {
  # stopifnot(is.numeric(nsf_award_id))
  stopifnot(is.character(format))
  stopifnot(is.logical(return_array))
  stopifnot(is.logical(clean_names))
  stopifnot(is.logical(as_tibble))
  
  REQ_URL = paste0("http://api.nsf.gov/services/v1/awards/",
                   nsf_award_id,
                   ".",
                   format)
  
  r <- httr::GET(url = REQ_URL)
  if (httr::status_code(r) == 200) {
    if (return_array) {
      resp <- unlist(httr::content(r))
      if (clean_names) {
        if (is.null(resp)) {
          return(NULL)
        }
        old_names <- names(resp)
        new_names <-
          stringr::str_remove(old_names, "response\\.award\\.")
        names(resp) <- new_names
      }
      if (as_tibble) {
        get
        resp |>
          as.list() |>
          tibble::as_tibble()
      } else {
        resp
      }
    } else {
      httr::content(r)
    }
  }
}

#------------------------------------------------------------------------------
get_mult_nsf_awards <-
  function(ids = c("1238599", "1147440"),
           vb = FALSE) {
    purrr::map(ids, get_nsf_data_for_award_id, .progress = vb) |>
      purrr::list_rbind()
  }

#------------------------------------------------------------------------------
extract_award_id <- function(award_str = "BCS-1238599") {
  stopifnot(is.character(award_str))
  
  award_id <- stringr::str_remove_all(award_str, "-") |>
    stringr::str_extract("[0-9]+")
  award_id
}

#------------------------------------------------------------------------------
add_clean_nsf_award_id <-
  function(nsf_award_df = get_vol_nsf_awards(),
           vb = FALSE) {
    stopifnot(is.data.frame(nsf_award_df))
    
    if (is.null(nsf_award_df))
      return(NULL)
    
    cleaned_ids <-
      purrr::map(nsf_award_df$award, extract_award_id, .progress = vb) |> unlist()
    nsf_award_df$award_id <- cleaned_ids
    nsf_award_df
  }


#------------------------------------------------------------------------------
merge_databrary_nsf_award_df <- function(vol_id = 15) {
  # Get NSF award(s) info from Databrary
  db_nsf_awards_df <- get_vol_awards(vol_id, funder_str = "NSF")
  
  # Add cleaned NSF award IDs, lookup data on NSF, and combine Databrary with
  # NSF info
  if (!is.null(db_nsf_awards_df)) {
    db_nsf_awards_clean_df <- db_nsf_awards_df |>
      add_clean_nsf_award_id()
    
    nsf_awards_df <-
      get_mult_nsf_awards(db_nsf_awards_clean_df$award_id)
    
    dplyr::left_join(db_nsf_awards_clean_df,
                     nsf_awards_df,
                     by = c("award_id" = "id")) |>
      dplyr::select(-funder_id,-award,-agency) |>
      dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
  } else {
    NULL
  }
}

#------------------------------------------------------------------------------
#' Summarize NSF Award Data from Multiple Databrary Volumes.
#'
#' @param vol_ids One or more Databrary volume IDs.
#' @returns A data frame (tibble) that combines the Databrary and NSF database
#' information about the awards from the set of volumes.
#' @export
#' @example summarize_mult_vol_nsf_awards()
summarize_mult_vol_nsf_awards <- function(vol_ids = 1:25) {
  stopifnot(is.numeric(vol_ids))
  
  purrr::map(1:25, merge_databrary_nsf_award_df, .progress = "NSF awards: ") |>
    purrr::list_rbind()
}