# R/functions.R

#------------------------------------------------------------------------------
get_vol_awards <- function(vol_id = 1,
                           funder_str = ".*",
                           vb = TRUE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(length(vol_id) > 0)
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

get_vol_nsf_awards <-
  function(vol_id = 1,
           vb = FALSE) {
    get_vol_awards(vol_id, funder_str = "NSF", vb)
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
    
    purrr::map(
      vols_ids,
      get_vol_awards,
      funder_str,
      vb,
      .progress = paste0("Awards for '", funder_str, "'.")
    ) |>
      purrr::list_rbind()
  }

#------------------------------------------------------------------------------
get_vols_nsf_awards <- function(vol_ids = 1:100,
                                vb = TRUE) {
  stopifnot(is.numeric(vol_ids))
  stopifnot(is.logical(vb))
  
  get_vols_awards(vol_ids, funder_str = "NSF", vb)
}


#------------------------------------------------------------------------------
# https://www.research.gov/common/webapi/awardapisearch-v1.htm
#' Get NSF funding data for an NSF award ID
#'
#' @param nsf_award_id The NSF award ID number.
#' @param format The type of data to extract from NSF; 'json' or 'xml'.
#' @param return_array Return a tabular array of data or not.
#' @param clean_names Clean up the field names or not.
#' @param as_tibble Convert to a tibble or not.
#' @example get_nsf_data_for_award_id()
# http://api.nsf.gov/services/v1/awards/1052893.json
get_nsf_data_for_award_id <- function(nsf_award_id = 1238599,
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
        # get
        resp |>
          as.list() |>
          tibble::as_tibble() |>
          dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
      } else {
        resp
      }
    } else {
      httr::content(r)
    }
  }
}

#------------------------------------------------------------------------------
# https://www.research.gov/common/webapi/awardapisearch-v1.htm
#
# http://api.nsf.gov/services/v1/awards/1052893.json
get_nsf_data_for_keyword <- function(nsf_keyword = "databrary",
                                     format = "json",
                                     return_array = TRUE,
                                     clean_names = TRUE,
                                     as_tibble = TRUE) {
  stopifnot(is.character(nsf_keyword))
  stopifnot(is.character(format))
  stopifnot(is.logical(return_array))
  stopifnot(is.logical(clean_names))
  stopifnot(is.logical(as_tibble))
  
  REQ_URL = paste0("http://api.nsf.gov/services/v1/awards.",
                   format,
                   "?keyword=",
                   nsf_keyword)
  
  r <- httr::GET(url = REQ_URL)
  if (httr::status_code(r) == 200) {
    if (return_array) {
      resp <- unlist(httr::content(r), recursive = FALSE)
      resps <- resp$response.award
      df <- do.call(rbind.data.frame, resps) |>
        dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
      if (as_tibble) {
        tibble::as_tibble(df)
      } else {
        df
      }
    } else {
      httr::content(r)
    }
  }
}

get_nsf_data_for_pi <- function(pi_last = "Adolph",
                                pi_first = "Karen",
                                     format = "json",
                                     return_array = TRUE,
                                     clean_names = TRUE,
                                     as_tibble = TRUE) {
  stopifnot(is.character(pi_last))
  stopifnot(is.character(format))
  stopifnot(is.logical(return_array))
  stopifnot(is.logical(clean_names))
  stopifnot(is.logical(as_tibble))
  
  REQ_URL = paste0("http://api.nsf.gov/services/v1/awards.",
                   format,
                   "?pdPIName=",
                   stringr::str_to_lower(pi_last))
  
  r <- httr::GET(url = REQ_URL)
  if (httr::status_code(r) == 200) {
    if (return_array) {
      resp <- unlist(httr::content(r), recursive = FALSE)
      resps <- resp$response.award
      df <- do.call(rbind.data.frame, resps) |>
        dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
      if (as_tibble) {
        tibble::as_tibble(df)
      } else {
        df
      }
    } else {
      httr::content(r)
    }
  }
}


# get_nsf_data_for_pi <-
#   function(pi_last = "adolph", pi_first = "karen") {
#     stopifnot(is.character(pi_last))
#     stopifnot(is.character(pi_first))
#     
#     this_df <- get_nsf_data_for_keyword(pi_last)
#     
#     if (!is.null(this_df)) {
#       out_df <- this_df |>
#         dplyr::filter(stringr::str_detect(piLastName, stringr::str_to_title(pi_last)))
#       if (pi_first != "") {
#         out_df <- out_df |>
#           dplyr::filter(stringr::str_detect(piFirstName, stringr::str_to_title(pi_first)))
#       }
#       out_df
#     } else {
#       NULL
#     }
#   }

#------------------------------------------------------------------------------
# https://www.research.gov/common/webapi/awardapisearch-v1.htm
#' Get NSF outcomes for an NSF award ID
#'
#' @param nsf_award_id The NSF award ID number.
#' @param format The type of data to extract from NSF; 'json' or 'xml'.
#' @param return_array Return a tabular array of data or not.
#' @param clean_names Clean up the field names or not.
#' @param as_tibble Convert to a tibble or not.
#' @example get_nsf_data_for_award_id()
# http://api.nsf.gov/services/v1/awards/1052893.json
get_nsf_outcomes_for_award_id <- function(nsf_award_id = 1238599,
                                          format = "json",
                                          return_array = FALSE,
                                          clean_names = FALSE,
                                          as_tibble = FALSE) {
  stopifnot(is.numeric(nsf_award_id))
  stopifnot(is.character(format))
  stopifnot(is.logical(return_array))
  stopifnot(is.logical(clean_names))
  stopifnot(is.logical(as_tibble))
  
  REQ_URL = paste0(
    "http://api.nsf.gov/services/v1/awards/",
    nsf_award_id,
    "/projectoutcomes.",
    format
  )
  
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
        # get
        resp |>
          as.list() |>
          tibble::as_tibble() |>
          dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
      } else {
        resp
      }
    } else {
      httr::content(r)
    }
  }
  httr::content(r)
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
merge_databrary_nsf_award_df <- function(vol_id, vb = FALSE) {
  stopifnot(is.numeric(vol_ids))
  stopifnot(vol_ids > 0)
  stopifnot(is.logical(vb))
  
  # Get NSF award(s) info from Databrary
  if (vb) message("Volume: ", vol_id)
  db_nsf_awards_df <- get_vol_awards(vol_id, funder_str = "NSF")
  
  # Add cleaned NSF award IDs, lookup data on NSF, and combine Databrary with
  # NSF info
  if (!is.null(db_nsf_awards_df)) {
    if (vb) message(" Cleaning award IDs")
    db_nsf_awards_clean_df <- db_nsf_awards_df |>
      add_clean_nsf_award_id()
    
    if (vb) message(" Retrieving award info from NSF")
    nsf_awards_df <-
      get_mult_nsf_awards(db_nsf_awards_clean_df$award_id)
    
    if (dim(nsf_awards_df)[1] < 1) {
      message(" No NSF data found for award ", db_nsf_awards_clean_df$award_id)
      return(NULL)
    }
    
    if (vb) message(" Merging Databrary & NSF data")
    dplyr::left_join(db_nsf_awards_clean_df,
                     nsf_awards_df,
                     by = c("award_id" = "id")) |>
      dplyr::select(-funder_id, -award, -agency) |>
      # Some NSF database cities are in all caps
      dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
      # TODO: Consider rectifying camel case with snake case field names
  } else {
    if (vb) message(" No NSF awards in volume ", vol_id)
    NULL
  }
}

#------------------------------------------------------------------------------
#' Summarize NSF Award Data For Multiple Databrary Volumes.
#'
#' @param vol_ids Numeric array of one or more Databrary volume IDs.
#' @returns A data frame (tibble) that combines the Databrary and NSF database
#' information about the NSF awards reported in `vol_ids`.
#' @example summarize_mult_vol_nsf_awards() # Summarizes NSF funding for Databrary
#' volumes 1 through 25.
#' @export
summarize_mult_vol_nsf_awards <- function(vol_ids = 1:25, vb = FALSE) {
  stopifnot(is.numeric(vol_ids))
  stopifnot(sum(vol_ids > 0) == length(vol_ids))
  stopifnot(min(vol_ids) < max(vol_ids))
  
  message("Retrieving NSF award info for Databrary volumes ", min(vol_ids), ":", max(vol_ids))
  purrr::map(vol_ids, merge_databrary_nsf_award_df, vb = vb,
             .progress = "NSF awards for vols: ") |>
    purrr::list_rbind()
}