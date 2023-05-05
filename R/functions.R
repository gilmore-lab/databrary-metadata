# R/functions.R

#------------------------------------------------------------------------------
# TODO: Finish this function
# select_nsf_funded_vols <- function(vb = TRUE,
#                                    db_login = Sys.getenv("DATABRARY_LOGIN")) {
#   stopifnot(is.logical(vb))
#   
#   if (db_login == "") {
#     message("DATABRARY_LOGIN not in ~/.Renviron")
#     db_login <-
#       readline(prompt = "Enter your Databrary login (email): ")
#     if (db_login == "" || !is.character(db_login)) {
#       stop("Invalid DATABRARY_LOGIN. Cannot continue.")
#     }
#   }
#   
#   if (!databraryapi::login_db(db_login)) {
#     stop("Databrary login failed.")
#   }
# }

#------------------------------------------------------------------------------
get_vol_nsf_awards <- function(vol_id = 1,
                               vb = TRUE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(is.logical(vb))
  
  vol_awards <- databraryapi::list_volume_funding(vol_id)
  nsf_awards <- dplyr::filter(vol_awards,
                              stringr::str_detect(funder_name, "NSF"))
  if (dim(nsf_awards)[1] == 0) {
    NULL
  } else {
    nsf_awards
  }
}

#------------------------------------------------------------------------------
get_multiple_vol_nsf_awards <- function(vol_ids = 1:25,
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
      if (as_tibble) {get
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
get_mult_nsf_awards <- function(ids, vb = FALSE){
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
    
    cleaned_ids <-
      purrr::map(nsf_award_df$award, extract_award_id, .progress = vb) |> unlist()
    nsf_award_df$award_id <- cleaned_ids
    nsf_award_df
  }


#------------------------------------------------------------------------------
# NOT WORKING YET
merge_databrary_nsf_award_df <- function(vol_id = 15) {
  db_nsf_awards_df <- get_vol_nsf_awards(vol_id) |>
    dplyr::filter(!is.null()) |>
    add_clean_nsf_award_id()
  
  nsf_awards_df <- get_mult_nsf_awards(db_nsf_awards_df$id)
  
  dplyr::left_join(db_nsf_awards_df, nsf_awards_df, by = "id")
}