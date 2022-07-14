#####==Read Metadata===#####
##Purpose: Rewrite the function redcap_read_oneshot(), since it cannot read token which is more than 32 characters.
##Ref:https://cran.r-project.org/web/packages/REDCapR/REDCapR.pdf
##    https://rdrr.io/cran/REDCapR/
redcap_metadata_read_pyl <- function(
  redcap_uri,
  token,
  forms             = NULL,
  forms_collapsed   = "",
  fields            = NULL,
  fields_collapsed  = "",
  verbose           = TRUE,
  config_options    = NULL
) {
  
  checkmate::assert_character(redcap_uri  , any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_character(token       , any.missing=FALSE, len=1, pattern="^.{1,}$")
  
  validate_field_names(fields, stop_on_error = TRUE)
  
  # token               <- sanitize_token(token)
  token               <- token
  fields_collapsed    <- REDCapR:::collapse_vector(fields   , fields_collapsed)
  forms_collapsed     <- REDCapR:::collapse_vector(forms    , forms_collapsed)
  verbose             <- REDCapR:::verbose_prepare(verbose)
  
  if (1L <= nchar(fields_collapsed) )
    REDCapR:::validate_field_names_collapsed(fields_collapsed, stop_on_error = TRUE)
  
  post_body <- list(
    token    = token,
    content  = "metadata",
    format   = "csv",
    forms    = forms_collapsed,
    fields   = fields_collapsed
  )
  
  # This is the important line that communicates with the REDCap server.
  kernel <- REDCapR:::kernel_api(redcap_uri, post_body, config_options)
  
  if (kernel$success) {
    col_types <-
      readr::cols(
        field_name  = readr::col_character(),
        .default    = readr::col_character()
      )
    
    try(
      # Convert the raw text to a dataset.
      ds <-
        readr::read_csv(
          file            = I(kernel$raw_text),
          col_types       = col_types,
          show_col_types  = FALSE
        ),
      # Don't print the warning in the try block.  Print it below,
      #   where it's under the control of the caller.
      silent = TRUE
    )
    
    if (exists("ds") & inherits(ds, "data.frame")) {
      outcome_message <- sprintf(
        "The data dictionary describing %s fields was read from REDCap in %0.1f seconds.  The http status code was %i.",
        format(nrow(ds), big.mark = ",", scientific = FALSE, trim = TRUE),
        kernel$elapsed_seconds,
        kernel$status_code
      )
      
      # If an operation is successful, the `raw_text` is no longer returned
      #   to save RAM.  The content is not really necessary with httr's status
      #   message exposed.
      kernel$raw_text   <- ""
    } else { # nocov start
      # Override the 'success' determination from the http status code
      #   and return an empty data.frame.
      kernel$success    <- FALSE
      ds                <- data.frame()
      outcome_message   <- sprintf(
        "The REDCap metadata export failed.  The http status code was %i.  The 'raw_text' returned was '%s'.",
        kernel$status_code,
        kernel$raw_text
      )
    }       # nocov end
  } else {
    ds                  <- data.frame() #Return an empty data.frame
    outcome_message     <- sprintf(
      "The REDCapR metadata export operation was not successful.  The error message was:\n%s",
      kernel$raw_text
    )
  }
  
  if (verbose)
    message(outcome_message)
  
  list(
    data               = ds,
    success            = kernel$success,
    status_code        = kernel$status_code,
    outcome_message    = outcome_message,
    forms_collapsed    = forms_collapsed,
    fields_collapsed   = fields_collapsed,
    elapsed_seconds    = kernel$elapsed_seconds,
    raw_text           = kernel$raw_text
  )
}
###Example:
# token <- "ED99ABDA97C387C71C476474433DFB2B"
# url <- "https://rc.rarediseasesnetwork.org/api/"
# redcap_metadata_read_pyl(redcap_uri=url,token = token)


#####==Read dataset without customized batch size==####
##Purpose: Rewrite the function redcap_read_oneshot(), since it cannot read token which is more than 32 characters.
##Ref:https://cran.r-project.org/web/packages/REDCapR/REDCapR.pdf
##    https://rdrr.io/cran/REDCapR/
redcap_read_oneshot_pyl <- function(
  redcap_uri,
  token,
  records                       = NULL,
  records_collapsed             = "",
  fields                        = NULL,
  fields_collapsed              = "",
  forms                         = NULL,
  forms_collapsed               = "",
  events                        = NULL,
  events_collapsed              = "",
  raw_or_label                  = "raw",
  raw_or_label_headers          = "raw",
  export_checkbox_label         = FALSE,
  # placeholder returnFormat
  export_survey_fields          = FALSE,
  export_data_access_groups     = FALSE,
  filter_logic                  = "",
  datetime_range_begin          = as.POSIXct(NA),
  datetime_range_end            = as.POSIXct(NA),
  
  col_types                     = NULL,
  guess_type                    = TRUE,
  guess_max                     = 1000L,
  verbose                       = TRUE,
  config_options                = NULL
) {
  library(REDCapR)
  checkmate::assert_character(redcap_uri                , any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_character(token                     , any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_atomic(records                      , any.missing=TRUE , min.len=0)
  checkmate::assert_character(records_collapsed         , any.missing=TRUE , len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(fields                    , any.missing=TRUE , min.len=1, pattern="^.{1,}$", null.ok=TRUE)
  checkmate::assert_character(fields_collapsed          , any.missing=TRUE , len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(forms                     , any.missing=TRUE , min.len=1, pattern="^.{1,}$", null.ok=TRUE)
  checkmate::assert_character(forms_collapsed           , any.missing=TRUE , len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(events                    , any.missing=TRUE , min.len=1, pattern="^.{1,}$", null.ok=TRUE)
  checkmate::assert_character(events_collapsed          , any.missing=TRUE , len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(raw_or_label              , any.missing=FALSE, len=1)
  checkmate::assert_subset(   raw_or_label              , c("raw", "label"))
  checkmate::assert_character(raw_or_label_headers      , any.missing=FALSE, len=1)
  checkmate::assert_subset(   raw_or_label_headers      , c("raw", "label"))
  checkmate::assert_logical(  export_checkbox_label     , any.missing=FALSE, len=1)
  # placeholder: returnFormat
  checkmate::assert_logical(  export_survey_fields      , any.missing=FALSE, len=1)
  checkmate::assert_logical(  export_data_access_groups , any.missing=FALSE, len=1)
  checkmate::assert_character(filter_logic              , any.missing=FALSE, len=1, pattern="^.{0,}$")
  checkmate::assert_posixct(  datetime_range_begin      , any.missing=TRUE , len=1, null.ok=TRUE)
  checkmate::assert_posixct(  datetime_range_end        , any.missing=TRUE , len=1, null.ok=TRUE)
  
  checkmate::assert_logical(  guess_type                , any.missing=FALSE, len=1)
  checkmate::assert_integerish(guess_max                , any.missing=FALSE, len=1, lower=1)
  checkmate::assert_logical(  verbose                   , any.missing=FALSE, len=1, null.ok=TRUE)
  checkmate::assert_list(     config_options            , any.missing=TRUE ,        null.ok=TRUE)
  
  REDCapR:::validate_field_names(fields, stop_on_error = TRUE)
  
  
  
  # token               <- sanitize_token(token)
  token               <- token
  records_collapsed   <- REDCapR:::collapse_vector(records  , records_collapsed)
  fields_collapsed    <- REDCapR:::collapse_vector(fields   , fields_collapsed)
  forms_collapsed     <- REDCapR:::collapse_vector(forms    , forms_collapsed)
  events_collapsed    <- REDCapR:::collapse_vector(events   , events_collapsed)
  filter_logic        <- REDCapR:::filter_logic_prepare(filter_logic)
  datetime_range_begin<- dplyr::coalesce(strftime(datetime_range_begin, "%Y-%m-%d %H:%M:%S"), "")
  datetime_range_end  <- dplyr::coalesce(strftime(datetime_range_end  , "%Y-%m-%d %H:%M:%S"), "")
  verbose             <- REDCapR:::verbose_prepare(verbose)
  
  if (1L <= nchar(fields_collapsed) )
    REDCapR::: validate_field_names_collapsed(fields_collapsed, stop_on_error = TRUE)
  
  post_body <- list(
    "token"                   = token,
    content                 = "record",
    format                  = "csv",
    type                    = "flat",
    rawOrLabel              = raw_or_label,
    rawOrLabelHeaders       = raw_or_label_headers,
    exportCheckboxLabel     = tolower(as.character(export_checkbox_label)),
    # placeholder: returnFormat
    exportSurveyFields      = tolower(as.character(export_survey_fields)),
    exportDataAccessGroups  = tolower(as.character(export_data_access_groups)),
    filterLogic             = filter_logic,
    dateRangeBegin          = datetime_range_begin,
    dateRangeEnd            = datetime_range_end
    # record, fields, forms & events are specified below
  )
  
  if (0L < nchar(records_collapsed)) post_body$records  <- records_collapsed
  if (0L < nchar(fields_collapsed )) post_body$fields   <- fields_collapsed
  if (0L < nchar(forms_collapsed  )) post_body$forms    <- forms_collapsed
  if (0L < nchar(events_collapsed )) post_body$events   <- events_collapsed
  
  # This is the important line that communicates with the REDCap server.
  kernel <-  REDCapR:::kernel_api(redcap_uri, post_body, config_options)
  
  if (kernel$success) {
    col_types <-
      if (!is.null(col_types)) col_types
    else if (guess_type)     NULL
    else                     readr::cols(.default = readr::col_character())
    
    try(
      # Convert the raw text to a dataset.
      ds <-
        readr::read_csv(
          file            = I(kernel$raw_text),
          col_types       = col_types,
          guess_max       = guess_max,
          show_col_types  = FALSE
        ) %>%
        as.data.frame(),
      
      # Don't print the warning in the try block.  Print it below,
      #   where it's under the control of the caller.
      silent = TRUE
    )
    
    if (exists("ds") & inherits(ds, "data.frame")) {
      outcome_message <- sprintf(
        "%s records and %s columns were read from REDCap in %0.1f seconds.  The http status code was %i.",
        format(  nrow(ds), big.mark = ",", scientific = FALSE, trim = TRUE),
        format(length(ds), big.mark = ",", scientific = FALSE, trim = TRUE),
        kernel$elapsed_seconds,
        kernel$status_code
      )
      
      # ds <- dplyr::mutate_if(
      #   ds,
      #   is.character,
      #   function(x) dplyr::coalesce(x, "") #Replace NAs with blanks
      # )
      # 
      # ds <- dplyr::mutate_if(
      #   ds,
      #   is.character,
      #   function( x ) gsub("\r\n", "\n", x, perl=TRUE)
      # )
      # ds <- dplyr::mutate_if(
      #   ds,
      #   function( x) inherits(x, "Date"),
      #   as.character
      # )
      # 
      # ds <- base::as.data.frame(ds)
      
      # If an operation is successful, the `raw_text` is no longer returned to
      #   save RAM.  The content is not really necessary with httr's status
      #   message exposed.
      kernel$raw_text   <- ""
    } else { # ds doesn't exist as a data.frame.
      # nocov start
      # Override the 'success' determination from the http status code.
      #   and return an empty data.frame.
      kernel$success   <- FALSE
      ds               <- data.frame()
      outcome_message  <- sprintf(
        "The REDCap read failed.  The http status code was %i.  The 'raw_text' returned was '%s'.",
        kernel$status_code,
        kernel$raw_text
      )
      # nocov end
    }
  } else { # kernel fails
    ds              <- data.frame() #Return an empty data.frame
    outcome_message <- if (any(grepl(kernel$regex_empty, kernel$raw_text))) {
      "The REDCapR read/export operation was not successful.  The returned dataset was empty."  # nocov
    } else {
      sprintf(
        "The REDCapR read/export operation was not successful.  The error message was:\n%s",
        kernel$raw_text
      )
    }
  }
  
  if (verbose)
    message(outcome_message)
  
  list(
    data               = ds,
    success            = kernel$success,
    status_code        = kernel$status_code,
    outcome_message    = outcome_message,
    records_collapsed  = records_collapsed,
    fields_collapsed   = fields_collapsed,
    forms_collapsed    = forms_collapsed,
    events_collapsed   = events_collapsed,
    filter_logic       = filter_logic,
    datetime_range_begin   = datetime_range_begin,
    datetime_range_end     = datetime_range_end,
    elapsed_seconds    = kernel$elapsed_seconds,
    raw_text           = kernel$raw_text
  )
}

###Example: 
# token <- "ED99ABDA97C387C71C476474433DFB2B"
# url <- "https://rc.rarediseasesnetwork.org/api/"
# 
# redcap_read_oneshot_pyl(redcap_uri=uri,
#                 token=token,
#                 records ='1')


###=====Read data with customized batch size====####
redcap_read_large <- function(
  batch_size                    = 100L,
  interbatch_delay              = 0.5,
  continue_on_error             = FALSE,
  redcap_uri,
  token,
  records                       = NULL, records_collapsed = "",
  fields                        = NULL, fields_collapsed  = "",
  forms                         = NULL, forms_collapsed   = "",
  events                        = NULL, events_collapsed  = "",
  raw_or_label                  = "raw",
  raw_or_label_headers          = "raw",
  export_checkbox_label         = FALSE,
  # placeholder: returnFormat
  export_survey_fields          = FALSE,
  export_data_access_groups     = FALSE,
  filter_logic                  = "",
  datetime_range_begin          = as.POSIXct(NA),
  datetime_range_end            = as.POSIXct(NA),
  
  col_types                     = NULL,
  guess_type                    = TRUE,
  guess_max                     = NULL, # Deprecated parameter
  verbose                       = TRUE,
  config_options                = NULL,
  id_position                   = 1L
) {
  
  checkmate::assert_character(redcap_uri                , any.missing=FALSE,     len=1, pattern="^.{1,}$")
  checkmate::assert_character(token                     , any.missing=FALSE,     len=1, pattern="^.{1,}$")
  checkmate::assert_atomic(  records                    , any.missing=TRUE, min.len=0)
  checkmate::assert_character(records_collapsed         , any.missing=TRUE ,     len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(fields                    , any.missing=TRUE , min.len=1, pattern="^.{1,}$", null.ok=TRUE)
  checkmate::assert_character(fields_collapsed          , any.missing=TRUE ,     len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(forms                     , any.missing=TRUE , min.len=1, pattern="^.{1,}$", null.ok=TRUE)
  checkmate::assert_character(forms_collapsed           , any.missing=TRUE ,     len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(events                    , any.missing=TRUE , min.len=1, pattern="^.{1,}$", null.ok=TRUE)
  checkmate::assert_character(events_collapsed          , any.missing=TRUE ,     len=1, pattern="^.{0,}$", null.ok=TRUE)
  checkmate::assert_character(raw_or_label              , any.missing=FALSE,     len=1)
  checkmate::assert_subset(   raw_or_label              , c("raw", "label"))
  checkmate::assert_character(raw_or_label_headers      , any.missing=FALSE,     len=1)
  checkmate::assert_subset(   raw_or_label_headers      , c("raw", "label"))
  checkmate::assert_logical(  export_checkbox_label     , any.missing=FALSE,     len=1)
  # placeholder: returnFormat
  checkmate::assert_logical(  export_survey_fields      , any.missing=FALSE,     len=1)
  checkmate::assert_logical(  export_data_access_groups , any.missing=FALSE,     len=1)
  checkmate::assert_posixct(  datetime_range_begin      , any.missing=TRUE , len=1, null.ok=TRUE)
  checkmate::assert_posixct(  datetime_range_end        , any.missing=TRUE , len=1, null.ok=TRUE)
  
  checkmate::assert_logical(  guess_type                , any.missing=FALSE,     len=1)
  
  if (!is.null(guess_max)) warning("The `guess_max` parameter in `REDCapR::redcap_read()` is deprecated.")
  checkmate::assert_logical(  verbose                   , any.missing=FALSE,     len=1, null.ok=TRUE)
  checkmate::assert_list(     config_options            , any.missing=TRUE ,            null.ok=TRUE)
  checkmate::assert_integer(  id_position               , any.missing=FALSE,     len=1, lower=1L)
  
  validate_field_names(fields, stop_on_error = TRUE)
  
  #token               <- sanitize_token(token)
  token               <- token
  records_collapsed   <- REDCapR:::collapse_vector(records  , records_collapsed)
  fields_collapsed    <- REDCapR:::collapse_vector(fields   , fields_collapsed)
  forms_collapsed     <- REDCapR:::collapse_vector(forms    , forms_collapsed)
  events_collapsed    <- REDCapR:::collapse_vector(events   , events_collapsed)
  filter_logic        <- REDCapR:::filter_logic_prepare(filter_logic)
  verbose             <- REDCapR:::verbose_prepare(verbose)
  
  if (1L <= nchar(fields_collapsed) )
    validate_field_names_collapsed(fields_collapsed, stop_on_error = TRUE)
  
  start_time <- Sys.time()
  
  metadata <- redcap_metadata_read_pyl(
    redcap_uri         = redcap_uri,
    token              = token,
    verbose            = verbose,
    config_options     = config_options
  )
  
  # if (!metadata$success) {
  #   error_message     <- sprintf(
  #     "The REDCapR record export operation was not successful.  The error message was:\n%s",
  #     metadata$raw_text
  #   )
  #   stop(error_message)
  # }
  
  initial_call <- redcap_read_pyl(
    redcap_uri         = redcap_uri,
    token              = token,
    records_collapsed  = records_collapsed,
    fields_collapsed   = metadata$data$field_name[id_position],
    forms_collapsed    = forms_collapsed,
    events_collapsed   = events_collapsed,
    filter_logic       = filter_logic,
    datetime_range_begin   = datetime_range_begin,
    datetime_range_end     = datetime_range_end,
    guess_type         = guess_type,
    verbose            = verbose,
    config_options     = config_options
  )
  
  # Stop and return to the caller if the initial query failed. --------------
  if (!initial_call$success) {
    outcome_messages  <- paste0("The initial call failed with the code: ", initial_call$status_code, ".")
    elapsed_seconds   <- as.numeric(difftime(Sys.time(), start_time, units="secs"))
    return(list(
      data                  = data.frame(),
      records_collapsed     = "failed in initial batch call",
      fields_collapsed      = "failed in initial batch call",
      forms_collapsed       = "failed in initial batch call",
      events_collapsed      = "failed in initial batch call",
      filter_logic          = "failed in initial batch call",
      datetime_range_begin  = "failed in initial batch call",
      datetime_range_end    = "failed in initial batch call",
      elapsed_seconds       = elapsed_seconds,
      status_code           = initial_call$status_code,
      outcome_messages      = outcome_messages,
      success               = initial_call$success
    ))
  }
  
  # Continue as intended if the initial query succeeded. --------------------
  unique_ids <- sort(unique(initial_call$data[[id_position]]))
  
  if (all(nchar(unique_ids)==32L))
    warn_hash_record_id()  # nocov
  
  ds_glossary            <- REDCapR::create_batch_glossary(row_count=length(unique_ids), batch_size=batch_size)
  lst_batch              <- NULL
  lst_status_code        <- NULL
  # lst_status_message placeholder
  lst_outcome_message    <- NULL
  success_combined       <- TRUE
  
  message("Starting to read ", format(length(unique_ids), big.mark=",", scientific=FALSE, trim=TRUE), " records  at ", Sys.time(), ".")
  for (i in ds_glossary$id) {
    selected_index  <- seq(from=ds_glossary$start_index[i], to=ds_glossary$stop_index[i])
    selected_ids    <- unique_ids[selected_index]
    
    if (i > 0) Sys.sleep(time = interbatch_delay)
    if (verbose) {
      message(
        "Reading batch ", i, " of ", nrow(ds_glossary), ", with subjects ",
        min(selected_ids), " through ", max(selected_ids),
        " (ie, ", length(selected_ids), " unique subject records)."
      )
    }
    read_result <- redcap_read_pyl(
      redcap_uri                  = redcap_uri,
      token                       = token,
      records                     = selected_ids,
      fields_collapsed            = fields_collapsed,
      events_collapsed            = events_collapsed,
      forms_collapsed             = forms_collapsed,
      raw_or_label                = raw_or_label,
      raw_or_label_headers        = raw_or_label_headers,
      export_checkbox_label       = export_checkbox_label,
      # placeholder: return_format
      export_survey_fields        = export_survey_fields,
      export_data_access_groups   = export_data_access_groups,
      filter_logic                = filter_logic,
      datetime_range_begin        = datetime_range_begin,
      datetime_range_end          = datetime_range_end,
      
      col_types                   = col_types,
      guess_type                  = FALSE,
      # guess_max                   = guess_max, # Not used, because guess_type is FALSE
      verbose                     = verbose,
      config_options              = config_options
    )
    
    lst_status_code[[i]]      <- read_result$status_code
    lst_outcome_message[[i]]  <- read_result$outcome_message
    
    if (!read_result$success) {
      # nocov start
      error_message <- sprintf(
        "The `redcap_read()` call failed on iteration %i.",
        i
      )
      error_message <- paste(
        error_message,
        ifelse(
          !verbose,
          "Set the `verbose` parameter to TRUE and rerun for additional information.",
          ""
        )
      )
      
      if (continue_on_error) warning(error_message)
      else stop(error_message)
      # nocov end
    }
    
    lst_batch[[i]]   <- read_result$data
    success_combined <- success_combined & read_result$success
    
    rm(read_result) #Admittedly overkill defensiveness.
  }
  
  ds_stacked               <- as.data.frame(dplyr::bind_rows(lst_batch))
  
  if (is.null(col_types) && guess_type) {
    ds_stacked <-
      ds_stacked %>%
      readr::type_convert()
  }
  
  elapsed_seconds          <- as.numeric(difftime( Sys.time(), start_time, units="secs"))
  status_code_combined     <- paste(lst_status_code    , collapse="; ")
  outcome_message_combined <- paste(lst_outcome_message, collapse="; ")
  
  list(
    data                = ds_stacked,
    success             = success_combined,
    status_codes        = status_code_combined,
    outcome_messages    = outcome_message_combined,
    # data_types          = data_types,
    records_collapsed   = records_collapsed,
    fields_collapsed    = fields_collapsed,
    forms_collapsed     = forms_collapsed,
    events_collapsed    = events_collapsed,
    filter_logic        = filter_logic,
    datetime_range_begin= datetime_range_begin,
    datetime_range_end  = datetime_range_end,
    
    elapsed_seconds     = elapsed_seconds
  )
}
