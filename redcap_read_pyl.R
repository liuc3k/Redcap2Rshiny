#####==redcap_read_pyl==####
##Purpose: Rewrite the function redcap_read_oneshot(), since it cannot read token which is more than 32 characters.
##Ref:https://cran.r-project.org/web/packages/REDCapR/REDCapR.pdf
redcap_read_pyl <- function(
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
  
  validate_field_names(fields, stop_on_error = TRUE)
  
  # ## We're intentionally not exporting this function.
  # collapse_vector <- function(elements, collapsed) {
  #   checkmate::assert_character(collapsed, len=1, any.missing=TRUE, null.ok=TRUE)
  #   
  #   if ((is.null(collapsed) | length(collapsed) == 0L) | all(nchar(collapsed) == 0L)) {
  #     
  #     # This is an empty string if `elements` (eg, fields`) is NULL.
  #     collapsed <- dplyr::if_else(
  #       is.null(elements),
  #       "",
  #       paste0(elements, collapse = ",")
  #     )
  #   }
  #   
  #   collapsed
  # }
  # 
  # ## We're intentionally not exporting this function.
  # filter_logic_prepare <- function(filter_logic) {
  #   # This is an empty string if `filter_logic` is NULL.
  #   if (all(nchar(filter_logic) == 0L))
  #     filter_logic <- dplyr::if_else(is.null(filter_logic), "", filter_logic)
  #   return( filter_logic )
  # }
  # 
  # ## We're intentionally not exporting this function.
  # verbose_prepare <- function (verbose) {
  #   ifelse(!is.null(verbose), verbose, getOption("verbose"))
  # }
  
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
    validate_field_names_collapsed(fields_collapsed, stop_on_error = TRUE)
  
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
# token_file <- "/home/secrets/token"
# 
# input_token <- readLines(token_file)
# url<-"https://appapi.int.rdcrn.org/redcap/pid/105"
# redcap_read_pyl(redcap_uri=url,
#                 token=input_token,
#                 records ='1',
#                 fields =c('local_id','visit'),
#                 events = c('b_arm_1','3_arm_1'))$data
