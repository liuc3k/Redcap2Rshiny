#####==redcap_report_pyl==####
##Purpose: Rewrite the function redcap_read_oneshot(), since it cannot read token which is more than 32 characters.
##Ref:https://cran.r-project.org/web/packages/REDCapR/REDCapR.pdf
redcap_report_pyl <- function(
  redcap_uri,
  token,
  report_id,
  raw_or_label                  = "raw",
  raw_or_label_headers          = "raw",
  export_checkbox_label         = FALSE,
  
  col_types                     = NULL,
  guess_type                    = TRUE,
  guess_max                     = 1000L,
  verbose                       = TRUE,
  config_options                = NULL
) {
  
  checkmate::assert_character(redcap_uri                , any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_character(token                     , any.missing=FALSE, len=1, pattern="^.{1,}$")
  checkmate::assert_integerish(report_id                , any.missing=FALSE, len=1, lower=1)
  checkmate::assert_character(raw_or_label              , any.missing=FALSE, len=1)
  checkmate::assert_subset(   raw_or_label              , c("raw", "label"))
  checkmate::assert_character(raw_or_label_headers      , any.missing=FALSE, len=1)
  checkmate::assert_subset(   raw_or_label_headers      , c("raw", "label"))
  checkmate::assert_logical(  export_checkbox_label     , any.missing=FALSE, len=1)
  
  checkmate::assert_logical(  guess_type                , any.missing=FALSE, len=1)
  checkmate::assert_integerish(guess_max                , any.missing=FALSE, len=1, lower=1)
  checkmate::assert_logical(  verbose                   , any.missing=FALSE, len=1, null.ok=TRUE)
  checkmate::assert_list(     config_options            , any.missing=TRUE ,        null.ok=TRUE)
  
  # token               <- sanitize_token(token)
  token               <- token
  verbose             <- REDCapR:::verbose_prepare(verbose)
  
  post_body <- list(
    'token'                   = token,
    content                 = "report",
    report_id               = report_id,
    format                  = "csv",
    rawOrLabel              = raw_or_label,
    rawOrLabelHeaders       = raw_or_label_headers,
    exportCheckboxLabel     = tolower(as.character(export_checkbox_label))
  )
  
  # This is the important line that communicates with the REDCap server.
  kernel <- REDCapR:::kernel_api(redcap_uri, post_body, config_options)
  
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
        "The REDCap report failed.  The http status code was %i.  The 'raw_text' returned was '%s'.",
        kernel$status_code,
        kernel$raw_text
      )
      # nocov end
    }
  } else { # kernel fails
    ds              <- data.frame() #Return an empty data.frame
    outcome_message <- if (any(grepl(kernel$regex_empty, kernel$raw_text))) {
      "The REDCapR report operation was not successful.  The returned dataset was empty."  # nocov
    } else {
      sprintf(
        "The REDCapR report operation was not successful.  The error message was:\n%s",
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
    report_id          = report_id,
    elapsed_seconds    = kernel$elapsed_seconds,
    raw_text           = kernel$raw_text
  )
}