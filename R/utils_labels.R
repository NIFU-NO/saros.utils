#' Remove Special Characters (<,>) in Variable Labels
#'
#' @param df Data frame
#'
#' @return A data frame
#' @export
remove_special_chars_in_labels <-
  function(df) {
    # z <-
    #   labelled::val_labels(df)
    z <-
      lapply(X = df, FUN = function(x) attr(x, "labels"))
    z <-
      lapply(X = seq_along(z), FUN = function(i) {
        x <- z[[i]]
        y <- names(z)[[i]]

                    if(!is.null(x) && any(grepl("<|>", names(x)))) {
                      cli::cli_warn(c(
                        "Current version of function doesn't handle special characters `<` or `>` in labels.",
                        i="Will remove these in {{y}}"))
                      names(x) <- stringi::stri_replace_all(str = names(x), regex = "<|>", replacement = "")
                    }
                    x
                  })
    for(i in seq_len(ncol(df))) {
      attr(df[[i]], "labels") <- z[[i]]
    }
    # labelled::val_labels(df) <- z
    df
  }



# Helper function to extract raw variable labels from the data
get_raw_labels <-
  function(data, col_pos = NULL, return_as_list = FALSE) {
    if(is.null(col_pos)) col_pos <- colnames(data)
    out <- lapply(X = stats::setNames(col_pos, nm=col_pos),
                  FUN = function(.x) {
                    y <- attr(data[[.x]], "label")
                    if(rlang::is_string(y)) y else NA_character_
                  })
    if(isFALSE(return_as_list)) out <- unlist(out)
    out
  }
