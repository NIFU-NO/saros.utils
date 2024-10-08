#' Re-attach label information from Qualtrics not obtained from regular data downloads
#'
#' @param data Data.frame with original variable names.
#' @param questions Data frame with questions obtained from `qualtRics::survey_questions()`
#' @param reverse_stata_replacement If variable names have already been modified
#' @param questions_var String, indicating column name in `questions` that indicates column names.
#' @param questions_question String, indicating column name in `questions` for the full question.
#' with full stops changed to underscores, this will reverse them for connection. Rarely needed. Defaults to FALSE.
#'
#' @returns Data returned with only variable labels modified.
#' @export
attach_qualtrics_labels <- function(data, questions, reverse_stata_replacement=FALSE,
                                    questions_var="qname", questions_question="question") {
  if(!inherits(data, "data.frame")) cli::cli_abort("{.arg data} must be of type data.frame, not {.obj_type_friendly {data}}.")
  for(col in colnames(data)) {


    patterns <- c("^[[:alpha:]]+[0-9]+_[0-9]+_[[:alnum:]]+",
                  "^[[:alpha:]]+[0-9]+_[[:alnum:]]+",
                  "^[[:alpha:]]+[[:alnum:]]+")
    col2 <- col
    for(pat in patterns) {
      if(stringi::stri_count_fixed(col2, pattern = "_") == 4-match(pat, patterns)) {
        col2 <- stringi::stri_extract_first_regex(col2, pattern = pat)
        break
      }
    }
    if(reverse_stata_replacement) col2 <- stringi::stri_replace_first_fixed(col2, pattern = "_", replacement = ".")


    if(!is.na(col2)) {
      main_question <- unname(questions[questions[[questions_var]] == col2, questions_question, drop=TRUE])
      main_question <- stringi::stri_trim_both(main_question)
      main_question <- stringi::stri_remove_empty_na(main_question)
      if(length(main_question)>0 && !all(is.na(main_question))) {
        attr(data[[col]], "label") <- stringi::stri_c(main_question, " - ", attr(data[[col]], "label"), ignore_null = TRUE)
      }
    }

  }
  cli::cli_progress_done("Finished!")
  data
}



#' Sanitize labels originating from e.g. `Qualtrics` for use in `saros`
#'
#' This function is quite specific to a few problems, users might find it lacking in functionality.
#'
#' @param data data.frame or tibble
#' @param sep String, separates main question from subquestion
#' @param multi_sep_replacement String. If multiple sep are found, replace the first ones with this.
#' @param replace_ascii_with_utf Flag. If TRUE, downloads a list from W3 used to convert html characters as ASCII to UTF8.
#' @param questions Data frame with questions obtained from `qualtRics::survey_questions()`
#'
#' @returns Identical data.frame as input, with only variable labels changed.
#' @export
#'
sanitize_labels <- function(data,
                            sep = " - ",
                            multi_sep_replacement = ": ",
                            replace_ascii_with_utf = FALSE,
                            questions = NULL) {

  # scrape lookup table of accented char html codes, from the 2nd table on this page
  if(isTRUE(replace_ascii_with_utf)) {
  ref_url <- 'http://www.w3schools.com/charsets/ref_html_8859.asp'
  cols <- c("Character", "Entity Name")
  char_table <- rvest::read_html(ref_url)
  char_table <- rvest::html_table(char_table)
  char_table <- char_table[1:4]
  char_table <- lapply(char_table, function(x) x[, cols])
  char_table <- do.call(rbind, char_table)
  char_table <- char_table[!duplicated(char_table[[cols[2]]]) & char_table[[cols[2]]] != "", ]
  }

  # here's a test string loaded with different html accents
  # test_str <- '&Agrave; &Aacute; &Acirc; &Atilde; &Auml; &Aring; &AElig; &Ccedil; &Egrave; &Eacute; &Ecirc; &Euml; &Igrave; &Iacute; &Icirc; &Iuml; &ETH; &Ntilde; &Ograve; &Oacute; &Ocirc; &Otilde; &Ouml; &times; &Oslash; &Ugrave; &Uacute; &Ucirc; &Uuml; &Yacute; &THORN; &szlig; &agrave; &aacute; &acirc; &atilde; &auml; &aring; &aelig; &ccedil; &egrave; &eacute; &ecirc; &euml; &igrave; &iacute; &icirc; &iuml; &eth; &ntilde; &ograve; &oacute; &ocirc; &otilde; &ouml; &divide; &oslash; &ugrave; &uacute; &ucirc; &uuml; &yacute; &thorn; &yuml;'

  # use mgsub from here (it's just gsub with a for loop)
  # http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub

  data <- lapply(rlang::set_names(colnames(data)), FUN = function(var) {
    label <- attr(data[[var]], "label")

    if(rlang::is_string(label)) {


      # Replace references with those provided in questions, if any
      if(!is.null(questions) &&
         is.data.frame(questions) &&
         colnames(questions) == c("qid", "questions_var", "question", "force_resp")) {
        reference_id <- stringi::stri_match_all_regex(label,
                                                      pattern = "\\$\\{q://([[:alnum:]]+)/ChoiceGroup/SelectedChoices\\}")[[1]][1,2]
        reference_var <- questions[questions$qid == reference_id, "questions_var"]
        if(rlang::is_string(reference_var) && !is.na(reference_var) &&
           any(reference_var %in% colnames(data))) {
          reference_values <- unique(data[[reference_var]])
          reference_values <- reference_values[!is.na(reference_values)]
          reference_values <- cli::ansi_collapse(reference_values, last = " & ", trunc = 6)
          reference_values <- stringi::stri_c(" (", reference_values, ") ")
          label <- stringi::stri_replace_all_regex(label,
                                                 pattern = "\\$\\{q://[[:alnum:]]+/ChoiceGroup/SelectedChoices\\}",
                                                 replacement = reference_values)
        }
      }

      if(isTRUE(replace_ascii_with_utf)) {
        for(i in seq_len(nrow(char_table))) {
          label <- stringi::stri_replace_all_fixed(str = label,
                                                   pattern = char_table[i, cols[2], drop=TRUE],
                                                   replacement = char_table[i, cols[1], drop=TRUE])
        }
      }

      label <- stringi::stri_replace_all_regex(label, pattern = "- Selected Choice ", replacement = "- ")
      label <- stringi::stri_replace_all_regex(label, pattern = "<.+?>|\\[.*\\]| - tekst", replacement = "")
      label <- stringi::stri_replace_all_regex(label, pattern = "\\$\\{[[:alnum:]]+[^[:alnum:]]([[:alnum:]]+)\\}", replacement = "$1")
      label <- stringi::stri_replace_all_regex(label, pattern = "\\{%name:([[:alnum:]]+) expression:.+?%\\}", replacement = "$1")
      label <- stringi::stri_replace_all_regex(label, pattern = "\\{%expression:.+?%\\}", replacement = "")
      label <- stringi::stri_replace_all_regex(label, pattern = "[[:space:]\n\r\t]+", replacement = " ")
      if(stringi::stri_count_fixed(label, " - ")>=2) label <- stringi::stri_replace_first_fixed(label, pattern = sep, replacement = multi_sep_replacement)
      if(stringi::stri_count_fixed(label, " - ")>=2) label <- stringi::stri_replace_first_fixed(label, pattern = sep, replacement = multi_sep_replacement)
      label <- stringi::stri_replace_all_regex(label, pattern = "^[[:space:]]|[[:space:]-:\\.]+$", replacement = "")
      label <- stringi::stri_replace_all_regex(label, pattern = ":+", replacement = ":")

      attr(data[[var]], "label") <- label
    }
    data[[var]]
  })
  data.frame(data)
}
