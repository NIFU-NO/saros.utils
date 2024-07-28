#' Recode Missing By Type of Missingness
#'
#' Useful for item difficulty estimation according to Mislevy's recommendation.
#'     Also allowing for escaping rows with all missingess (typically
#'     not administered).
#'
#' @param df Data frame, or vector. Must be a dataframe, not a matrix, in this
#'   function. Only include item variables.
#' @param accept_vector Handles vectors if accept_vector=TRUE. Set to false to
#'   avoid accidents when using function per block and there is just one item in
#'   the block.
#' @param skipped What to replace skipped values with
#' @param not_administered What to replace not administered values with.
#' @param all_missing What to replace values in rows with all missing with.
#' @importFrom rlang set_names warn abort
#' @return A data.frame (or vector, if input is vector and accept_vector=TRUE)
#'   with recoded cells.
#' @export
#'
#' @examples
#' # Original data
#' input <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	NA,0,1,0,1, # First missing
#' 	NA,NA,1,0,1, # First two missing
#' 	1,0,NA,0,1, # One in middle missing
#' 	1,NA,NA,NA,1, # All in the middle missing
#' 	1,0,1,0,NA, # Last one missing
#' 	1,0,1,NA,NA, # Last two missing
#' 	1,0,NA,NA,NA, # Last three missing
#' 	NA,NA,NA,NA,NA # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#' # What should be the output for item estimation according to Mislevy
#' # Skipped=> 0, not_administered=>NA, all_missing=>NA
#' y_i <-  stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	0,0,1,0,1, # First missing
#' 	0,0,1,0,1, # First two missing
#' 	1,0,0,0,1, # One in middle missing
#' 	1,0,0,0,1, # All in the middle missing
#' 	1,0,1,0,0, # Last one missing
#' 	1,0,1,0,NA, # Last two missing
#' 	1,0,0,NA,NA, # Last three missing
#' 	NA,NA,NA,NA,NA # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#'
#' # What should be the output for person estimation according to Mislevy
#' # Skipped=> 0, not_administered=>NA, all_missing=>NA
#' y_p <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	0,0,1,0,1, # First missing
#' 	0,0,1,0,1, # First two missing
#' 	1,0,0,0,1, # One in middle missing
#' 	1,0,0,0,1, # All in the middle missing
#' 	1,0,1,0,0, # Last one missing
#' 	1,0,1,0,0, # Last two missing
#' 	1,0,0,0,0, # Last three missing
#' 	0,0,0,0,0 # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#' # Recoding for counting skipped, not_administered, all_missing, etc
#' # Skipped=> 99, not_administered=>999, all_missing=>9999
#' y_info <- stats::setNames(as.data.frame(matrix(c(
#' 	1,0,1,0,1, # All present
#' 	99,0,1,0,1, # First missing
#' 	99,99,1,0,1, # First two missing
#' 	1,0,99,0,1, # One in middle missing
#' 	1,99,99,99,1, # All in the middle missing
#' 	1,0,1,0,99, # Last one missing
#' 	1,0,1,99,999, # Last two missing
#' 	1,0,99,999,999, # Last three missing
#' 	9999,9999,9999,9999,9999 # All missing
#' ), nrow = 9, byrow = TRUE)), nm=stringi::stri_c(ignore_null=TRUE, "X", 1:5))
#'
#' y_i2 <- omitted_recoder_df(input) #Mislevy item estimation
#' y_p2 <- omitted_recoder_df(input, skipped = 0L, #Mislevy person estimation
#'                            not_administered = 0L, all_missing = 0L)
#' y_info2 <- omitted_recoder_df(input, skipped = 99,
#'                               not_administered = 999, all_missing = 9999)
#' identical(y_i, y_i2)
#' identical(y_p, y_p2)
#' identical(y_info, y_info2)
#' \dontrun{
#' omitted_recoder_df(input[,4]) # Should fail
#' }
#' identical(omitted_recoder_df(input[,4], accept_vector=TRUE),
#'          c(0,0,0,0,0,0,0,NA,NA))
#' identical(omitted_recoder_df(input[,4, drop=FALSE]),
#'           input[,4, drop=FALSE]) # Output should equal input

omitted_recoder_df <- function(df, accept_vector=FALSE, skipped=0L,
							   not_administered=NA_integer_,
							   all_missing=NA_integer_) {
	omittedRecoderVec <- function(vec) {
		vec_new <- vec
		N <- length(vec)
		if(all(is.na(vec))) {
			vec_new <- rep(all_missing, times=N)
		} else {
			for(i in N:1L) { # Going backwards on all a person's responses,

				if(is.na(vec[i])) { # if the response is blank AND either
					if((any(!is.na(vec[min(c(i+1L, N)):N]))) ||  #  1) any responses after this to the end are present OR  #i==1L && it is the first response AND
					   (i!=1L && !is.na(vec[i-1L]) && all(is.na(vec[min(c(i+1L, N)):N]))) ## or the prior response is PRESENT (if not first response)
					) {
						vec_new[i] <- skipped   # Then set this response as 'skipped'
					} else { # OR if the response is blank AND
						if((i == 1L || is.na(vec[i-1L])) && # 1) it is the first response or the prior response is MISSING AND
						   all(is.na(vec[i:N]))) { # 2) All responses from this and to the end are all blank
							vec_new[i] <- not_administered # Recode as not administered.
						}
					}
				}
			}
		}
		vec_new
	}
	if(is.atomic(df)) {
		if(!accept_vector) {
			rlang::abort("Vectors not accepted.")
		} else if(is.atomic(df)) omittedRecoderVec(df)
	} else {
		if(ncol(df)==1) {
			rlang::warn("Unable to recode single-column data.frame without knowing context.")
			df
		} else {
		  stats::setNames(as.data.frame(t(apply(df, 1, omittedRecoderVec))),
							 nm=colnames(df))
		}
	}
}









#' Create All Possible Combinations of Vector Elements with Minimum A and
#' Maximum B.
#'
#' @param vec Vector
#' @param n_min Minimum number of elements
#' @param n_max Maximum number of elements. Defaults to length of vec.
#'
#' @importFrom utils combn
#' @importFrom rlang is_integer
#' @return A data frame
#' @export
#' @examples
#' combn_upto()
combn_upto <-
  function(vec=c("a", "b", "c", "d", "e", "f", "g"),
           n_min=6L,
           n_max=length(vec)) {
	stopifnot(rlang::is_integer(as.integer(n_min)))
	stopifnot(n_max<=length(vec))
	x <-
	  unlist(lapply(n_min:n_max, function(x) utils::combn(x = vec, m = x, simplify = F)), recursive = FALSE)
	x <- stats::setNames(x, x)
	rev(x)
}




trim_columns <- function(data, cols = c(".variable_label_prefix_dep", ".variable_label_prefix_dep",
                                        ".variable_label_prefix_indep", ".variable_label_suffix_indep")) {
  for(col in cols) {
    if(is.character(data[[col]])) {
      data[[col]] <- stringi::stri_trim_both(data[[col]])
      data[[col]] <- stringi::stri_replace_all_regex(data[[col]], pattern = "[[:space:]]+", replacement = " ")
    }
  }
  data
}


#' Mutate a (factor, character, integer, etc) column into multiple columns,
#'
#' @description Easily mutate a single column into multiple columns (~dummies+1),
#' while retaining variable labels and order of the original factor variable.
#'
#' @param data Data frame or tibble.
#' @param col Single column. Tidy-select.
#' @param var_separator *Variable separator*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Separator between old variable name and categories.
#'
#' @param label_separator
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Separator between old label name and new label part.
#'
#' @return Original data frame with the binary columns attached, containing new labels.
#' @export
#'
col_to_binaries <- function(data, col,
                            var_separator = "___",
                            label_separator = " - ") {
  if(length(dplyr::select(data, {{col}}))>1L) {
    cli::cli_abort(c("Only 1 column is currently allowed, for your protection.",
                     i="You have provided {length(dplyr::select(data, {{col}})} columns."))
  }
  col_enq <- rlang::enquo(arg = col)
  col_nm <- rlang::as_name(x = col_enq)
  col_pos <- tidyselect::eval_select(expr = col_enq, data = data)
  col_label <- attr(data[[col_pos]], "label")

  if(is.factor(data[, col_pos, drop=TRUE]) | is.ordered(data[, col_pos, drop=TRUE]) |
     is.integer(data[, col_pos, drop=TRUE]) | is.numeric(data[, col_pos, drop=TRUE])) {
    data2 <-
      data |>
      dplyr::arrange(as.numeric({{col}}))
  } else {
    data2 <-
      data |>
      dplyr::arrange({{col}})
  }
  data3 <-
    data2 |>
    dplyr::select({{col}}) |>
    dplyr::mutate(`_dummy` = 1L,
                  `_id` = seq_len(nrow(data))) |>
    tidyr::pivot_wider(names_from = {{col}},
                       values_from = tidyselect::all_of("_dummy"),
                       values_fill = 0L,
                       names_glue = stringi::stri_c(ignore_null=TRUE, col_nm, var_separator, "{.name}")) |> #
    dplyr::select(!tidyselect::all_of("_id"))


  new_labels <-
    stringi::stri_c(ignore_null=TRUE, col_label, label_separator, unique(data2[[col_pos]]))

  for(i in seq_len(ncol(data3))) {
    attr(data3[[i]], "label") <- new_labels[i]
  }
  dplyr::bind_cols(data2, data3)
  }


