#' Create QMD-files for the mesos groups in a mesos variable
#'
#' @param dir_path String, path to folder where the reference qmd-files are stored
#' (and where in which the new mesos folders will be created)
#' @param mesos_var String, inserted into qmd-files as `params$mesos_var`
#' @param mesos_groups Character vector of mesos groups.
#'
#' @return Data frame with with names.
#' @export
#'
#' @examples
#' writeLines(c(""), con = fs::path(tempdir(), "index.qmd"))
#' create_mesos_qmd_files(dir_path=tempdir(),
#' mesos_var = "f_uni", mesos_groups = paste0("Uni of ", LETTERS[1:5]))
create_mesos_qmd_files <- function(
    dir_path,
    mesos_var,
    mesos_groups) {

  new_qmd_files <-
    fs::dir_ls(path = dir_path, regexp = "\\.qmd", recurse = FALSE)
  if(length(new_qmd_files)==0) {
    cli::cli_abort("No files found.")
  }
  new_qmd_files <-
    new_qmd_files |>
    stringi::stri_replace_last_fixed(pattern = ".qmd", replacement = "") |>
    basename() |>
    tidyr::expand_grid(main_file = _,  mesos_group = mesos_groups) |>
    dplyr::mutate(main_file_no_ = stringi::stri_replace_first_regex(.data$main_file, pattern="^_", replacement = ""),
                  new_file_path = fs::path(.env$dir_path, .data$mesos_group, paste0(.data$main_file_no_, ".qmd")),
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(contents = {
      yaml <- list(params = list(mesos_var = .env$mesos_var,
                                 mesos_group = .data$mesos_group))
      if(.data$main_file_no_ %in% c("index", "0_report")) yaml$title <- paste0(.data$mesos_group)
      yaml <- yaml::as.yaml(x = yaml)
      paste0("---\n", yaml,  "---\n",
             paste0("\n{{< include ../", .data$main_file, ".qmd >}}\n"),
             sep="\n")
    })

  fs::dir_create(path = dirname(new_qmd_files$new_file_path))

  for(i in seq_len(nrow(new_qmd_files))) {
    cat(new_qmd_files[i, "contents", drop=TRUE],
        file = new_qmd_files[i, "new_file_path", drop=TRUE])
  }
  new_qmd_files
}
