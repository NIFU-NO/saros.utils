#' Remove Reports from Sidebars
#'
#' Finds HTML-files recursively in a path, and removes all entries in the sidebar
#' to certain filenames, such as "0_report.pdf"
#'
#'
#' @param path Folder path as string.
#' @param filename_as_regex Character vector of regex filenames to look for (i.e.,
#' the dot separating file extension must be escaped as \\.).
#'
#' @return List of files processed.
#' @export
#'
#' @examples remove_reports_from_sidebars(path = tempdir())
remove_reports_from_sidebars <- function(path = "_site",
                                         filename_as_regex = c("0_report\\.pdf", "0_report\\.docx")) {
  fs::dir_ls(path = path,
             all = FALSE, recurse = TRUE, type = "file", regexp = "\\.html") |>
    lapply(FUN = function(.x) {
      readLines(.x) |>
        paste0(collapse="\n") |>
        stringi::stri_replace_all_regex(
          pattern = paste0("<li class=\"sidebar-item\">\\s*<div class=\"sidebar-item-container\">\\s*<a href=\"[^\"]*", filename_as_regex, "\"[^>]*>\\s*<span class=\"menu-text\">[^<]*</span></a>\\s*</div>\\s*</li>"),
          replacement = "") |>
        stringi::stri_split_lines() |>
        writeLines(con = .x)
      .x
    }) |>
    unlist()
}
