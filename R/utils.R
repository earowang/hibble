not_data_frame <- function(x) {
  if (!is.data.frame(x)) {
    stop(
      sprintf("Must be a `data.frame`-like object, not class `%s`.", class(x)),
      call. = FALSE)
  }
}

not_tibble <- function(x) {
  if (!is_tibble(x)) {
    stop(
      sprintf("Can only handle `tbl_df`, not class `%s`.", class(x)),
      call. = FALSE)
  }
}
