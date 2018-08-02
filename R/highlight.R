#' Highlight data frame output in terminal or markdown
#'
#' @param x A data frame.
#' @param ... One or more unquoted expressions separated by commas, or integers
#' regarding the positions.
#' @param styles Style names or functions passed to `crayon::combine_styles()`.
#'
#' @return A `tbl_hl`.
#'
#' @rdname highlight
#' @export
highlight_header <- function(x, styles) {
  not_data_frame(x)
  # default bg = "#ffff88"
  # need to use crayon::make_style() to create a "crayon" object.
  # if print.tbl_hl in terminal, directly use `cat()`.
  # if knit_print.tbl_hl in markdown, translate to hex.
  # rows & cols: integer postions to highlight
  hibble(
    x, 
    header = list(header = TRUE, style = crayon::combine_styles(styles))
  )
}

#' @rdname highlight
#' @export
highlight_rows <- function(x, ..., styles) {
  not_data_frame(x)
  hibble(x, rows = list(rows = 1, style = crayon::combine_styles(styles)))
}

#' @rdname highlight
#' @export
highlight_cols <- function(x, ..., styles) {
  not_data_frame(x)
  hibble(x, cols = list(cols = 1, style = crayon::combine_styles(styles)))
}

#' @rdname highlight
#' @export
highlight_cells <- function(x, ..., styles) {
  not_data_frame(x)
  hibble(
    x, 
    cells = list(rows = 1, cols = 1, style = crayon::combine_styles(styles))
  )
}

#' @rdname highlight
#' @export
highlight_sum <- function(x, styles) {
  not_tibble(x)
  hibble(x, sum = list(sum = TRUE, style = crayon::combine_styles(styles)))
}

#' @rdname highlight
#' @export
highlight_all <- function(x, styles) {
  not_data_frame(x)
  hibble(
    x, 
    header = list(header = TRUE, style = crayon::combine_styles(styles)),
    rows = list(rows = 1, style = crayon::combine_styles(styles)),
    cols = list(cols = 1, style = crayon::combine_styles(styles)),
    cells = list(
      cells = list(rows = 1, cols = 1), 
      style = crayon::combine_styles(styles)
    ),
    sum = list(sum = TRUE, style = crayon::combine_styles(styles))
  )
}

highlight_data <- function(x) {
  if (!is_hibble(x)) {
    stop("Must be `tbl_hl`.")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop(
      "Package `tibble` required for `highlight_data()`", ".\n",
      "Please install and try again.", call. = FALSE
    )
  }
  tibble::as_tibble(attr(x, "highlight"))
}

hibble <- function(x, ...) {
  cls <- c("tbl_hl", class(x))
  structure(x, highlight = list(...), class = cls)
}

is_hibble <- function(x) {
  inherits(x, "tbl_hl")
}
