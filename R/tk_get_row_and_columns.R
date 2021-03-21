tk_get_row_and_columns <- function(widget, ...) {
  UseMethod("tk_get_row_and_columns", widget)
}

tk_get_row_and_columns.loon <- function(widget, ...) {
  list(
    row = 20,
    column = 20
  )
}

tk_get_row_and_columns.l_pairs <- function(widget, ...) {

  args <- list(...)
  span <- args$span
  histspan <- args$histspan
  histAdjust <- args$histAdjust

  row_column <- extract_num(names(widget))
  column <- max(vapply(row_column, function(x) x[1L], as.numeric(1L)) * span,
                na.rm = TRUE) - span + histspan
  row <- max((vapply(row_column, function(x) x[2L], as.numeric(2L)) + histAdjust) * span,
             na.rm = TRUE) - span + histspan
  list(
    row = row,
    column = column
  )
}

tk_get_row_and_columns.l_facet_wrap <- function(widget, ...) {

  args <- list(...)
  by <- args$by
  title <- args$title

  loc <- l_getLocations(widget)
  nrow <- loc$nrow
  ncol <- loc$ncol

  if(is.atomic(by)) len_by <- 1
  else len_by <- length(by)

  # loon default span
  span <- 10
  column <- ncol * span + 1 # y label
  row <- nrow * (span + len_by) + if(is.null(title)) 0 else 1 + 1 # xlabel and title

  list(
    row = row,
    column = column
  )
}

tk_get_row_and_columns.l_facet_grid <- function(widget, ...) {

  args <- list(...)
  by <- args$by
  title <- args$title

  loc <- l_getLocations(widget)
  nrow <- loc$nrow
  ncol <- loc$ncol
  n <- nrow * ncol

  if(is.atomic(by)) len_by <- 1
  else len_by <- length(by)

  # loon default span
  span <- 10
  column <- ncol * span + 1 + floor(len_by/2) # y label + row subtitles
  row <- nrow * span + (floor(len_by/2) + len_by %% 2) +
    if(is.null(title)) 0 else 1 + 2 # column subtitles xlabel + title

  list(
    row = row,
    column = column
  )
}
