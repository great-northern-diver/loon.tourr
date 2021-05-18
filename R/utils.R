`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

scale01 <- function(x) {
  if(is.list(x)) x <- unlist(x)
  x <- c(x)
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)

  if(maxx == minx) return(rep(maxx, length(x)))

  (x - minx)/(maxx - minx)
}

char2null <- function(x, warn = FALSE, message = "") {
  if(length(x) == 0) {
    if(warn) {
      warning(message,
              call. = FALSE)
    }
    return(NULL)
  }
  x
}

remove_null <- function(..., as_list = TRUE) {
  if(as_list)
    Filter(Negate(is.null),
           list(...)
    )
  else
    Filter(Negate(is.null), ...)
}

extract_num <- function(x) {
  vec <- gregexpr("[0-9]+", x)
  lapply(regmatches(x, vec), as.numeric)
}

get_scaledData <- function(data,
                           scaling) {

  data <- as.data.frame(data)
  fun <- loon::l_getScaledData

  if (scaling == "sphere") {
    fun(data = as.data.frame(tourr::sphere_data(data)),
        scaling = "data")
  } else {
    fun(data = data, scaling = scaling)
  }
}

# suppress the `cat` or `print`
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
