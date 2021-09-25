get_slicingDistance <- function(slicingDistance = NULL, data, d = ncol(data)) {

  if(is.null(slicingDistance)) {
    halfRange <- max(sqrt(rowSums(tourr::center(data)^2)))
    vRel <- (halfRange^(d - 2))/10
    # a customized (suggested distance)
    slicingDistance <- vRel^(1/(d - 2))
  }

  slicingDistance
}

get_slicingIn <- function(proj, data, slicingDistance) {
  orthogonalDistance <- anchored_orthogonal_distance(proj, data)
  orthogonalDistance[,1] < slicingDistance
}

anchored_orthogonal_distance <- function (plane, data, anchor = NULL) {

  n <- ncol(data)

  if (is.null(anchor)) {
    anchor <- matrix(colMeans(data), ncol = n)
  } else {
    anchor <- matrix(anchor, ncol = n)
  }
  alpha_sq <- sum(anchor^2) - sum((anchor %*% plane)^2)
  full_norm_sq <- apply(data, 1, function(x) sum(x^2))

  subtr <- lapply(seq(ncol(plane)),
                  function(i) {
                    as.matrix(data) %*% plane[, i]
                  })

  dist_sq <- full_norm_sq - Reduce("+", lapply(subtr, function(x) x^2))

  subtr_drop <- lapply(seq(ncol(plane)),
                       function(i) {
                         subtr[[i]] * drop(anchor %*% plane[, i])
                       })

  xterm <- 2 * (as.matrix(data) %*% t(anchor) - Reduce("+", subtr_drop))
  sqrt(dist_sq + alpha_sq - xterm)
}

