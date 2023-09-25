#' trim vector function
#'
#' Trims vectors and calculates mean.
#'
#' @param inputVec,s,l A vector, and two integers.
#' @return mean.
#' @export

trimmed_mean <- function(inputVec, s, l) {
  if (length(inputVec) < s + l + 1) {
    stop("Not enough values to remove.")
  }
  sorted_data <- sort(inputVec)
  trimmed_data <- sorted_data[(s + 1):(length(sorted_data) - l)]
  return(mean(trimmed_data))
}
