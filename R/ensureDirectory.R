#' Ensure that directory exists.
#'
#' @param path    directory path to check
#'
#' @return no any return value. stop if any error.
#'
#' @export ensureDirectory
#'

ensureDirectory <- function(dir) {
	if (!dir.exists(dir)) {
		stopifnot(dir.create(dir, recursive = TRUE))
	}
	invisible(NULL)
}
