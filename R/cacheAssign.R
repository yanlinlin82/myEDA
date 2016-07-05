#' Assign with cache mechanism.
#'
#' @param x         variable name
#' @param value     expression to assign to variable
#'
#' @return the same value assigned to the variable
#'
#' @export cacheAssign
#'

cacheAssign <- function(x, value) {

	varname <- as.character(substitute(x))

	if (getOption("cacheAssignVerbose", TRUE)) {
		message("cacheAssign(", varname, ")")
	}

	cacheDirectory <- getOption("cacheDirectory", "cache/")
	ensureDirectory(cacheDirectory)

	filename <- paste0(cacheDirectory, "/", varname, ".rds")
	if (file.exists(filename)) {
		v <- readRDS(filename)
	} else {
		v <- value
		saveRDS(v, filename)
	}
	assign(varname, v, envir = parent.frame())
}
