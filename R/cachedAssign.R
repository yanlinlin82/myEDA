#' Assign a Value to a Name with Cache Mechanism.
#'
#' @description Assign a value to a name in an environment with cache mechanism. 
#'
#' @param x         a variable name. Either a name or character string will work.
#' @param value     a value to be assigned to ‘x’.
#'
#' @details When it is the first time to assign some variable with 'cachedAssign()',
#' expression of 'value' will be evaluated, saved to cache file, and then assigned
#' to variable 'x'. After that, as the cache file exists, evaluation of 'value'
#' will be skipped, and value stored in cache file will be loaded and assigned to
#' 'x' instead.
#'
#' Cache files are stored in cache directory, as the same name as the varaibles,
#' with extension name '.rds'. By default, the cache directory is set to 'cached/',
#' that means variable 'foo' will be stored as cache file 'cached/foo.rds'. The
#' cache directory can be configured by 'option("cache.directory")'
#'
#' 'loadCache()' just loads value from existed cache file, and it will fail when
#' cache file does not exist.
#'
#' @return like 'assign()' in 'base' package, this function is invoked for its
#' side effect, which is assigning 'value' to the variable 'x'.
#'
#' @export
cachedAssign <- function(x, value) {

	varname <- as.character(substitute(x))

	if (getOption("cache.verbose", TRUE)) {
		message("cachedAssign(", varname, ")")
	}
	cacheDirectory <- getOption("cache.directory", "cache/")
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

#' @rdname cachedAssign
#' @export
loadCache <- function(x) {

    varname <- as.character(substitute(x))

	if (getOption("cache.verbose", TRUE)) {
		message("loadCache(", varname, ")")
	}
	cacheDirectory <- getOption("cache.directory", "cache/")

    assign(varname, readRDS(paste0(cacheDirectory, "/", varname, ".rds")), envir = parent.frame())
}
