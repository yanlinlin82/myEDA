#' Assign a Value to a Name with Cache Mechanism.
#'
#' @description Assign a value to a name in an environment with cache mechanism. 
#'
#' @param x         a variable name, given as a character string.
#' @param value     a value to be assigned to ‘x’.
#' @param force     force to overwrite or load from the cache file.
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
#' 'cacheExists()' is for checking if a variable has been cached.
#'
#' @return like 'assign()' in 'base' package, this function is invoked for its
#' side effect, which is assigning 'value' to the variable 'x'.
#'
#' @export
cachedAssign <- function(x, value, force = FALSE) {

    stopifnot(is.character(x))
    stopifnot(length(x) == 1)

	if (getOption("cache.verbose", TRUE)) {
		message("cachedAssign(", x, ")")
	}
	cacheDirectory <- getOption("cache.directory", "cache/")
	ensureDirectory(cacheDirectory)

	filename <- paste0(cacheDirectory, "/", x, ".rds")
	if (!force && file.exists(filename)) {
		v <- readRDS(filename)
	} else {
		v <- value
		saveRDS(v, filename)
	}
	assign(x, v, envir = parent.frame())
}

#' @rdname cachedAssign
#' @export
loadCache <- function(x, force = FALSE) {

    stopifnot(is.character(x))
    stopifnot(length(x) == 1)

    if (force || !exists(x, envir = parent.frame())) {

        if (getOption("cache.verbose", TRUE)) {
            message("loadCache(", x, ")")
        }
        cacheDirectory <- getOption("cache.directory", "cache/")

        assign(x, readRDS(paste0(cacheDirectory, "/", x, ".rds")), envir = parent.frame())
    } else {
        x
    }
}

#' @rdname cachedAssign
#' @export
cacheExists <- function(x) {

    stopifnot(is.character(x))
    stopifnot(length(x) == 1)

    cacheDirectory <- getOption("cache.directory", "cache/")

    file.exists(paste0(cacheDirectory, "/", x, ".rds"))
}
