#' Manipulate directory tree.
#'
#' @param dir    directory path to check
#'
#' @return no any return value. stop if any error.
#'
#' @export DirTree
#'
#' @examples
#'
#'   dirTree <- DirTree$new()                # create 'plots/'
#'
#'   dirTree$enter("foo")                    # create 'plots/01_foo/'
#'     png(dirTree$get("rnorm10"))           # got    'plots/01_foo/01_rnorm10.png'
#'     plot(rnorm(10))
#'     dev.off()
#'   dirTree$leave()
#'
#'   dirTree$enter("bar")                    # create 'plots/02_bar/'
#'     dirTree$enter("sub1")                 # create 'plots/02_bar/01_sub1/'
#'       jpeg(dirTree$get("sin", ".jpg"))    # got    'plots/02_bar/01_sub1/01_sin.jpg'
#'       plot(sin, 0, 2 * pi)
#'       dev.off()
#'     dirTree$leave()
#'     dirTree$enter("sub2")                 # create 'plots/02_bar/02_sub2/'
#'       f <- dirTree$get("mtcars", ".csv")  # got    'plots/02_bar/02_sub2/01_mtcars.csv'
#'       write.csv(mtcars, f)
#'     dirTree$leave()
#'   dirTree$leave()
#'
#'   dirTree$exit()                          # safely exit

DirTree <- R6Class("DirTree",

    public = list(
        initialize = function(dir = "plots") { private$startPlotDir(dir) },
        enter = function(name = "") { private$enterPlotDir(name) },
        get = function(name = "", extname = ".png") { private$getPlotFilename(name, extname) },
        leave = function() { private$leavePlotDir() },
        exit = function() { private$endPlotDir() }
    ),

    private = list(
        stack = data.frame(dir = character(), id = integer()),

        startPlotDir = function(dir) {
            stopifnot(nrow(private$stack) == 0)
            stopifnot(length(dir) == 1)
            if (dir == "") {
                dir <- "."
            } else if (dir != "/" && substr(dir, nchar(dir), nchar(dir)) == "/") {
                dir <- substr(dir, 1, nchar(dir) - 1)
            }
            ensureDirectory(dir)
            private$stack <- data.frame(dir = dir, id = 0)
            message("Plotting (in '", dir, "') ...")
            return(invisible(NULL))
        },

        enterPlotDir = function(name = "") {
            stopifnot(nrow(private$stack) > 0)
            n <- nrow(private$stack)
            private$stack$id[[n]] <- private$stack$id[[n]] + 1
            dir <- paste0(sprintf("%02d", private$stack$id[[n]]), ifelse(name == "", "", paste0("_", name)))
            dir <- gsub("/*$", "", dir)
            private$stack <- rbind(private$stack, data.frame(dir = dir, id = 0))
            ensureDirectory(paste(private$stack$dir, collapse = "/"))
            return(invisible(NULL))
        },

        leavePlotDir = function() {
            stopifnot(nrow(private$stack) > 1)
            n <- nrow(private$stack)
            private$stack <- private$stack[-n, ]
            return(invisible(NULL))
        },

        getPlotFilename = function(name = "", extname = ".png") {
            stopifnot(nrow(private$stack) > 0)
            n <- nrow(private$stack)
            private$stack$id[[n]] <- private$stack$id[[n]] + 1
            return(paste0(paste0(private$stack$dir, collapse = "/"),
                          "/",
                          sprintf("%02d", private$stack$id[[n]]),
                          ifelse(name == "", "", paste0("_", name)),
                          extname))
        },

        endPlotDir = function() {
            stopifnot(nrow(private$stack) == 1)
            private$stack <- data.frame(dir = character(), id = integer())
            message("Plotting done.")
            return(invisible(NULL))
        }
    )
)
