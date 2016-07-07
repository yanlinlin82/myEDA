#' Browse Color Names for Choosing
#'
#' @param ncol   number of columns to show on each page
#' @param nrow   number of rows to show on each page
#'
#' @examples browseColors()
#'
#' @export

browseColors <- function(ncol = 5, nrow = 20) {

    a <- data_frame(name = colors()) %>%
        mutate(value = 1:n(),
               row = (value - 1) %% nrow + 1,
               col = (value - 1) %/% nrow %% ncol + 1,
               page = (value - 1) %/% (nrow * ncol) + 1)

    par(ask = TRUE)

    cm <- a$name
    names(cm) <- a$name
    for (i in unique(a$page)) {
        g <- a %>%
            filter(page == i) %>%
            ggplot(aes(x = col, y = row, fill = name)) +
                geom_tile(color = "black", size = .5) +
                guides(fill = FALSE) +
                scale_fill_manual(values = cm) +
                geom_text(aes(label = name))
        print(g)
    }
    invisible(NULL)
}
