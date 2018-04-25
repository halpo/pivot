find_connection <- function(x)UseMethod('find_connection')

find_connection.op <- function(x){
    assert_that(inherits(x, 'op'))
    op <- x
    x <- op$x
    while (!is.null(x)) {
        if (inherits(x, 'tbl_lazy')) {
            return(x$src$con)
        } else
        if (inherits(x, 'op')) {
            x <- x$x
        } else {
            stop( "Could not find base table to infer con from.  "
                , "Final op$x...$x value was "
                , paste(class(x), collapse="/")
                )
        }
    }
    stop("Could not find a valid connection.")
}
find_connection.tbl_lazy<- function(x)x$src$con

#' @importFrom magrittr %>%
get_pivot_levels <- function(data, key, ..., con=find_connection(data)){
    key  <- rlang::enquo(key)
    dots <- rlang::quos(...)

    data %>% dplyr::ungroup() %>% dplyr::select(!!key) %>% 
        dplyr::distinct() %>% 
        dplyr::pull(!!key) %>% 
        as.character() %>%
        tidyselect::vars_select(!!!dots)
}

#' @export
levels.op_pilot <- function(x){
    assert_that(inherits(x, "op_pivot"))
    con <- find_connection(x)
    assert_that(inherits(con, "DBIConnection"))
    levels_op_pivot(op=x, con=con)
}
levels_op_pivot <-
function( op
        , con = find_connection(op)
        ){
    assert_that( inherits(op , 'op_pivot') )
    if (!is.null(op$args$levels))
        return(op$args$levels)
    purrr::map_lgl(op$dots, rlang::is_syntactic_literal)
    if (!any(purrr::map_lgl(op$dots, rlang::quo_is_call)))
        return (purrr::map_chr(op$dots, rlang::quo_text))
    get_pivot_levels(op$x, !!op$args$key, !!!op$dots, con=con)
}
