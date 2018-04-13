
#' Pivot a table
#'
#' @inheritParams tidyr::spread
#' @param ... Selection criteria for levels of key to select.
#'
#' @export
pivot <- function(data, key, value, ..., fill=NULL)UseMethod("pivot")

#' @export
#' @importFrom dbplyr add_op_single op_single
pivot.tbl_lazy <-
function( data, key, value, ..., fill=NULL
        , info = getOption("pivot::info", FALSE)
        ){
    key   <- rlang::enquo(key)
    value <- rlang::enquo(value)
    dots  <- rlang::quos(...)
    if (!is.null(fill) && is.na(fill)) fill <- NULL
    args  <- list( key=key, value=value, fill=fill)
    if (identical(dots, rlang::quos())){
        if (info) message("No levels specified, defaulting to everything().")
        dots <- rlang::quos(tidyselect::everything())
        args$levels <- get_pivot_levels(data, !!key, !!!dots)
    } else
    if (any(. <- purrr::map_lgl(dots, rlang::quo_is_call))){
        args$levels <- get_pivot_levels(data, !!key, !!!dots)
        if (info) message( "calls ", paste(purrr::map_chr(dots[.], rlang::quo_text)), collapse=', '
                         , " were found in level specification, and evaluated to ("
                         , paste(args$levels, collapse=", "), ')'
                         )
    }
    add_op_single('pivot', data, dots, args)
}

find_connection <- function(x)UseMethod('find_connection')

find_connection.op <- function(x){
    assert_that(inherits(op, 'op'))
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

    dbplyr::select_query( from     = sql_build(data, con)
                , select   = dbplyr::ident(dplyr::select_var(dplyr::tbl_vars(data), !!key))
                , order_by = dbplyr::ident(dplyr::select_var(dplyr::tbl_vars(data), !!key))
                , distinct = TRUE
                ) %>%
        dbplyr::sql_render(con=con) %>%
        dbplyr::db_collect(con=con) %>%
        rlang::eval_tidy(expr=key) %>%
        as.character() %>%
        dplyr::select_vars(!!!dots)
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

all_names <- function (x) {
    if (is.name(x))  return(as.character(x))
    if (!is.call(x)) return(NULL)
    unique(unlist(lapply(x[-1], all_names), use.names = FALSE))
}

#' @export
#' @importFrom dbplyr sql_build
sql_build.op_pivot <- function(op, con, ...){
    key       <- dplyr::select_var(op_vars(op$x), !!op$args$key)
    value     <- dbplyr::partial_eval(op$args$value, vars=op_vars(op$x))
    used_vars <- all_names(rlang::get_expr(value))
    levels    <- if (!is.null(op$args$levels)) op$args$levels
                    else levels_op_pivot(op, con=con)
    select <- if (length(op_grps(op$x)) == 0)
            dplyr::select_vars(op_vars(op$x), exclude=c(key, used_vars))
        else
            dplyr::select_vars(op_vars(op$x), include=op_grps(op$x))

    assert_that(!any(used_vars %in% select))

    pivot_query( from   = dbplyr::sql_build(op$x, con=con)
               , select = ident(select)
               , key    = ident(key)
               , value  = value
               , levels = ident(levels)
               , fill   = op$args$fill
               )
}

#' @export
#' @importFrom dbplyr op_vars
op_vars.op_pivot <- function(op)c( op_grps(op), unname(levels_op_pivot(op)))

#' @export
#' @importFrom dbplyr op_grps
op_grps.op_pivot <- function(op)op_grps(op$x)

#' Create a pivot query representation
#'
#' Create a pivot table
#'
#' @param from     the from clause
#' @param select   variables to select in addition to levels.
#' @param key      Variable columns originate from
#' @param value    The expresion to evaluate to create the values
#' @param levels   the levels of key to turn into columns.
#' @param order_by optional order by clause
#' @param fill     optional value to fill in structural missing values.
#'                 It is the responsibility of the user to ensure type
#'                 compatability.
#' @export
pivot_query <-
function( from
        , key
        , value
        , levels
        , select = character(0)
        , order_by = NULL
        , fill = NULL
        ){
    assert_that( rlang::is_quosure(value)
               , dbplyr::is.ident(key)
               , dbplyr::is.ident(levels)
               , dbplyr::is.ident(select) || is.null(select)
               , dbplyr::is.ident(order_by) || is.null(order_by)
               )
    structure( list( from   = from
                   , key    = key
                   , value  = value
                   , levels = levels
                   , select = select
                   , fill = fill
                   )
             , class = c('pivot_query', 'query')
             )
}

#' @export
#' @importFrom dbplyr sql_render ident is.ident build_sql
sql_render.pivot_query <-
function( query, con=query$con, ..., root=FALSE){
    assert_that(inherits(query, 'pivot_query'))
    if (!is.ident(query$from))
        query$from <- dplyr::sql_subquery(con, sql_render(query$from, con, ..., root=root))
    sql_pivot( con = con
             , from   = query$from
             , select = query$select
             , key    = query$key
             , value  = query$value
             , levels = query$levels
             , fill = query$fill
             )
}

#' Create a Pivot Query
#'
#' Creates a SQL pivot query.  Similar to the \code{tidyr::spread}
#' function.
#'
#' @inheritParams pivot_query
#' @param con    a Database connection
#' @param ...   arguments to pass on or ignore.
#'
#' @importFrom dbplyr escape
#' @export
sql_pivot <- function(con, from, select, key, value, levels, ...)
                    UseMethod('sql_pivot')

sql_pivot_MSSQLServer <-
function(con, from, select, key, value, levels, order_by=NULL, fill=NULL, ...){
    assert_that( inherits(con, "Microsoft SQL Server")
               , dbplyr::is.ident(select)
               , dbplyr::is.ident(key)
               , dbplyr::is.ident(levels)
               ,  rlang::is_quosure(value)
               )
    nl     <- DBI::SQL('\n')
    indent <- DBI::SQL('\t')
    pvt.name <- ident(gsub('[^A-Za-z0-9]', '_', rlang::quo_text(value)))

    value.text <- if (is.name(rlang::quo_get_expr(value))){
        warning( "Microsoft SQL Server Pivot Query requires an aggregate function.  "
               , "Changing value to 'max(", rlang::quo_text(value), ")'"
               )
        dbplyr::translate_sql(max(!!value, na.rm=TRUE), con=con, window=FALSE)
    } else {
        dbplyr::translate_sql(!!value, con=con, window=FALSE)
    }

    levels.text <- if(is.null(fill)) escape(levels, collapse=NULL) else {
        fill <- escape(fill, con=con)

        dbplyr::sql(rlang::set_names(nm=ifelse(rlang::have_name(levels), names(levels), unclass(levels)),
            purrr::map_chr( escape(levels, collapse=NULL)
                          , ~build_sql('ISNULL(', dbplyr::sql(.), ',', fill, ')')
                          ) %>%
                rlang::set_names(levels)
        ))
    }


    build_sql( con=con
             , "SELECT "
             , escape(c( escape(select, collapse=NULL, con = con)
                       , levels.text
                       ), collapse = ", ", con=con)
             , nl, "FROM " , from
             , nl, "PIVOT ("
             , build_sql( con=con
                        , nl, indent, value.text
                        , nl, indent, 'FOR '  , unname(key), " IN "
                        , escape(unname(levels), parens=TRUE, collapse = ", ", con = con)
                        )
             , nl, ') AS ', pvt.name
             , if (!is.null(order_by)) {
                    assert_that(is.character(order_by), length(order_by) > 0)
                    build_sql(con=con, "\nORDER BY ", escape(unname(order_by), parens=FALSE, collapse=", ", con=con))
                }
             )
}

#' @export
`sql_pivot.Microsoft SQL Server` <- function(...)sql_pivot_MSSQLServer(...)
