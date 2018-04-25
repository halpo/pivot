
#' Pivot a table
#'
#' @inheritParams tidyr::spread
#' @param ... Selection criteria for levels of key to select.
#' @examples
#' library(dplyr)
#' library(dbplyr)
#' # establish db as a database connection
#' \dontshow{
#'    con <- simulate_mssql()
#'    src <- src_dbi(con)
#'    base <- list( x = ident('##iris')
#'                , vars  = tbl_vars(iris)
#'                ) %>% structure(class=c('op_base_remote', 'op_base', 'op'))
#'    db_iris <- structure( list( src = src
#'                              , ops = base
#'                              )
#'        , class = c('tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl'))
#' }
#' \dontrun{
#' db_iris <- copy_to(db, iris)
#' }
#' result <- pivot( db_iris, Species, mean(Petal.Length, na.rm=TRUE)
#'                , setosa, versicolor, virginica)
#' sql_render(result)
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
        # nocov start
        if (info) message("No levels specified, defaulting to everything().")
        dots <- rlang::quos(tidyselect::everything())
        args$levels <- get_pivot_levels(data, !!key, !!!dots)
        # nocov end
    } else
    if (any(. <- purrr::map_lgl(dots, rlang::quo_is_call))){
        # nocov start
        args$levels <- get_pivot_levels(data, !!key, !!!dots)
        if (info) message( "calls ", paste(purrr::map_chr(dots[.], rlang::quo_text)), collapse=', '
                         , " were found in level specification, and evaluated to ("
                         , paste(args$levels, collapse=", "), ')'
                         )
        # nocov end
    }
    add_op_single('pivot', data, dots, args)
}

#' @export
#' @importFrom rlang :=
pivot.data.frame <- 
function( data, key, value, ...
        , fill =NULL
        , info = getOption("pivot::info", FALSE)
        ){
    key   <- rlang::enquo(key)
    value <- rlang::enquo(value)
    dots  <- rlang::quos(...)
    if (is.null(fill)) fill <- NA
    if (identical(dots, rlang::quos())) {
        if (info) message("No levels specified, defaulting to everything().")
        dots <- rlang::quos(tidyselect::everything())
    }
    used <- all_names(value)
    
    if (rlang::quo_is_call(value)) {
        remaining <- setdiff(dplyr::tbl_vars(data), used)
        levels  <- tidyselect::vars_select(as.character(unique(dplyr::pull(data, !!key))), !!!dots)
        dplyr::summarise( dplyr::group_by(data, !!key, add=TRUE)
                        , !!rlang::quo_text(value) := !!value
                        ) %>%
        dplyr::ungroup() %>%
        spread( key   = !!key
              , value = !!rlang::quo_text(value)
              , fill =fill
              , convert=FALSE, drop=TRUE, sep=NULL) %>%
        dplyr::group_by(!!!dplyr::groups(data))
    } else {
        data %>% dplyr::select(!!key, !!!dplyr::groups(data), !!!used) %>% 
        tidyr::spread( key=!!key, value=!!value, fill=fill
                     , convert=FALSE, drop=TRUE, sep=NULL) %>% 
        dplyr::select(!!!dplyr::group_vars(data), !!!dots)
    }
}
if (FALSE) {#@testing
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
            tidyselect::vars_select(op_vars(op$x), .exclude=c(key, used_vars))
        else
            tidyselect::vars_select(op_vars(op$x), .include=op_grps(op$x))

    assert_that(!any(used_vars %in% select))
    
    needed.vars <- unname(c(key, select, used_vars))
    
    from <- dbplyr::sql_build(op$x, con=con)
    if (!all(dbplyr::op_vars(op$x) %in% needed.vars))
        from <- dbplyr::select_query(from, needed.vars)

    pivot_query( from   = from
               , select = ident(select)
               , key    = ident(key)
               , value  = value
               , levels = ident(levels)
               , fill   = op$args$fill
               )
}

#' @export
#' @importFrom dbplyr op_vars
op_vars.op_pivot <- function(op){
    names <- base::names(levels_op_pivot(op))
    names <- dplyr::coalesce(dplyr::na_if(names, ''), levels_op_pivot(op))
    c( op_grps(op), names)
}

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
#' @param value    The expression to evaluate to create the values
#' @param levels   the levels of key to turn into columns.
#' @param order_by optional order by clause
#' @param fill     optional value to fill in structural missing values.
#'                 It is the responsibility of the user to ensure type
#'                 compatibility.
#' @examples
#' library(dplyr)
#' library(dbplyr)
#' con <- simulate_mssql()
#' 
#' query <- pivot_query( ident('##iris'), key = ident('Species')
#'                     , levels = ident(c('setosa', 'virginica', 'versicolor'))
#'                     , value  = rlang::quo(mean(Petal.Length, na.rm=TRUE))
#'                     )
#' sql_render(query, con=con)
#' @export
pivot_query <-
function( from
        , key
        , value
        , levels
        , select = ident()
        , order_by = NULL
        , fill = NULL
        ){
    assert_that( rlang::is_quosure(value)
               , dbplyr::is.ident(key)
               , dbplyr::is.ident(levels)
               , dbplyr::is.ident(select) || (length(select)==0)
               , dbplyr::is.ident(order_by) || is.null(order_by)
               )
    structure( list( from   = from
                   , key    = key
                   , value  = value
                   , levels = levels
                   , select = select
                   , fill = fill
                   , order_by = order_by
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
#' @examples
#' library(dbplyr)
#' query <- sql_pivot( dbplyr::simulate_mssql()
#'                   , from   = ident('##iris')
#'                   , select = ident()
#'                   , key    = ident('Species')
#'                   , value  = rlang::quo(mean(Petal.Length, na.rm=TRUE))
#'                   , levels = ident(c('versicolor', 'virginica'))
#'                   )
#' sql_render(query)
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
             , '\n', "FROM " , from
             , '\n', "PIVOT ("
             , build_sql( con=con
                        , '\n    ', value.text
                        , '\n    ', 'FOR '  , unname(key), " IN "
                        , escape(unname(levels), parens=TRUE, collapse = ", ", con = con)
                        )
             , '\n', ') AS ', pvt.name
             , if (!is.null(order_by)) {
                    assert_that(is.character(order_by), length(order_by) > 0)
                    build_sql(con=con, "\nORDER BY ", escape(unname(order_by), parens=FALSE, collapse=", ", con=con))
                }
             )
}

#' @export
`sql_pivot.Microsoft SQL Server` <- function(...)sql_pivot_MSSQLServer(...)

#' @export
#' @importFrom dplyr sql_escape_ident
`sql_escape_ident.Microsoft SQL Server` <- function(con, x) dbplyr::sql_quote(x, "\"")
