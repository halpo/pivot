#' @import assertthat
NULL

globalVariables('.')

#' Pivot a table
#'
#' @inheritParams dplyr::select
#' @inheritParams tidyr::spread
#' @param ... Selection criteria for levels of key to select.
#'
#' @examples
#' \dontrun{
#' # establish db as a database connection
#' library(dbplyr)
#' library(tidyverse)
#' db_iris <- copy_to(db, iris)
#' long.iris <- unpivot(db_iris, Variable, Value, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' means <- summarise(group_by(long.iris, Species, Variable), Mean = mean(Value, na.rm=TRUE)
#' pivot(means, Species, setosa, versicolor, virginica)
#' }
#'
#' @export
unpivot <- function(data, key, value, ...) UseMethod("unpivot")

#' @export
#' @importFrom dbplyr add_op_single op_single
unpivot.tbl_lazy <-
    function(data, key, value, ...){
        key   <- rlang::enquo(key)
        value <- rlang::enquo(value)
        dots  <- rlang::quos(...)
        args  <- list( key=key, value=value)
        add_op_single('unpivot', data, dots=dots, args = args)
    }

#' @export
#' @importFrom dbplyr sql_build
sql_build.op_unpivot <- function(op, con, ...){
    levels   <- dplyr::select_vars( op_vars(op$x), !!!(op$dots), exclude=op_grps(op))
    select   <- dplyr::select_vars( op_vars(op$x), tidyselect::everything()
                                  , include = op_grps(op$x)
                                  , exclude = levels
                                  )
    key      <- rlang::quo_name(op$args$key)
    value    <- rlang::quo_name(op$args$value)

    if (length(select))
        assert_that( !(key   %in% select)
                   , !(value %in% select)
                   )

    unpivot_query( from   = dbplyr::sql_build(op$x, con=con)
                 , select = ident(select)
                 , key    = ident(key)
                 , value  = ident(value)
                 , levels = ident(levels)
                 )
}


#' @export
op_vars.op_unpivot <- function(op){
    select <- dplyr::select_vars(op_vars(op$x), include=op_grps(op$x))
    c( names(select), rlang::quo_name(op$args$key), rlang::quo_name(op$args$value))
}

#' @export
op_grps.op_unpivot <- function(op)op_grps(op$x)


#' Create a pivot query representation
#'
#' Create a pivot table
#'
#' @param from   the from clause
#' @param select variables to select in addition to levels.
#' @param key    Variable columns originate from
#' @param value  The expression to evaluate to create the values
#' @param levels the columns to turn into values of a variable.
#' @param order_by optional order by clause
#'
#' @export
unpivot_query <-
    function( from
            , key
            , value
            , levels
            , select = character(0)
            , order_by = NULL
            ){
        structure( list( from   = from
                       , key    = key
                       , value  = value
                       , levels = levels
                       , select = select
                       )
                 , class = c('unpivot_query', 'query')
                 )
    }


#' @export
sql_render.unpivot_query <-
function( query, con=query$con, ..., root=FALSE){
    assert_that(inherits(query, 'unpivot_query'))
    if (!is.ident(query$from))
        query$from <- dplyr::sql_subquery(con, sql_render(query$from, con, ..., root=root))
    sql_unpivot( con = con
               , from   = query$from
               , select = query$select
               , key    = query$key
               , value  = query$value
               , levels = query$levels
               )
}


#' Create an unpivot query
#'
#' Creates a SQL pivot query.  Similar to the \code{tidyr::gather}
#' function.
#'
#' @inheritParams unpivot_query
#' @param con    a Database connection
#'
#' @importFrom dbplyr escape
#' @export
sql_unpivot <- function(con, from, select, key, value, levels, order_by=NULL)
    UseMethod('sql_unpivot')

#' @export
`sql_unpivot.Microsoft SQL Server` <-
    function(con, from, select, key, value, levels, order_by=NULL){
    pvt.name <- gsub('[()"]', '_', value)
    build_sql( con=con
             , "SELECT "
             , escape(c(select, key, value), collapse=', ', con=con), '\n'
             , "FROM ", from, "\n"
             , "UNPIVOT", '\n'
             , "    (", value, " FOR ", key, " IN", '\n'
             , "        (", escape(levels , parens=FALSE, collapse = ", ", con = con), ')\n'
             , "    ) AS ", pvt.name
             , if (!is.null(order_by)) {
                   assert_that(is.character(order_by), length(order_by) > 0)
                   build_sql(con=con, "\nORDER BY ", escape(unname(order_by), parens=FALSE, collapse=", ", con=con))
               }
             )
    }
