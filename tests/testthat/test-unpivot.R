context("unpivot.R")

test_that("UNPIVOT construction", {
    con <- dbplyr::simulate_mssql()
    src <- dbplyr::src_dbi(con)

    base <- list( x = ident('##iris')
                , vars  = dplyr::tbl_vars(iris)
                ) %>% structure(class=c('op_base_remote', 'op_base', 'op'))


    db_iris <- dbplyr::tbl_lazy(iris, con=con)
    # db_iris <- structure( list( src = src
    #                           , ops = base
    #                           )
    #     , class = c('tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl'))
        
    levels <- rlang::quos(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
        
    long <- unpivot(db_iris, Variable, Value, !!!levels)
    expect_is(long, 'tbl_lazy')
    expect_identical(long$src, src)
    expect_equal(long$ops$dots, levels)
    expect_equal(long$ops$args$key, rlang::quo(Variable))
    expect_equal(long$ops$args$value, rlang::quo(Value))

    built <- sql_build(long)
    expect_is(built, 'unpivot_query')
    expect_is(built, 'query')
    expect_equal(built$key, ident('Variable'))
    expect_equal(built$value, ident('Value'))
    expect_equal(built$levels
                , ident(rlang::set_names(c('Sepal.Length', 'Sepal.Width'
                                          , 'Petal.Length', 'Petal.Width'))))
    expect_equal(built$select, ident(c('Species'='Species')))

    expect_equal(dplyr::tbl_vars(long), c('Species', 'Variable', 'Value'))
    expect_equal(dplyr::group_vars(long), character(0))


    query <- sql_render(built, con=con)
    expect_is(query, 'sql')
    expect_is(query, 'character')
    expect_equal(query, 
        dbplyr::sql(paste( 'SELECT `Species`, `Variable`, `Value`'
                         , 'FROM "df"'
                         , 'UNPIVOT'
                         , '    ("Value" FOR "Variable" IN'
                         , '        ("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")'
                         , '    ) AS "Value"'
                         , sep='\n'))
    )
})
test_that("unpivot.data.frame", {
    levels <- rlang::quos(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
    long <- unpivot(iris, Variable, Value, !!!levels)
    expected <- gather(iris, Variable, Value, !!!levels)
    
    expect_identical(long, expected)
})
test_that("order_by", {
    query <- unpivot_query( from   = ident('my_table')
                          , key    = ident('Key')
                          , value  = ident('Value')
                          , levels = ident(c('a', 'b', 'c'))
                          , select = ident('Variable')
                          , order_by = ident(c('Key', 'Variable'))
                          )
    expect_is(query, 'unpivot_query')
    expect_is(query, 'query')
    expect_identical(query$from    , ident('my_table'))
    expect_identical(query$key     , ident('Key'))
    expect_identical(query$value   , ident('Value'))
    expect_identical(query$levels  , ident(c('a', 'b', 'c')))
    expect_identical(query$select  , ident('Variable'))
    expect_identical(query$order_by, ident(c('Key', 'Variable')))
    
    sql <- sql_render(con=dbplyr::simulate_mssql(), query)
    expect_is(sql, 'sql')
    expect_identical(sql, 
        dbplyr::sql(paste( 'SELECT `Variable`, `Key`, `Value`'
                         , 'FROM "my_table"'
                         , 'UNPIVOT'
                         , '    ("Value" FOR "Key" IN'
                         , '        ("a", "b", "c")'
                         , '    ) AS "Value"'
                         , 'ORDER BY "Key", "Variable"'
                         , sep='\n'))
    )
})


