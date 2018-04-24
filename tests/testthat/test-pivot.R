context("pivot.R")

test_that("PIVOT construction", {
    con <- dbplyr::simulate_mssql()
    src <- dbplyr::src_dbi(con)

    pivot.base <- 
        list( x = ident('##long')
            , vars  = c("Species", "Variable", "Value")
            ) %>% structure(class=c('op_base_remote', 'op_base', 'op'))
    db_long <- structure( list( src = src
                              , ops = pivot.base
                              )
        , class = c('tbl_dbi', 'tbl_sql', 'tbl_lazy', 'tbl'))


    grouped <- group_by(db_long, Variable)

    wide <- pivot(grouped, Species, mean(Value, na.rm=TRUE), Length = Petal.Length, Petal.Width)
    expect_is(wide, 'tbl_sql')
    expect_identical(wide$src, src)
    expect_identical(wide$ops$x, grouped$ops)
    expect_is(wide$ops, 'op_pivot')
    expect_equal(wide$ops$dots, quos(Length = Petal.Length, Petal.Width))
    expect_equal(wide$ops$args$key, quo(Species))
    expect_equal(wide$ops$args$value, quo(mean(Value, na.rm=TRUE)))
    expect_null(wide$ops$args$fill)
    expect_equal(op_vars(wide), c('Variable', 'Length', 'Petal.Width'))

    expect_equal( levels_op_pivot(wide$ops)
                , c(Length = 'Petal.Length', 'Petal.Width')
                )

    pivot.built <- sql_build(wide)
    expect_is(pivot.built, 'pivot_query')
    expect_is(pivot.built, 'query')
    expect_identical(pivot.built$from  , ident('##long'))
    expect_identical(pivot.built$key   , ident('Species'))
    expect_identical(pivot.built$value , quo(mean(Value, na.rm=TRUE)))
    expect_identical(pivot.built$levels, ident(c('Length'='Petal.Length', 'Petal.Width')))
    expect_identical(pivot.built$select, ident(c('Variable'='Variable')))
    expect_null(pivot.built$fill)
    expect_null(pivot.built$order_by)

    pivot.sql <- sql_render(wide)
    expect_is(pivot.sql, 'sql')
    expect_is(pivot.sql, 'character')
    expect_equal(length(pivot.sql), 1)

    expect_identical(pivot.sql,
        sql(paste( 'SELECT "Variable", "Petal.Length" AS "Length", "Petal.Width"'
                 , 'FROM "##long"'
                 , 'PIVOT ('
                 , '    AVG("Value")'
                 , '    FOR "Species" IN ("Petal.Length", "Petal.Width")'
                 , ') AS "mean_Value__na_rm___TRUE_"'
                 , sep='\n'))
    )
})

