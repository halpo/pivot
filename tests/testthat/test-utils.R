context("utils.R")

test_that("find_connection", {
    con <- dbplyr::simulate_mssql()
    src <- dbplyr::src_dbi(con)

    lazy.long <- dbplyr::tbl_lazy(gather(iris, Variable, Value, 1:4), con=con)
    grouped <- dplyr::group_by(lazy.long, 'Species')

    expect_error(find_connection(lazy.long$ops), 'Could not find base table')
    expect_identical(find_connection(lazy.long), con)
    expect_identical(find_connection(grouped), con)
    
    tab <- dplyr::make_tbl(c("test", "lazy"), src = src)
    base <- dbplyr::op_base( tab, vars=dplyr::tbl_vars(iris), 'test') 
    op.group <- dbplyr::op_single("group_by", x=tab, dots=rlang::quos(Species))
    
    expect_identical(find_connection(base), con)
    expect_identical(find_connection(op.group), con)
})
test_that("get_pivot_levels", {
    con <- dbplyr::simulate_mssql()
    src <- dbplyr::src_dbi(con)
    lazy.iris <- dbplyr::tbl_lazy(iris, con=con)
    lazy.long <- dbplyr::tbl_lazy(gather(iris, Variable, Value, 1:4), con=con)

    expect_identical(apply_op(lazy.iris), iris)

    tbl.pivot <- pivot(lazy.iris, Species, mean(Petal.Length, na.rm=TRUE), starts_with('v')) 
    expect_is(tbl.pivot, 'tbl_lazy')
    expect_is(tbl.pivot$ops, 'op_pivot')
    expect_identical(tbl.pivot$ops$dots, rlang::quos(starts_with('v')))
    expect_null(tbl.pivot$ops$args$levels)
    expect_identical( tbl.pivot$ops$args$key
                    , rlang::quo(Species)
                    )
    expect_identical( tbl.pivot$ops$args$value
                    , rlang::quo(mean(Petal.Length, na.rm=TRUE))
                    )

})


