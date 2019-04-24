context("tidyr.R")

test_that("spread.tbl_lazy", {
    con <- dbplyr::simulate_mssql()
    src <- dbplyr::src_dbi(con)
    lazy.long <- dbplyr::tbl_lazy(gather(iris, Variable, Value, 1:4), con=con)

    wide <- spread(lazy.long, Species, mean(Value))
    expect_is(wide, 'tbl_lazy')
    expect_is(wide$ops, 'op_pivot') 
    expect_equal(wide$ops$dots, rlang::quos(tidyselect::everything()))
    expect_null(wide$ops$args$levels)
    expect_equal( wide$ops$args$key, rlang::quo(Species))
    expect_equal( wide$ops$args$value, rlang::quo(mean(Value)))
})
