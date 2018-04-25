#' @importFrom tidyr spread
#' @export
spread.tbl_lazy <- function(data, key, value, fill=NULL, ...){
    key   <- rlang::enquo(key)
    value <- rlang::enquo(value)
    data  %>%
        dplyr::group_by_(.dots =
             tidyselect::vars_select( dplyr::tbl_vars(data)
                                    , tidyselect::everything()
                                    , .exclude = c( dplyr::select_var(dplyr::tbl_vars(data), !!key)
                                                  , all_names(value)
                                                  ))) %>%
        pivot(!!key, !!value, tidyselect::everything(), fill=fill)
}

#' @importFrom tidyr gather
#' @export
gather.tbl_lazy <- function(data, key='key', value='value', ...){
    key   <- rlang::enquo(key)
    value <- rlang::enquo(value)
    dots  <- rlang::quos(...)
    unpivot(data, key, value, !!!dots)
}
