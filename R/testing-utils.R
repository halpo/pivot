apply_op <- function(op)UseMethod('apply_op')
apply_op.tbl_lazy    <- function(op)apply_op(op$ops)
apply_op.op_base_local <- function(op)op$x
apply_op.op_pivot <- function(op){
    levels <- op$args$levels
    data <- apply_op(op$x) 
    pivot( data
         , key   = !!op$args$key
         , value = !!op$args$value
         , fill  = op$args$fill
         , !!!op$dots
         )
}
apply_op.op <- function(op){
    FUN <- match.fun(substring(class(op)[1], 4))
    e <- rlang::expr(FUN(!!!c(rlang::expr(.data), op$args, op$dots)))
    
    .data <- apply_op(op$x)
    eval(e, env=environment())
}
pull.tbl_lazy <- function(.data, var=-1)
    dplyr::pull(apply_op(.data), var=!!rlang::enquo(var))



