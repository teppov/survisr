#' Calculate a row-wise summary across variables.
#' Return `NA` if nothing found with the `tidy_select`.
#' To be used inside `mutate()`, for example:
#'
#' df %>%
#'     mutate(
#'
#'         mean__ce_det = mean(
#'             c_across( any_of( c(
#'                 "num__ce_det_01", "num__ce_det_02"
#'             ) ) ),
#'             na.rm = TRUE
#'         )
#' =>
#'         mean__ce_det = td_summary_across(
#'             c(
#'                 var_01, var_02
#'             ),
#'             na.rm = TRUE
#'         )
#'     )
#'
#' @param tidy_select A tidyselect selection of variables
#' @param summary_func A summary function to be used across the variables.
#'                     Default: `mean`
#' @param ... Additional arguments for the summary function,
#'            for example `na.rm = TRUE`
#'
#' @return The summary calculation, or `NA` if nothing found with `tidy_select`.
#' @export
#'
#' @examples
#'
td_summary_across <- function(
        tidy_select,
        summary_func = mean,
        na_ratio = 0.5,
        ...
) {

    cx <- c_across( {{ tidy_select }} )

    if( is.null( cx ) ) return( NA )

    nof_vars <- length( cx )

    if_else(
        sum( is.na( cx ) ) / nof_vars < na_ratio,
        summary_func( cx, ... ),
        NA_real_
    )
}


#' Test if x contains integer numbers
#'
#' The function `is.wholenumber` is copied from the R Documentation: "integer".
#'
#' @param x The item to test.
#' @param tol Default: `.Machine$double.eps^0.5`.
#'
#' @return A logical scalar or vector depending on the input.
#'
#' @examples
#' is.wholenumber( 1 ) # is TRUE
#' ( x <- seq( 1, 5, by = 0.5 ) )
#' is.wholenumber( x ) # -->  TRUE FALSE TRUE ...
#'
is.wholenumber <- function(
    x,
    tol = .Machine$double.eps^0.5
) {
    abs( x - round( x ) ) < tol
}


#' Get the colour of text based on the background colour.
#'
#' Adapted from StackOverflow:
#' https://stackoverflow.com/a/61283940/7002525
#'
#' @param background_colour A vector of any of the three kinds of R color
#'     specifications, i.e., either a color name (as listed by `colors()`),
#'     a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see `rgb`),
#'     or a positive integer i meaning `palette()[i]`.
#' @param light_text_colour The colour of light text (on a dark background).
#'     Default: "white"
#' @param dark_text_colour The colour of dark text (on a light background).
#'     Default: "black"
#' @param threshold The luminance threshold. Default: 0.5.
#'
#' @examples
#' get_text_colour( 'yellow' )
#' get_text_colour( '#000000' )
#'
svr_get_text_colour <- function(
    background_colour,
    light_text_colour = 'white',
    dark_text_colour = 'black',
    threshold = 0.5
) {

    background_luminance <- c(
        c( .299, .587, .114 ) %*% col2rgb( background_colour ) / 255
    )

    ifelse(
        background_luminance < threshold,
        light_text_colour,
        dark_text_colour
    )
}




#' Get the first-appearing mode from values
#' Adapted from https://stackoverflow.com/a/8189441/7002525
#'
#' @param values A list of values
#'
#' @return The first mode of x
#' @export
#'
#' @examples
svr_mode1 <- function( values, na.rm = TRUE ) {
    if( na.rm ) values <- na.omit( values )
    uvalues <- unique( values )
    uvalues[which.max( tabulate( match( values, uvalues ) ) )]
}


#' Get modes from a set of modes
#' Adapted from https://stackoverflow.com/a/8189441/7002525
#'
#' @param x
#' @param as_string
#'
#' @return
#' @export
#'
#' @examples
svr_mode <- function( values, na.rm = TRUE, as_string = FALSE ) {

    if( na.rm ) values <- na.omit( values )

    uvalues <- unique( values )
    tab <- tabulate( match( values, uvalues ) )

    if( as_string ) {
        paste( uvalues[tab == max( tab )], collapse = ', ' )

    } else {
        uvalues[tab == max( tab )]
    }
}


#' Calculate mean if the proportion of NA in values is less than
#' or equal to max_prop_na.
#'
#' @param values The values from which the mean is calculated
#' @param max_prop_na The maximum proportion of NA
#'
#' @return
#' @export
#'
#' @examples
svr_mean <- function(
    values,
    max_prop_na = 1/3
) {

    if( sum( is.na( values ) ) / length( values ) > max_prop_na ) {
        return( NA )
    }

    mean( values, na.rm = TRUE )
}


# Get unique values from column
svr_get_unique <- function( df, colname ) {

    df %>%
        filter( !is.na( {{ colname }} ) ) %>%
        pull( {{ colname }} ) %>%
        unique()
}


svr_isemptystr <- function( string ) {
    if( is.null( string ) ) return( TRUE )
    if( is.na( string ) ) return( TRUE )
    if( string == '' ) return( TRUE )
    return( FALSE )
}


svr_pivot_longer <- function(
    df,
    select_vars = everything(),
    keep_categories = NULL,
    dropna = FALSE,
    var_labels = NULL,
    category_labels = NULL
) {

    df.long <- df %>%
        select( {{ select_vars }} ) %>%
        pivot_longer(
            cols = everything()
        )

    if( !is.null( keep_categories ) ) {
        df.long <- df.long %>%
            mutate( value = factor( value, levels = keep_categories ) )
    }

    if( dropna ) {
        df.long <- df.long %>% drop_na()
    }

    if( !is.null( var_labels ) ) {
        df.long <- df.long %>%
            mutate( name = recode( name, !!!var_labels ) )
    }

    if( !is.null( category_labels ) ) {
        df.long <- df.long %>%
            mutate( value = recode( value, !!!category_labels ) )
    }

    df.long
}

