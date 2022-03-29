

svr_summary_num <- function(
    df,
    varnames = NULL,
    selection = NULL,
    as_numeric = FALSE,
    digits = 1
) {

    if( !is.null( varnames ) ) {
        df.sub <- df %>%
            select( all_of( varnames ) )
    } else if( !is.null( selection ) ) {
        df.sub <- df %>%
            select( {{ selection }} )
    } else {
        df.sub <- df
    }

    if( as_numeric ) {
        df.sub <- df.sub %>%
            mutate( across( everything(), as.numeric ) )
    }

    df.sub %>%

        pivot_longer( cols = everything() ) %>%

        group_by( name ) %>%

        summarise(
            n = sum( !is.na( value ) ),
            na = sum( is.na( value ) ),
            uniq = n_distinct( value ),
            min = round( min( value, na.rm = TRUE ), digits = digits ),
            max = round( max( value, na.rm = TRUE ), digits = digits ),
            mean = round( mean( value, na.rm = TRUE ), digits = digits ),
            sd = round( sd( value, na.rm = TRUE ), digits = digits ),
            med = round( median( value, na.rm = TRUE ), digits = digits ),
            mode = svr_mode( value, as_string = TRUE )
        )
}


svr_summary_cat <- function(
    df,
    varnames = NULL,
    as_character = FALSE
) {

    if( !is.null( varnames ) ) {
        df.sub <- df %>%
            select( all_of( varnames ) )
    } else if( !is.null( selection ) ) {
        df.sub <- df %>%
            select( {{ selection }} )
    } else {
        df.sub <- df
    }

    if( as_character ) {
        df.sub <- df.sub %>%
            mutate( across( everything(), as.character ) )
    }

    df.sub %>%

        pivot_longer( cols = everything() ) %>%

        group_by( name ) %>%

        summarise(
            n = sum( !is.na( value ) ),
            na = sum( is.na( value ) ),
            uniq = n_distinct( value ),
            mode = svr_mode( value, as_string = TRUE ),
            nmode = sum( value == mode, na.rm = TRUE )
        )
}
