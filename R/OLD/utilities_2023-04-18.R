

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


#' Add missing columns to a data frame.
#'
svr_add_missing_columns <- function( df, column_names ) {

    # Loop over all the given names
    for( name in column_names ) {

        # If a name not found in the column names
        # of the given data frame..
        if( ! name %in% colnames( df ) ) {

            #.. add an empty column with the name
            df[[name]] <- NA
        }
    }

    df
}



# Factorize categorical variable.
# In R, categorical variables are defined as factors.
# Factors enable plotting variables with categorical scales
# instead of continuous scales.
svr_factorize <- function(
    df,
    varname,
    levels = NULL,
    labels = NULL
) {

    df %>%

        # Mutate the specified variable into a factor
        mutate( across(
            all_of( varname ),
            ~factor(
                .x,
                levels = levels,
                labels = labels
            )
        ) )
}



# Set the data types of variables

svr_set_var_dtype <- function(
    df,
    varname,
    dtype,
    cat_levels = NULL,
    cat_labels = NULL
) {

    if( dtype == 'integer' ) {
        df <- df %>%
            mutate( !! varname := as.integer( .data[[varname]] ) )
    } else if( dtype == 'decimal' ) {
        df <- df %>%
            mutate( !! varname := as.numeric( .data[[varname]] ) )
    } else if( dtype == 'categorical' ) {
        df <- df %>%
            svr_factorize( varname, cat_levels, cat_labels )
    } else {
        df <- df %>%
            mutate( !! varname := as.character( .data[[varname]] ) )
    }

    df
}


svr_set_df_dtypes <- function(
    df,
    dtypes,
    categories = NULL,
    duplicate_prefix = 'BAK__'
) {

    # Use only names found in the data
    common_names <- names( dtypes )[names( dtypes ) %in% names( df )]

    # "Backup", i.e. create duplicates of all variables to be changed
    if( !is.null( duplicate_prefix ) ) {
        df <- df %>%
            mutate( across(
                .cols = all_of( common_names ),
                .names = paste0( duplicate_prefix, '{.col}' )
            ) )
    }

    for( varname in common_names ) {

        dtype <- dtypes[[varname]]

        if( dtype == 'categorical' ) {
            cats <- deframe(
                categories[[varname]][c( 'categoryname', 'mapping' )]
            )
        }

        df <- df %>%
            svr_set_var_dtype(
                varname,
                dtype,
                cat_levels = cats,
                cat_labels = names( cats )
            )
    }

    df
}


# Get unique values from column

svr_get_unique <- function( df, colname ) {

    df %>%
        filter( !is.na( {{ colname }} ) ) %>%
        pull( {{ colname }} ) %>%
        unique()
}


# Augmented `read_excel()` function

svr_read_excel_sheet <- function(
    path,
    sheetname = NULL,
    non_na_col = NULL,
    col_types = 'text',
    meta_varname = 'META__EXCELSHEET'
) {

    df <- read_excel(
        path = path,
        sheet = sheetname,
        col_types = col_types,
        # Handle possible duplicate column labels
        # by adding a suffix with "_"
        # https://stackoverflow.com/a/64868570/7002525
        .name_repair = ~make.unique( .x, sep = '_' )
    )

    if( !is.null( meta_varname ) ) {
        # Add the name of the sheet as a "meta" variable
        df[meta_varname] <- sheetname
    }

    if( !is.null( non_na_col ) ) {
        df <- filter( df, !is.na( .data[[non_na_col]] ) )
    }

    df
}


# Read multiple sheets from an Excel file

svr_read_excel_sheets <- function(
    path,
    non_na_col = NULL,
    sheetnames = NULL,
    col_types = 'text',
    read_excel_fun = svr_read_excel_sheet,
    meta_varname = 'META__EXCELSHEET'
) {

    if( is.null( sheetnames ) | length( sheetnames ) == 0 ) {
        return(
            svr_read_excel_sheet(
                path = path,
                sheetname = NULL,
                non_na_col = non_na_col,
                col_types = col_types,
                meta_varname = meta_varname
            )
        )
    }

    lapply(
        sheetnames,
        read_excel_fun,
        path = path,
        non_na_col = non_na_col,
        col_types = col_types,
        meta_varname = meta_varname
    ) %>%
        bind_rows()
}


# Validate data frame
#
# Define a function that stops a process in case a rule fails:

svr_validate_df <- function( df, rules, msg = NULL ) {

    rules_valid <- confront( dat = df, x = rules )

    if( ! all( rules_valid, na.rm = TRUE ) ) {
        stop( paste( c( 'Data not valid!', msg ), collapse = ' ' ) )
    }

    # Return a valid data frame
    df
}


svr_add_categorical_vars <- function(
    df,
    category_spec_list,
    prefix = 'fct__'
) {

    for( varname in names( category_spec_list ) ) {

        fct_varname <- paste0( prefix, varname )

        df <- df %>%

            # Mutate new factor variables
            # with levels from the specification
            mutate( across(
                all_of( varname ),
                ~factor(
                    .x,
                    levels = category_spec_list[[varname]]$value
                ),

                # Create names for the new variables using the prefix
                .names = paste0( prefix, '{.col}' )
            ) ) %>%

            # Recode the values (and factor levels)
            # with the labels from the specification
            mutate( across(
                all_of( fct_varname ),
                ~recode(
                    .x,
                    !!!deframe(
                        category_spec_list[[varname]][c(
                            'mapping',
                            'value'
                        )]
                    )
                )
            ) )
    }

    df
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

