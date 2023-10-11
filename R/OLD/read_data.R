

svr_read_csv_files <- function(
    csv_dir,
    separator = ';',
    col_types = 'ALL_CHAR',
    na = c( '', ' ' )
) {

    if( isTRUE( col_types == 'ALL_CHAR' ) ) {
        col_types <- cols( .default = 'c' )
    }

    # By @joran (SO), https://stackoverflow.com/users/324364/joran
    # https://stackoverflow.com/a/28887734/7002525

    # If the path is different than your working directory you'll
    # need to set full.names = TRUE to get the full paths
    csv_files <- list.files( csv_dir, full.names = TRUE )

    # Apply the `read_delim` to all files in the list
    # and pass the separator as an additional argument
    tibble_list <- lapply(
        csv_files,
        read_delim,
        # Arguments for the read_delim() function:
        delim = separator,
        col_types = col_types,
        na = na
    )

    # Set the name of each list element to its
    # respective file name. Note full.names = FALSE to
    # get only the file names, not the full path
    names( tibble_list ) <- gsub(
        '.csv',
        '',
        list.files( csv_dir, full.names = FALSE ),
        fixed = TRUE
    )

    tibble_list
}


svr_conform_df <- function(
    df,
    col_names,
    dtypes,
    categories,
    rename_tolower = TRUE,
    add_missingcols = TRUE,
    drop_undefinedcols = TRUE,
    fun_addmetadata = function( df, nameindex ) {
        df %>% mutate( dataset_name = nameindex )
    },
    fun_arg = NULL
) {

    if( rename_tolower ) {
        # Ensure that all column names are in lower case letters
        col_names <- tolower( col_names )
        df <- df %>%
            rename_with( tolower )
    }

    if( add_missingcols ) {
        df <- df %>%
            svr_add_missing_columns( column_names = col_names )
    } else {
        col_names <- col_names[col_names %in% names( df )]
    }

    # Set the names to the defined variable names
    df<- df %>%
        setnames(
            old = col_names,
            new = names( col_names ),
            # If "missing" columns not added, skip absent names
            skip_absent = !add_missingcols
        )

    if( drop_undefinedcols ) {

        # Select only defined variables
        df <- df %>%
            select( all_of( names( col_names ) ) )
    }

    if( !is.null( fun_addmetadata ) ) {
        df <- df %>%
            # Apply the metadata function
            fun_addmetadata( fun_arg )
    }

    df %>%

        # TODO: add validate_df()

        # Ensure that all the variables have
        # the right data type (see "Utilities")
        svr_set_df_dtypes(
            dtypes = dtypes,
            categories = categories
        )
}


svr_conform_df_to_specs <- function(
    df,
    specs,
    add_missingcols = TRUE,
    drop_undefinedcols = TRUE,
    fun_addmetadata = function( df, nameindex ) {
        df %>% mutate( dataset_name = nameindex )
    },
    fun_arg = NULL
) {

    dtypes <- deframe( specs$variables[c( 'varname', 'datatype' )] )
    categories <- svr_create_category_spec_list( specs )

    # Get the non-NA mapping rows and name them with the variable name
    col_names <- specs$variables %>%
        filter( !is.na( mapping ) ) %>%
        select( varname, mapping ) %>%
        deframe()

    df %>%
        svr_conform_df(
            col_names = col_names,
            dtypes = dtypes,
            categories = categories,
            add_missingcols = add_missingcols,
            drop_undefinedcols = drop_undefinedcols,
            fun_addmetadata = fun_addmetadata,
            fun_arg = fun_arg
        )
}

svr_edit_dflistitem <- function(
    nameindex,
    list,
    col_names,
    dtypes,
    categories,
    add_missingcols = TRUE,
    drop_undefinedcols = TRUE,
    fun_addmetadata = function( df, nameindex ) {
        df %>% mutate( dataset_name = nameindex )
    }
) {

    # Access an item in the list with the name
    list[[nameindex]] %>%

        svr_conform_df(
            col_names = col_names,
            dtypes = dtypes,
            categories = categories,
            add_missingcols = add_missingcols,
            drop_undefinedcols = drop_undefinedcols,
            fun_addmetadata = fun_addmetadata,
            fun_arg = nameindex
        )
}


svr_conform_dflist_to_specs <- function(
    df_list,
    specs,
    add_missingcols = TRUE,
    drop_undefinedcols = TRUE,
    fun_addmetadata = function( df, nameindex ) {
        df %>% mutate( dataset_name = nameindex )
    }
) {

    # Adapted from an SO answers by
    # Gavin Simpson, https://stackoverflow.com/a/11115275/7002525
    # and Brian Diggs, https://stackoverflow.com/a/18520422/7002525

    dtypes <- deframe( specs$variables[c( 'varname', 'datatype' )] )
    categories <- svr_create_category_spec_list( specs )

    # Get the non-NA mapping rows and name them with the variable name
    col_names <- specs$variables %>%
        filter( ! is.na( mapping ) ) %>%
        select( varname, mapping ) %>%
        deframe()

    # Get the names of the tibble list
    list_names <- names( df_list )

    lapply(

        # Use `setNames()` to apply a function for each name of the list
        setNames( list_names, list_names ),

        # The function to apply
        svr_edit_dflistitem,

        # Pass parameters to the function
        list = df_list,
        col_names = col_names,
        dtypes = dtypes,
        categories = categories,
        add_missingcols = add_missingcols,
        drop_undefinedcols = drop_undefinedcols,
        fun_addmetadata = fun_addmetadata
    )
}


svr_read_excel_data_sheet <- function(
    path,
    specs,
    sheetname = NULL,
    non_na_col = NULL,
    col_types = 'text',
    meta_varname = 'META__EXCELSHEET'

) {

    svr_read_excel_sheet(
        path = path,
        sheetname = sheetname,
        non_na_col = non_na_col,
        meta_varname = meta_varname
    ) %>%
        svr_conform_df_to_specs(
            specs = specs
        )
}



