svr_langs <- c( 'fi', 'en' )


svr_read_spec_excel <- function(
    spec_path,
    lang = 'en',
    validate = TRUE
) {

    specs <- list()

    spec_excel_sheetnames <- excel_sheets( spec_path )
    print( 'The spec Excel contains the sheets:' )
    print( paste( spec_excel_sheetnames, collapse = ', ' ) )

    # Read the setup from the first sheet (the default)
    specs$setup <- read_excel(
        path = spec_path,
        col_types = 'text'
    ) %>%
        # Keep only rows where vartable is not NA
        filter( !is.na( vartable ) )

    # Get the variable sheet names from the setup table
    variable_sheetnames <- specs$setup %>%
        pull( vartable )
    if( ! all( variable_sheetnames %in% spec_excel_sheetnames ) ) {
        names_str <- paste( variable_sheetnames, collapse = ', ' )
        stop( paste0(
            'In setup::vartable, not all of the given sheet names
            found within the Excel sheets: ', names_str
        ) )
    }

    # Read all variable sheets from the spec Excel
    # into a single data frame
    specs$variables <- svr_read_excel_sheets(
        path = spec_path,
        sheetnames = variable_sheetnames,
        non_na_col = 'varname',
        meta_varname = 'vartable'
    )

    # Get the (unique) category table names from the variable table
    category_sheetnames <- svr_get_unique( specs$variables, categorytable )
    if( ! all( category_sheetnames %in% spec_excel_sheetnames ) ) {
        names_str <- paste( category_sheetnames, collapse = ', ' )
        stop( paste0(
            'In variables::categorytable, not all of the given
            sheet names found within the Excel sheets: ', names_str
        ) )
    }

    # Read all category sheets from the spec Excel
    # into a single data frame
    specs$categories <- svr_read_excel_sheets(
        path = spec_path,
        sheetnames = category_sheetnames,
        non_na_col = 'categoryset',
        meta_varname = 'categorytable'
    )

    # Get the (unique) color sheet names from the category table
    color_sheetnames <- svr_get_unique( specs$categories, colortable )
    if( ! all( color_sheetnames %in% spec_excel_sheetnames ) ) {
        names_str <- paste( color_sheetnames, collapse = ', ' )
        stop( paste0(
            'In categories::colortable, not all of the given
            sheet names found within the Excel sheets: ', names_str
        ) )
    }

    # Read all color sheets from the spec Excel
    # into a single data frame
    specs$colors <- svr_read_excel_sheets(
        path = spec_path,
        sheetnames = color_sheetnames,
        non_na_col = 'colorname',
        meta_varname = 'colortable'
    )

    # Add the color code to the category specs
    specs$categories <- specs$categories %>%
        left_join(
            specs$colors,
            by = c( 'colortable', 'colorname' )
        )

    # Get the (unique) rule sheet names from the variable table
    rules_sheetnames <- svr_get_unique( specs$variables, ruletable )
    if( length( rules_sheetnames ) > 0 ) {
        if( ! all( rules_sheetnames %in% spec_excel_sheetnames ) ) {
            names_str <- paste( rules_sheetnames, collapse = ', ' )
            stop( paste0(
                'In variables::ruletable, not all of the given
            sheet names found within the Excel sheets: ', names_str
            ) )
        }
        # Read all rule sheets from the spec Excel into a single data frame
        specs$rules <- svr_read_excel_sheets(
            path = spec_path,
            sheetnames = rules_sheetnames,
            non_na_col = 'ruleset',
            meta_varname = 'ruletable'
        )
    } else {
        specs$rules <- NULL
    }

    if( validate ) {
        validation <- svr_validate_specs( specs )
        if( ! validation$all_valid ) {
            stop(
                'The specification is not valid!
                Get more information with
                validate_specs(
                read_spec_excel( ..., validate = FALSE ) )'
            )
        } else {
            print( 'The specification is valid.' )
        }
    }

    specs
}


create_category_spec_list <- function(
    specs,
    lang = 'en'
) {

    # Get the names of categorical variables
    cat_varnames <- specs$variables %>%
        filter( datatype == 'categorical' ) %>%
        pull( varname )

    # Choose plot labels based on language
    plotlabel_str <- paste( c( 'plotlabel', lang ), collapse = '_' )

    categories <- lapply(

        cat_varnames,

        function( name ) {

            # Get the name of the category set from the variable specs
            categoryset_name <- specs$variables %>%
                filter( varname == name ) %>%
                pull( categoryset )

            # Add the required category specs into a list
            specs$categories %>%
                # Get only rows for current category set
                filter( categoryset == categoryset_name ) %>%
                # Create a new variable with values in the chosen language
                mutate( plotlabel = .data[[plotlabel_str]] ) %>%
                # Drop unnecessary columns
                select( name, mapping, plotlabel, colorhex )

        }
    )

    names( categories ) <- cat_varnames

    categories
}


get_col_type <- function( data_type ) {

    if( isTRUE( data_type == 'integer' ) ) {
        return( col_integer() )

    } else if( isTRUE( data_type == 'decimal' ) ) {
        return( col_double() )

    } else {
        return( col_character() )
    }
}


get_col_types <- function( specs ) {

    # Initialize col types
    col_types <- cols_only()

    # Loop over every variable name in the specs..
    for( colname in specs$variables$mapping ) {

        # ..and add col data type for every non-NA column name
        if( ! is.na( colname ) ) {
            col_types$cols[[colname]] <- get_col_type(
                # (the test has to be wrapped inside which(),
                #  see https://stackoverflow.com/a/37351328/7002525 )
                specs$variables[which(
                    specs$variables$mapping == colname
                ), ]$datatype
            )
        }
    }

    col_types
}


svr_get_color_code <- function(
    specs,
    colorname
) {

    specs$colors %>%
        filter( colorname == colorname ) %>%
        pull( colorhex )
}


svr_get_category_labels <- function(
    specs,
    variable_name,
    lang = 'en'
) {
    svr_get_category_specs(
        specs = specs,
        variable_name = variable_name,
        lang = lang
    ) %>% select( name, labels ) %>% deframe()
}


svr_get_var_plotlabels <- function( specs, varnames, lang ){

    plotlabel_lang <- paste0( 'plotlabel_', lang )

    specs$variables %>%
        filter( varname %in% varnames ) %>%
        select( all_of( c( 'varname', plotlabel_lang ) ) ) %>%
        deframe()
}
