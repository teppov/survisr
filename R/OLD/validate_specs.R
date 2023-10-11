
svr_validate_specs <- function(
    specs,
    variable_datatypes = c(
        'categorical',
        'integer',
        'decimal',
        'text'
    ),
    colorhex_regex = '^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$'
) {

    # Get the unique color names from the category specs
    colornames <- svr_get_unique( specs$categories, colorname )
    # Get the unique category set names from the category specs
    categorysetnames <- svr_get_unique( specs$categories, categoryset )

    # Define the rules
    rules_setup <- validator(
        tabletype_notna = !is.na( tabletype ),
        tablename_notna = !is.na( tablename )
    )
    rules_colors <- validator(
        colorname_notna = !is.na( colorname ),
        colorname_unique = all_unique( colorname ),
        colorhex_valid = grepl( colorhex_regex, colorhex )
    )
    rules_categories <- validator(
        categoryset_notna = !is.na( categoryset ),
        categoryname_notna = !is.na( categoryname ),
        colortable_valid = colortable %in% spec_excel_sheetnames,
        colorname_valid = colorname %in% colornames
    )
    rules_variables <- validator(
        varname_notna = !is.na( varname ),
        varname_unique = all_unique( varname ),
        datatype_valid = datatype %in% variable_datatypes,
        categories_defined = if( datatype == 'categorical' )
            categorytable %in% spec_excel_sheetnames &
            categoryset %in% categorysetnames,
        mapping_unique = all_unique( mapping )
    )

    checks <- list()
    checks$setup <- confront(
        dat = specs$setup, x = rules_setup
    )
    checks$color <- confront(
        dat = specs$colors, x = rules_colors
    )
    checks$category <- confront(
        dat = specs$categories, x = rules_categories
    )
    checks$variable <- confront(
        dat = specs$variables, x = rules_variables
    )

    if( !is.null( specs$rules ) ) {
        rules_rules <- validator(
            rulename_notna = !is.na( rulename ),
            rule_notna = !is.na( rule )
        )
        checks$rules <- confront(
            dat = specs$rules, x = rules_rules
        )
    } else {
        checks$rules <- NULL
    }

    # Add flag for indicating whether all rules are valid
    checks$all_valid <- all( sapply( checks, all, na.rm = TRUE ) )

    checks
}
