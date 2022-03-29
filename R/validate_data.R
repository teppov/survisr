

svr_create_validation <- function( specs ) {

    # Define a header as it appears in a
    # "validator" object turned data.frame
    rules_header <- c(
        'name', 'label', 'description', 'origin',
        'created', 'language', 'severity', 'rule'
    )

    # Create a tibble with only a header
    # From Nik@SO: https://stackoverflow.com/a/60495352/7002525
    validator_rules <- rules_header %>%
        rlang::rep_named( list( character() ) ) %>%
        as_tibble()

    # Rules for categorical variables
    #################################

    # Get the name and category set
    # of the variables with a category set defined
    category_vars <- specs$variables %>%
        select( varname, categoryset ) %>%
        filter( !is.na( categoryset ) )

    # Loop over all the categorical variables
    for( varname in category_vars$varname ) {

        # Get the name of the category set
        # from the `category_vars` data frame
        categoryset_name <- category_vars[
            category_vars$varname==varname, ]$categoryset

        # Get the category values of the current category set
        category_names <- specs$categories %>%
            filter( categoryset == categoryset_name ) %>%
            pull( categoryname )

        # Add rows to the `validator_rules` data frame
        validator_rules <- validator_rules %>%
            add_row(
                name = paste0( varname, '_cat' ),
                rule = paste0(
                    varname,
                    ' %vin% c( "',
                    paste( category_names, collapse = '", "' ),
                    '" )'
                ),
                label = paste0( 'Categorical variable: ', varname ),
                description = paste0(
                    'A rule for testing the values of the variable "',
                    varname, '" against a set of predefined categories.'
                )
            )
    }

    # Rules for integer variables
    #############################

    # Create a data frame with only categorical variables
    integer_vars <- specs$variables %>%
        select( varname, datatype ) %>%
        filter( datatype == 'integer' )

    # Loop over all categorical variables
    for( varname in integer_vars$varname ) {

        # Add rows to the `validator_rules` data frame
        validator_rules <- validator_rules %>%
            add_row(
                name = paste0( varname, '_int' ),
                rule = paste0( 'is.wholenumber( ', varname, ' )' ),
                label = paste0( 'Integer variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of the variable "',
                    varname, '" are integer numbers.'
                )
            )
    }

    # Rules for decimal variables
    #############################

    # Create a data frame with only categorical variables
    decimal_vars <- specs$variables %>%
        select( varname, datatype ) %>%
        filter( datatype == 'decimal' )

    # Loop over all categorical variables
    for( varname in decimal_vars$varname ) {

        # Add rows to the `validator_rules` data frame
        validator_rules <- validator_rules %>%
            add_row(
                name = paste0( varname, '_dec' ),
                rule = paste0( 'is.numeric( ', varname, ' )' ),
                label = paste0( 'Decimal variable: ', varname ),
                description = paste0(
                    'A rule for testing if the values of the variable "',
                    varname, '" are decimal numbers.'
                )
            )
    }

    # Other rules
    #############

    rules <- specs$rules %>%
        rename( name = rulename )

    validator_rules <- validator_rules %>%
        bind_rows( rules )

    # # Create a data frame with only variables with other rules
    # rule_vars <- specs$variables %>%
    #     select( varname, ruleset ) %>%
    #     filter( !is.na( ruleset ) )
    #
    # # Loop over all variables with rules
    # for( varname in rule_vars$varname ) {
    #
    #     # Get the name of the rule set from the `rule_vars` data frame
    #     ruleset_name <- rule_vars[rule_vars$varname==varname, ]$ruleset
    #
    #     # Create a data frame with only the rows of the current rule set
    #     ruleset_df <- specs$rules %>%
    #         filter( ruleset == ruleset_name )
    #
    #     # Initialize an index for naming the rules
    #     i = 1
    #
    #     # Loop over all rule templates in the created `ruleset_df` data frame
    #     for( rule in ruleset_df$rule ) {
    #
    #         # Add rows to the `validator_rules` data frame
    #         validator_rules <- validator_rules %>%
    #             add_row(
    #                 name = paste0( varname, '_r', i ),
    #                 rule = rule
    #             )
    #
    #         i <- i + 1
    #     }
    # }

    validator( .data = arrange( validator_rules, name ) )
}

