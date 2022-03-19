
svr_create_group_plots_1grouping <- function(
    df,
    plot_fun,
    grouping_varname,
    ...
) {

    # Split the dataframe by the grouping variable
    df.s <- split( df, df[[grouping_varname]] )

    # Remove empty data frames (`nrow( df ) > 0`)
    df.s <- df.s[sapply( df.s, function( df ) nrow( df ) ) > 0]

    # Add the varname as prefix to list item names
    names( df.s ) <- paste0( grouping_varname, '__', names( df.s ) )

    # Get the names of the dataframe list
    list_names <- names( df.s )

    lapply(

        # Use `setNames()` to apply a function for each name of the list
        setNames( list_names, list_names ),

        # The function to apply
        function( nameindex, list, plot_fun, ... ) {
            list[[nameindex]] %>%
                plot_fun(
                    title_suffix = paste0( ': ', nameindex ),
                    ...
                )
        },

        # Pass parameters to the function
        list = df.s,
        plot_fun = plot_fun,
        ...
    )
}


svr_create_group_plots <- function(
    df,
    plot_fun,
    grouping_varnames,
    ...
) {

    l <- lapply(
        grouping_varnames,
        svr_create_group_plots_1grouping,
        df = df,
        plot_fun = plot_fun,
        ...
    )

    unlist( l, recursive = FALSE )
}


svr_create_plots <- function(
    df,
    plot_fun,
    grouping_varnames = NULL,
    ...
) {

    if( is.null( grouping_varnames ) ) {
        list( 'UNGROUPED' = plot_fun( df, ... ) )

    } else{
        l <- svr_create_group_plots(
            df = df,
            plot_fun = plot_fun,
            grouping_varnames = grouping_varnames,
            ...
        )
        l[['UNGROUPED']] <- plot_fun( df, ... )
        l
    }
}


svr_theme <- function() {

    theme_minimal() +

        theme(
            axis.title = element_text( size = 12 ),
            axis.text = element_text( size = 11 )
        )
}


svr_theme_v_varnames <- function() {

    svr_theme() +

        theme(
            axis.text.y = element_text( size = 14 )
        )
}


