

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
                plot_fun( ... )
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
        ungrouped_label = 'ALL',
        ...
) {

    l = list()

    if( is.null( grouping_varnames ) ) {
        l[[ungrouped_label]] <- plot_fun( df, ... )

    } else{
        l <- svr_create_group_plots(
            df = df,
            plot_fun = plot_fun,
            grouping_varnames = grouping_varnames,
            ...
        )
        l[[ungrouped_label]] <- plot_fun( df, ... )
    }

    l
}


svr_theme <- function() {

    theme_minimal() +

        theme(
            axis.title = element_text( size = 12 ),
            axis.text = element_text( size = 11 )
        )
}


svr_theme_vertical <- function() {

    svr_theme() +

        theme(
            axis.text.y = element_text(
                size = 14,
                face = 'bold'
            )
        )
}


svr_add_linebreaks <- function(
        label,
        max_nof_rows = 3,
        max_row_length = 20,
        ellipsis = '..'
) {

    # Split the label into words
    words <- str_split( label, ' ', simplify = TRUE )

    # Ensure that all words are shorter than the max row length + "\n"
    words <- lapply(
        words,
        str_trunc,
        width = max_row_length - 2,
        side = 'right',
        ellipsis = ellipsis
    )

    rows <- vector( mode = 'list', length = max_nof_rows )
    row_no <- 1
    word_no <- 1

    for( word in words ) {

        if( is.null( rows[[row_no]] ) ) {
            # The row is still empty -> add the word
            rows[[row_no]] <- word

        } else {

            # The row has words

            if( nchar( rows[[row_no]] ) +
                nchar( word ) + 2 > max_row_length ) {

                # The word does not fit into the row

                # Increment the row number
                row_no <- row_no + 1

                # Reached the max nof rows
                if( row_no > max_nof_rows ) {

                    # Add ellipsis all words don't fit
                    if( word_no <= length( words ) ) {
                        rows[[row_no-1]] <- paste0(
                            rows[[row_no-1]], ellipsis
                        )
                        # Ensure that the row length does not exceed max
                        rows[[row_no-1]] <- str_trunc(
                            rows[[row_no-1]], width = max_row_length,
                            side = 'right', ellipsis = ellipsis
                        )
                    }

                    break # out of the loop
                }

                # Add the row to the next row
                rows[[row_no]] <- word

            } else {

                # The word fits

                # Paste the word at the end of the row
                rows[[row_no]] <- paste0( rows[[row_no]], ' ', word )
            }
        }

        # Increment the word number
        word_no <- word_no + 1
    }

    # Paste non-NULL (compact) rows together with a line break
    paste( compact( rows ), collapse = '\n' )
}

