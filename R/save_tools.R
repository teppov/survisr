

svr_save_plot <- function(
    p,
    filename_sans_ext,
    filename_prefix = NULL,
    filename_suffix = NULL,
    dir_path = NULL,
    units = 'mm',
    # A 16:9 ratio that fits into an A4
    width  = 288,
    height = 162,
    height_scale = 1,
    dpi = 320,
    bg_color = 'white',
    return_plot = TRUE
) {

    # Paste possible prefix and suffix to the file name and add the `.png` extension
    file_name <- paste0(
        filename_prefix,
        filename_sans_ext,
        filename_suffix, '.png' )

    ggsave(
        plot = p,
        filename = file_name,
        path = dir_path,
        units = units,
        width = width,
        height = height * height_scale,
        dpi = dpi,
        bg = bg_color,
    )

    if( return_plot ) {
        return( p )
    }
}
