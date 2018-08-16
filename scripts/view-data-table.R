library(optparse)
library(promisedyntracer)
library(stringr)
library(fs)

parse_program_arguments <- function() {
    option_list <- list(
        make_option(c("-r", "--rows"), action="store",
                    type="character", default="1:row_count",
                    help="Row indices of data frame to be printed (use 'row_count' to refer to the last row index)",
                    metavar="rows"),

        make_option(c("-c", "--columns"), action="store",
                    type="character", default="1:column_count",
                    help="Column indices of data frame to be printed (use 'column_count' to refer to the last column index)",
                    metavar="columns"),

        make_option(c("-q", "--quote"), action="store_true",
                    type="logical", default=FALSE,
                    help="Whether or not strings should be printed with surrounding quotes",
                    metavar="quote"),

        make_option(c("-w", "--width"), action="store",
                    type="integer", default=160,
                    help="Maximum number of columns on a line used for printing",
                    metavar="width"),

        make_option(c("--row-names"), action="store_true",
                    type="logical", default=FALSE,
                    help="Whether or not row names should be printed",
                    metavar="row-names"),

        make_option(c("--column-names"), action="store_true",
                    type="logical", default=FALSE,
                    help="Whether or not column names should be printed",
                    metavar="column-names"),

        make_option(c("-p", "--page"), action="store_true",
                    type="logical", default=FALSE,
                    help="Whether or not data frame should be paged",
                    metavar="page"))

    args <- parse_args(OptionParser(usage="view-data-table.R [OPTION]... filename",
                                    option_list = option_list),
                       positional_arguments=TRUE)

    list(rows = args$options$rows,
         columns = args$options$columns,
         quote = args$options$quote,
         width = args$options$width,
         row_names = args$options$`row-names`,
         column_names = args$options$`column-names`,
         page = args$options$page,
         filepath = if(is.na(args$args[1])) "" else args$args[1])
}

show_data_table <- function(settings) {
    if(!file_exists(settings$filepath)) {
        stop(str_c("cannot find file '", settings$filepath, "'"))
    }

    options(width = settings$width)

    options(max.print = .Machine$integer.max)

    df <- read_data_table(settings$filepath)

    row_count <- nrow(df)
    column_count <- ncol(df)

    df_view <- df[eval(parse(text = settings$rows)),
                  eval(parse(text = settings$columns))]

    if(!settings$column_names) {
        colnames(df_view) <- c()
    }

    if(settings$page)
        page(df_view, method = "print",
             quote = settings$quote,
             row.names = settings$row_names)
    else
        print(df_view,
              quote = settings$quote,
              row.names = settings$row_names)

    invisible(NULL)
}

show_data_table(parse_program_arguments())
