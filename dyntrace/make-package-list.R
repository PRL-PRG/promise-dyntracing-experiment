main <- function() {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) == 0)
        stop("missing output filepath")

    filepath <- args[1]
    packages <- installed.packages(lib.loc = .libPaths())[, "Package"]
    write.table(packages, filepath, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = FALSE);
}

main()

