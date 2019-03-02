main <- function() {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) == 0) {
        stop("missing output filepath")
    }

    filepath <- args[1]

    cat("Searching for packages in: \n")
    print(.libPaths())
    cat("\n")

    packages <- sort(unique(installed.packages(lib.loc = .libPaths())[, "Package"]))

    cat("Found", toString(length(packages)), "unique packages\n\n")

    cat("Writing package list to", filepath, "\n");

    write.table(packages, filepath, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = FALSE);
}

main()

