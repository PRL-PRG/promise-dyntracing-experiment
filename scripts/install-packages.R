install_from_cran <- function(package_names) {
    if (length(package_names) == 0)
        return()

    install.packages(package_names,
                     repos = "http://cran.us.r-project.org",
                     Ncpus = 8,
                     keep_outputs = TRUE,
                     INSTALL_opts = c(
                         ## byte-compile packages
                         ##"--byte-compile",
                         ## extract and keep examples
                         "--example",
                         ## copy and retain test directory for the package
                         "--install-tests",
                         ## keep line numbers
                         "--with-keep.source",
                         "--no-multiarch"))
}

get_installed_packages <- function() {
    packages <- installed.packages()[, "Package"]
    names(packages) <- NULL
    packages
}

main <- function() {

    args = commandArgs(trailingOnly=TRUE)

    if (length(args) == 0 || length(args) > 1) {
        stop("Expected one argument: path to a file with package names")
    }

    filepath <- args[1]

    required_package_names <- read.csv(filepath,
                                       header = FALSE,
                                       row.names = NULL,
                                       stringsAsFactors = FALSE)

    installed_packages <- get_installed_packages()

    package_names <- unlist(setdiff(required_package_names,
                                    installed_packages))

    is_github_package <- grepl("/", package_names)

    install_from_cran(package_names[!is_github_package])
}

main()
