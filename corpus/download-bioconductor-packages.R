#! /usr/bin/env Rscript

library(rvest)
library(stringr)
library(purrr)
library(readr)
library(progress)
library(tools)
library(optparse)
library(tibble)
library(methods)
library(htmltidy)
library(httr)

BIOCONDUCTOR_PACKAGE_LIST_PAGE <-
  list("ANNOTATION DATA" = "https://www.bioconductor.org/packages/release/data/annotation/",
       "EXPERIMENT DATA" = "https://www.bioconductor.org/packages/release/data/experiment/",
       "SOFTWARE" = "https://bioconductor.org/packages/release/bioc/")

parse_program_arguments <- function() {

  usage <- str_c("%prog ",
                 "experiment-data-package-dir ",
                 "experiment-data-url-file ",
                 "annotation-data-package-dir ",
                 "annotation-data-url-file ",
                 "software-package-dir ",
                 "software-url-file ",
                 "html-compliance-error-file")

  description <- paste(
    "",
    "experiment-data-package-dir     directory in which experiment data packages will be stored",
    "experiment-data-url-file        file in which experiment data package urls will be stored",
    "annotation-data-package-dir     directory in which annotation data packages will be stored",
    "annotation-data-url-file        file in which annotation data package urls will be stored",
    "software-package-dir            directory in which software packages will be stored",
    "software-url-file               file in which software package urls will be stored",
    "html-compliance-error-file      file in which html compliance errors will be stored",
    sep = "\n")

  option_parser <- OptionParser(usage = usage,
                                description = description,
                                add_help_option = TRUE,
                                option_list = list())

  arguments <- parse_args(option_parser, positional_arguments = 7)

  list("EXPERIMENT DATA" = list(package_source_dirname=arguments$args[1],
                                package_url_filename=arguments$args[2]),
       "ANNOTATION DATA" = list(package_source_dirname=arguments$args[3],
                                package_url_filename=arguments$args[4]),
       "SOFTWARE" = list(package_source_dirname=arguments$args[5],
                         package_url_filename=arguments$args[6]),
       html_compliance_error_filename=arguments$args[7])
}

read_tidy_html <-
  function(url, html_compliance_error_filename) {
    write_lines(str_c("Reading ", url),
                html_compliance_error_filename,
                append=TRUE)
    sink(html_compliance_error_filename, append=TRUE)
    html <-
      url %>%
      GET(user_agent('R')) %>%
      tidy_html(content(., as="text", encoding="UTF-8"),
              list(TidyDocType="html5")) %>%
      read_html()
    sink()
    write_lines(str_c("Finished ", url),
                html_compliance_error_filename,
                append=TRUE)
    html
  }

extract_package_source_url <-
  function(package_url, html_compliance_error_filename) {
    package_url %>%
      read_tidy_html(html_compliance_error_filename) %>%
      html_nodes("td.rpack a") %>%
      html_attr("href") %>%
      str_subset("src/contrib") %>%
      str_c(dirname(package_url), "/", .)
  }

download_package_source <-
  function(package_source_url,
           package_source_dir,
           html_compliance_error_filename) {

    tryCatch({
      download.file(package_source_url,
                    file.path(package_source_dir, basename(package_source_url)),
                    quiet=TRUE)
    }, error = function(e) {
      write_lines(str_c("Error downloading ",
                        package_source_url),
                  html_compliance_error_filename)
    })
  }

extract_package_urls <-
  function(package_list_page, html_compliance_error_filename) {
    package_list_page %>%
      read_tidy_html(html_compliance_error_filename) %>%
      html_nodes(".row_odd a, .row_even a") %>%
      html_attr("href") %>%
      str_c(package_list_page, .)
  }

source_url_to_package_name <-
  function(package_source_url) {
    package_source_url %>%
      basename() %>%
      str_replace("_.*.tar\\.gz","") %>%
      str_trunc(15, "right") %>%
      str_pad(15, "right")
  }


extract_and_download <-
  function(package_list_page,
           package_source_dirname,
           package_url_filename,
           html_compliance_error_filename) {

    cat("Extracting package urls from", package_list_page, "\n")
    package_urls <- extract_package_urls(package_list_page,
                                         html_compliance_error_filename)
    package_url_count <- length(package_urls)
    cat("Extracted", package_url_count, "package urls\n")

    cat("\nCreating directory", package_source_dirname, "\n")
    dir.create(package_source_dirname, showWarnings = FALSE, recursive = TRUE)

    cat("Creating file", package_url_filename, "\n")
    file.create(package_url_filename)

    extraction_progress <-
      progress_bar$new(format = str_c("Extracted source url for :package  ",
                                      "[:bar] :percent (:current/:total)  ",
                                      "ETA: :eta   ELAPSED: :elapsed"),
                       total = package_url_count, clear = FALSE, width = 100)

    cat("\nExtracting package source urls\n")
    package_source_urls <-
      package_urls %>%
      imap_dfr(function(package_url, index) {
        package_source_url <-
          extract_package_source_url(package_url,
                                     html_compliance_error_filename)
        package_name <- source_url_to_package_name(package_source_url)
        extraction_progress$tick(tokens = list(package = package_name,
                                               current = index,
                                               total = package_url_count))
        write_lines(package_source_url,
                    package_url_filename,
                    append = TRUE)
        tibble(package_name, package_source_url, index)
      })

    cat("\nDownloading package sources\n")

    download_progress <-
      progress_bar$new(format = str_c("Downloading source for :package  ",
                                      "[:bar] :percent (:current/:total)  ",
                                      "ETA: :eta   ELAPSED: :elapsed"),
                       total = package_url_count, clear = FALSE, width = 100)

    package_source_urls %>%
      pmap(function(package_name, package_source_url, index) {
        download_progress$tick(tokens = list(package = package_name,
                                             current = index,
                                             total = package_url_count))
        download_package_source(package_source_url,
                                package_source_dirname,
                                html_compliance_error_filename)
        package_name
      })
  }

main <-
  function() {

    settings <- parse_program_arguments()

    divider <- paste0(c("\n", rep("=", 100), "\n"), collapse="", sep="")
    header <- paste0(c("\n", rep("-", 100), "\n\n"), collapse="", sep="")

    html_compliance_error_dirname = dirname(settings$html_compliance_error_filename)
    cat("\nCreating directory", html_compliance_error_dirname, "\n")
    dir.create(html_compliance_error_dirname,
               showWarnings = FALSE,
               recursive = TRUE)

    cat("Creating file", settings$html_compliance_error_filename, "\n")
    file.create(settings$html_compliance_error_filename)


    BIOCONDUCTOR_PACKAGE_LIST_PAGE %>%
    map2(., names(.),
         function(package_list_page, name) {
           cat("\n", name, "PACKAGES")
           cat(header)
           extract_and_download(package_list_page,
                                settings[[name]]$package_source_dirname,
                                settings[[name]]$package_url_filename,
                                settings$html_compliance_error_filename)
           cat(divider)
         })


    cat("\nDone\n")
  }

main()
