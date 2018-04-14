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

BIOCONDUCTOR_PACKAGE_LIST_PAGE <- "https://bioconductor.org/packages/release/bioc/"

parse_program_arguments <- function() {

  usage <- "%prog package-dir url-file"
  description <- paste(
    "",
    "package-dir    directory to which package source will be downloaded",
    "url-file       file in which package source url will be stored",
    sep = "\n")

  option_parser <- OptionParser(usage = usage,
                                description = description,
                                add_help_option = TRUE,
                                option_list = list())

  arguments <- parse_args(option_parser, positional_arguments = 2)

  list(package_source_dirname=arguments$args[1],
       package_url_filename=arguments$args[2])
}


read_tidy_html <-
  function(url) {
    url %>%
      GET(user_agent('R')) %>%
      tidy_html(content(., as="text", encoding="UTF-8"),
              list(TidyDocType="html5")) %>%
      read_html()
  }

extract_package_source_url <-
  function(package_url) {
    package_url %>%
      read_tidy_html() %>%
      html_nodes("td.rpack a") %>%
      html_attr("href") %>%
      str_subset("src/contrib") %>%
      str_c("https://bioconductor.org/packages/release/bioc/html/", .)
  }

download_package_source <-
  function(package_source_url, package_source_dir) {
    download.file(package_source_url,
                  file.path(package_source_dir, basename(package_source_url)),
                  quiet=TRUE)
  }

extract_package_url <-
  function(package_list_page) {
    package_list_page %>%
      read_tidy_html() %>%
      html_nodes(".row_odd a, .row_even a") %>%
      html_attr("href") %>%
      str_c("https://bioconductor.org/packages/release/bioc/", .)
  }

source_url_to_package_name <-
  function(package_source_url) {
    package_source_url %>%
      basename() %>%
      str_replace("_.*.tar\\.gz","") %>%
      str_trunc(20, "right") %>%
      str_pad(20, "right")
  }

main <-
  function() {
    settings <- parse_program_arguments()

    cat("Extracting package urls from", BIOCONDUCTOR_PACKAGE_LIST_PAGE, "\n")
    package_urls <- extract_package_url(BIOCONDUCTOR_PACKAGE_LIST_PAGE)
    package_url_count <- length(package_urls)
    cat("Extracted", package_url_count, "package urls\n")

    cat("\nCreating directory", settings$package_source_dirname, "\n")
    dir.create(settings$package_source_dirname, showWarnings = FALSE)

    cat("Creating file", settings$package_url_filename, "\n")
    file.create(settings$package_url_filename)

    extraction_progress <-
      progress_bar$new(format = "Extracted source url for :package [:bar] :percent ETA: :eta   ELAPSED: :elapsed",
                       total = package_url_count, clear = FALSE, width = 100)

    cat("\nExtracting package source urls\n")
    package_source_urls <-
      package_urls %>%
      map_dfr(function(package_url) {
        package_source_url <- extract_package_source_url(package_url)
        package_name <- source_url_to_package_name(package_source_url)
        extraction_progress$tick(tokens = list(package = package_name))
        write_lines(str_c(package_source_url, "\n"),
                    settings$package_url_filename,
                    append = TRUE)
        tibble(package_name, package_source_url)
      })

    cat("\nDownloading package sources\n")

    download_progress <-
      progress_bar$new(format = "Downloading source for :package [:bar] :percent ETA: :eta   ELAPSED: :elapsed",
                       total = package_url_count, clear = FALSE, width = 100)

    package_source_urls %>%
      pmap(function(package_name, package_source_url) {
        download_progress$tick(tokens = list(package = package_name))
        download_package_source(package_source_url, settings$package_source_dirname)
        package_name
      })

    cat("\nDone\n")
  }

main()
