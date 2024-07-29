##########################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# Edit date: May 19, 2024
# CONTENT:
## - my_glance_data: glance at a specified number of random data
## - my_qq_plot: create histogram with overlaid dnorm curve
## - my_scatter: create scatterplot with lm and loess curve
## - list_plotter: plot color list as a palette
## - save_data_file: save data file
## - pkgs_dl: package downloads
## - t_col: transparent colors
##########################################################



library(glossary)

glossary::glossary_path(here::here("../glossary-pb/glossary.yml"))
glossary::glossary_popup("hover")


##########################################################
# my_glance_data: Glance at a specified number of random data
# Purpose:
# To prevent possible bias with head()/tail() or
# other function that print some data excerpts
# Used in "Statistics with R"
# See: https://bookdown.org/pbaumgartner/swr-harris/
##########################################################

# df   = dataframe or tibble
# N    = number of records chosen randomly
# seed = set.seed for reproducibility

my_glance_data <- function(df, N = 8, seed = 42){
    df_temp <- first_and_last_row(df)

    set.seed(seed)
    df |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        dplyr::relocate(obs) |>
        dplyr::slice_sample(n = N) |>
        dplyr::bind_rows(df_temp) |>
        dplyr::arrange(obs)
}

first_and_last_row <-  function(df) {
    df |>
        dplyr::mutate(obs = dplyr::row_number()) |>
        dplyr::filter(dplyr::row_number() %in% base::c(1, dplyr::n()))
}


################################################################
# save_data_file: Save data file for the specified chapter
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################

save_data_file <- function(chapter_folder, object, file_name){
    data_folder <- base::paste0(here::here(), "/data/")
    if (!base::file.exists(data_folder))
    {base::dir.create(data_folder)}

    chap_folder <-
        base::paste0(
            here::here(),
            paste0("/data/", chapter_folder, "/")
        )
    if (!base::file.exists(chap_folder))
    {base::dir.create(chap_folder)}

    base::saveRDS(object = object,
                  file = paste0(chap_folder, "/", file_name))
}


################################################################
# pkgs_downloads: Get number of downloads from RStudio CRAN Mirror
# Purpose:
# Compare popularity of different packages
# Author: Peter Baumgartner
# pkgs = character vector of package names
# period = "last-day" "last-week", "last-month"
# days: period days = 1, 7, 30
# returns: tibble with packages sorted by download figures
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################
pkgs_dl <-  function(pkgs, period = "last-week", days = 7) {
    dl_pkgs <- cranlogs::cran_downloads(when = period, packages = pkgs)

    start_date = base::min(dl_pkgs$date)
    end_date = base::max(dl_pkgs$date)

    dl_pkgs |>
        dplyr::group_by(package) |>
        dplyr::summarize(average = trunc(sum(count) / days)) |>
        dplyr::arrange(desc(average)) |>
        dplyr::mutate(
            from = start_date,
            to = end_date
        )
}


