####################################
# Data Download
####################################
library(here)
library(rvest)
library(stringr)
library(dplyr)
library(assertr)
library(purrr)
library(conflicted)
conflict_prefer("filter", winner = "dplyr")
conflict_prefer("pluck", winner = "purrr")
# 1/3 OD ----------------------------------------------------------------------
if (!dir.exists(here("data-raw/mlit_transport12")))
  dir.create(here("data-raw/mlit_transport12"), recursive = TRUE)
if (!all.equal(list.files(here("data-raw/mlit_transport12/")),
              c("001179095.xlsx", "001179689.xlsx"))) {
  domain_url <- "https://www.mlit.go.jp"
  x <-
    read_html(str_glue("{domain_url}/sogoseisaku/transport/sosei_transport_tk_000035.html"))
  df_list <-
    tibble::tibble(
      url = x %>%
        html_nodes(css = "#contentsColumnWrapL > div:nth-child(2) > table > tbody > tr > td:nth-child(3) > a") %>%
        html_attr("href") %>%
        str_glue("{domain_url}{x}",
                 x = .),
      title = x %>%
        html_nodes(css = "#contentsColumnWrapL > div:nth-child(2) > table > tbody > tr > td:nth-child(2)") %>%
        html_text(trim = TRUE)) %>%
    verify(nrow(.) == 24L)
  df_list %>%
    filter(str_detect(title, "鉄道駅コード|線別駅間移動人員")) %>%
    verify(nrow(.) == 2L) %>%
    pull(url) %>%
    walk(
      ~ download.file(url = .x,
                      destfile = here(str_glue("data-raw/mlit_transport12/{x}",
                                               x = basename(.x))))
    )
  usethis::use_git_ignore("data-raw/mlit_transport12/")
}

# 2/3 Train Railway -----------------------------------------------------------
if (length(list.files(here("data-raw/ksj/"))) != 13) {
  if (!dir.exists(here::here("data-raw/ksj")))
    dir.create(here::here("data-raw/ksj"))
  library(curl)
  domain_url <- "https://nlftp.mlit.go.jp"
  x <-
    curl_fetch_memory(str_glue("{domain_url}/ksj/gml/datalist/KsjTmplt-N02-v2_3.html"),
                      handle = new_handle(ssl_verifypeer = FALSE)) %>%
    pluck("content") %>%
    read_html()
  target_url <-
    x %>%
    html_nodes(css = "#menu-button") %>%
    .[length(.)] %>%
    html_attr("onclick") %>%
    str_split("'", simplify = TRUE) %>%
    str_subset("data.+zip$") %>%
    str_replace(".+/data", "/data") %>%
    str_glue("{domain_url}/ksj/gml{x}",
             x = .)
  target_file <-
    here(str_glue("data-raw/ksj/{x}",
                  x = basename(target_url)))
  curl_download(target_url,
                destfile = target_file,
                handle = new_handle(ssl_verifypeer = FALSE))
  unzip(target_file, exdir = here("data-raw/ksj/"))
  usethis::use_git_ignore("data-raw/ksj")
}

# 3/3 Administration Area -----------------------------------------------------
if (length(list.files(here("data-raw/gm-japan/"), recursive = TRUE)) != 64L) {
  if (!dir.exists(here("data-raw/gm-japan")))
    dir.create(here("data-raw/gm-japan"))
  target_url <-
    "http://www1.gsi.go.jp/geowww/globalmap-gsi/download/data/gm-japan/gm-jpn-all_u_2_2.zip"
  target_file <-
    here(str_glue("data-raw/gm-japan/{x}",
                        x = basename(target_url)))
  download.file(target_url,
                destfile = target_file)
  unzip(target_file, exdir = here("data-raw/gm-japan/"))
  unlink(target_file)
  usethis::use_git_ignore("data-raw/gm-japan/")
}
