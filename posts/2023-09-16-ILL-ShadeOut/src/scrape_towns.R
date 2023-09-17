
links <-
  rvest::read_html("https://www.citipedia.info/province/general/United+Kingdom_Oxfordshire") |>
  rvest::html_elements("a")

get_city_info <- function(x) {
  rvest::read_html(stringr::str_glue("https://www.citipedia.info{x}")) |>
    rvest::html_table() |>
    dplyr::bind_rows()
}

tbl <-
  data.frame(city = rvest::html_text2(links),
           link = rvest::html_attr(links, "href")) |>
  dplyr::filter(grepl("city/", link)) |>
  dplyr::distinct(city, link) |>
  dplyr::arrange(city) |>
  dplyr::mutate(data = purrr::map(link, get_city_info, .progress = TRUE)) |>
  tidyr::unnest(data) |>
  tidyr::pivot_wider(names_from = X1, values_from = X2) |>
  janitor::clean_names() |>
  dplyr::mutate(population = readr::parse_number(population))

readr::write_csv(tbl, "posts/2023-09-16-ILL-ShadeOut/data/towns.csv")
