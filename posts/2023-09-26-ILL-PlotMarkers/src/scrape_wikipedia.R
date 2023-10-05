
library(rvest)
library(ggplot2)
library(dplyr)

scrape_tbl <- function(url, n, slice) {
  read_html(stringr::str_glue("https://en.wikipedia.org/wiki/{url}_(UK_Parliament_constituency)")) |>
    html_table() |>
    _[[n]] |>
    janitor::clean_names() |>
    slice_head(n = slice) |>
    mutate(constituency = url, .before = everything(),
           votes = readr::parse_number(as.character(votes)),
           percent = readr::parse_number(as.character(percent)))
}

tbl <-
  bind_rows(
    scrape_tbl("Banbury", 6, 4),
    scrape_tbl("Henley", 3, 4),
    scrape_tbl("Oxford_East", 3, 8),
    scrape_tbl("Oxford_West_and_Abingdon", 3, 4),
    scrape_tbl("Wantage", 3, 4),
    scrape_tbl("Witney", 4, 3)
  ) |>
  janitor::remove_constant() |>
  select(constituency, party = party_2, candidate, votes, percent)

readr::write_csv(tbl, "posts/2023-09-26-ILL-PlotMarkers/data/election_results.csv")

tbl |>
  left_join(colours) |>
  mutate(party_2 = forcats::fct_reorder(party_2, percent)) |>
  ggplot(aes(y = "", x = percent, fill = party_2)) +
  geom_col(show.legend = F) +
  coord_polar() +
  expand_limits(y = 0)  +
  scale_fill_manual(values = cols) +
  theme_void() +
  facet_wrap(vars(constituency))
