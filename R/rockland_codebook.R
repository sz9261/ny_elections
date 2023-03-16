names_short <- readLines('R/rockland_elections.R') %>% 
  Filter(f = \(x) str_detect(x, 'Filter'))

short_a <- word(names_short)
patterns <- str_extract(names_short, "'([^']+)'")
fls <- str_extract(names_short, '\\b(el\\w+)\\b')

lookup <- tibble(
  short_name = short_a,
  pattern = str_remove_all(patterns, "'"),
  file_type = fls
) %>% 
  mutate(needs_regex = str_detect(pattern, '\\\\'))

party_candidate_xlsx <- lapply(
  setdiff(dir_ls('data-raw/rockland_elections', glob = '*.xlsx'), 'data-raw/rockland_elections/COUNTYWIDE_PROPS.xlsx'),
  function(x) {
    z <- read_xlsx(x, .name_repair = \(x) vctrs::vec_as_names(x, repair = 'unique', quiet = TRUE)) |> 
      select(-starts_with('...')) %>% 
      slice(1:2)
    z[, which(!is.na(z[1, ]))]  %>% 
      t() %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      as_tibble() %>% 
      setNames(nm = c('office', 'party', 'candidate')) %>% 
      mutate(office = word(office, sep = fixed('...')))
  }) %>%
  bind_rows(.id = 'file') %>% 
  mutate(file = setdiff(dir_ls('data-raw/rockland_elections', glob = '*.xlsx'), 'data-raw/rockland_elections/COUNTYWIDE_PROPS.xlsx')[as.integer(file)])

party_candidate_xls <- lapply(
  dir_ls('data-raw/rockland_elections', glob = '*.xls'), 
  function(x) {
    z <- read_xls(x, .name_repair = \(x) vctrs::vec_as_names(x, repair = 'unique', quiet = TRUE)) |> 
      select(-starts_with('...')) %>% 
      slice(1:2)
    z[, which(!is.na(z[1, ]))]  %>% 
      t() %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      as_tibble() %>% 
      setNames(nm = c('office', 'party', 'candidate')) %>% 
      mutate(office = word(office, sep = fixed('...')))
  }) %>% 
  bind_rows(.id = 'file')


party_candidate_csv <- lapply(
  dir_ls('data-raw/rockland_elections', glob = '*.csv'), 
  function(x) {
    z <- z <- read_csv(x, show_col_types = FALSE) |>
      select(-starts_with('...')) %>% 
      slice(1:2) %>% 
      suppressMessages()
    z[, which(!is.na(z[1, ]))]  %>% 
      t() %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      as_tibble() %>% 
      setNames(nm = c('office', 'party', 'candidate')) %>% 
      mutate(office = word(office, sep = fixed('...')))
  }) %>% 
  bind_rows(.id = 'file')

party_candidate <- bind_rows(party_candidate_xlsx, party_candidate_csv, party_candidate_xls)
noms_all <- bind_rows(noms, noms_csv, noms_xls)
party_candidate <- left_join(party_candidate, noms_all, by = 'file')

party_candidate <- party_candidate %>% 
  left_join(lookup %>% filter(!needs_regex) %>% select(short_name, pattern), by = c(names = 'pattern'))

lookup_nr <- lookup %>% filter(needs_regex) %>% select(short_name, pattern) %>% 
  mutate(pattern = str_replace_all(pattern, '\\\\\\\\', '\\\\'))

for (i in seq_len(nrow(lookup_nr))) {
  m <- str_detect(party_candidate$names, lookup_nr$pattern[i])
  party_candidate$short_name[m] <- lookup_nr$short_name[i]
}

party_candidate <- party_candidate %>% 
  mutate(short_name = case_when(
    !is.na(short_name) ~ short_name,
    str_sub(names, 1, 9) == '14_MEMASS' ~ 'shd_14',
    str_sub(names, 1, 8) == '14_STSEN' ~ 'ssd_14',
    str_sub(names, 1, 5) == '15_LD' ~ 'rcl_15',
    str_sub(names, 1, 5) == '18_LD' ~ 'rcl_18',
    str_sub(names, 1, 5) == '19_LD' ~ 'rcl_19',
    TRUE ~ NA_character_
  ))

party_candidate <- party_candidate %>% 
  mutate(short_name = str_sub(short_name, 1, 6))

rockland_codebook <- party_candidate %>% 
  select(
    source_file = file,
    office = office,
    office_year = short_name,
    party = party,
    candidate = candidate
  ) %>% 
  mutate(
    column_stem = paste0(office_year, '_', tolower(party)),
    office_short_name = str_sub(office_year, 1, 3),
    year = 2000L + as.integer(str_sub(office_year, 5, 6)),
  ) %>% 
  relocate(year, .after = office_year) %>% 
  relocate(office_short_name, .after = office)

write_csv(rockland_codebook, 'data/rockland_codebook.csv')
