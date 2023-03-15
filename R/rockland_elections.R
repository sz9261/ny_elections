# read in full data ----
el_l <- lapply(
  dir_ls('data-raw/rockland_elections', glob = '*.xlsx'), 
  function(x) {
    z <- read_xlsx(x, .name_repair = \(x) vctrs::vec_as_names(x, repair = 'unique', quiet = TRUE)) |> 
      select(-starts_with('...'))
    
    noms <- paste(
      names(z),
      sapply(z |> slice(1:2), \(y) paste0(na.omit(y), collapse = '_')),
      sep = '_'
    ) |> 
      str_remove_all('\\.{3}\\d+')
    
    z |> 
      slice(-c(1:2)) |> 
      setNames(noms) |> 
      mutate(file = x)
  }
)

noms <- names(el_l) |> 
  path_file() |> 
  path_ext_remove() |> 
  tibble(names = _) |> 
  mutate(
    names = ifelse(str_detect(names, 'COUNTYWIDE'), paste0('21', names), names),
    names = ifelse(str_detect(names, 'COUNTWIDE'), paste0('21', names), names),
    names = str_remove(names, 'GNYROCK'),
    names = str_remove(names, 'COUNTYWIDE'),
    names = str_remove(names, 'COUNTWIDE'),
    names = str_remove(names, '_GE2014'),
    names = str_remove(names, 'GENYRO'),
    names = case_when(
      str_starts(names, '0') ~ paste0('2', names),
      str_starts(names, '2') ~ names,
      TRUE ~ paste0('1', names)
    )
  ) %>% 
  bind_cols(tibble(file = names(el_l)))
names(el_l) <- noms$names

el_l <- lapply(el_l, \(x) left_join(x, noms, by = 'file'))


# 14 atg ----
atg_14 <- Filter(\(x) x$names[1] == '14_ATTGEN', el_l)[[1]]
atg_14 <- atg_14 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'attorney_general_', 'atg_14_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('atg_')) %>% 
  rename(precinct = district, atg_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2014L, office = 'Attorney General', district = NA_integer_,
         across(starts_with('atg'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, atg_total, starts_with('atg')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 Comptroller ----
com_14 <- Filter(\(x) x$names[1] == '14_COMPT', el_l)[[1]]
com_14 <- com_14 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'comptroller_', 'com_14_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('com_')) %>% 
  rename(precinct = district, com_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2014L, office = 'Comptroller', district = NA_integer_,
         across(starts_with('com'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, com_total, starts_with('com')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 County Court Judge ----
jud_14 <- Filter(\(x) x$names[1] == '14_CTYCRTJUD', el_l)[[1]]
jud_14 <- jud_14 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_court_judge_', 'jud_14_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('jud_')) %>% 
  rename(precinct = district, jud_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2014L, office = 'County Court Judge', district = NA_integer_,
         across(starts_with('jud'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, jud_total, starts_with('jud')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 CTYLEG ----
ld_14 <- Filter(\(x) x$names[1] == '14_CTYLEG', el_l)[[1]]

ld_14 <- ld_14 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_legislator_5th_legislative_district_', 'rcl_14_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('rcl_')) %>% 
  rename(precinct = district, rcl_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2014L, office = 'Rockland County Legislature', district = 5L,
         across(starts_with('rcl'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, rcl_total, starts_with('rcl')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 SHD ----
shd_14 <- Filter(\(x) str_sub(x$names[1], 1, 9) == '14_MEMASS', el_l)
shd_14 <- lapply(shd_14, \(x) x %>% 
                  janitor::clean_names() %>% 
                  rename_with(.fn = \(x) str_replace(x, 'member_of_assembly_\\d+th_assembly_district_', 'shd_14_')) %>% 
                  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shd_')) %>% 
                  rename(precinct = district, shd_total = ballots_cast_total_ballots_cast))
shd_14 <- bind_rows(shd_14) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2014L, office = 'NY Assembly', district = as.integer(str_extract(names, '\\d+$')),
         across(starts_with('shd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shd_total, starts_with('shd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 US Congress ---
ush_14 <- Filter(\(x) x$names[1] == '14_REP_CONG', el_l)[[1]]
ush_14 <- ush_14 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_17th_congressional_district_', 'ush_14_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) %>% 
  rename(precinct = district, ush_total = ballots_cast_total_ballots_cast) %>%  
  mutate(year = 2014L, office = 'US House', district = 97L,
         across(starts_with('ush'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ush_total, starts_with('ush')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 SSD ----
ssd_14 <- Filter(\(x) str_sub(x$names[1], 1, 8) == '14_STSEN', el_l)
ssd_14 <- lapply(ssd_14, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'state_senator_\\d+th_senatorial_district_', 'ssd_14_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ssd_')) %>% 
                   rename(precinct = district, ssd_total = ballots_cast_total_ballots_cast))
ssd_14 <- bind_rows(ssd_14) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2014L, office = 'NY Senate', district = as.integer(str_extract(names, '\\d+$')),
         across(starts_with('ssd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ssd_total, starts_with('ssd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 14 Supreme Court ----
spc_14 <- Filter(\(x) x$names[1] == '14_SUPCRT', el_l)[[1]]
spc_14 <- spc_14 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'supreme_court_justice_9th_judicial_district_', 'spc_14_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('spc_')) %>% 
  rename(precinct = district, spc_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2014L, office = 'Supreme Court', district = NA_integer_,
         across(starts_with('spc'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )



# 15 District Attorney ----
dat_15 <- Filter(\(x) x$names[1] == '15_DISTRICT_ATTORNEY', el_l)[[1]]
dat_15 <- dat_15 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'district_attorney_', 'dat_15_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('dat_')) %>% 
  rename(precinct = district, dat_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%   
  mutate(year = 2015L, office = 'District Attorney', district = NA_integer_,
         across(starts_with('dat'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, dat_total, starts_with('dat')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 15 Family Court ----
fam_15 <- Filter(\(x) x$names[1] == '15_FAM_COURT_JUDGE', el_l)[[1]]
fam_15 <- fam_15 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'family_court_judge_', 'fam_15_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('fam_')) %>% 
  rename(precinct = district, fam_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%   
  mutate(year = 2015L, office = 'Family Court Judge', district = NA_integer_,
         across(starts_with('fam'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, fam_total, starts_with('fam')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 15 LD 5 ----
ld_15 <- Filter(\(x) str_sub(x$names[1], 1, 5) == '15_LD', el_l)
ld_15 <- lapply(ld_15, \(x) x %>% 
                  janitor::clean_names() %>% 
                  rename_with(.fn = \(x) str_replace(x, 'county_legislator_legislative_district_\\d+_', 'rcl_15_')) %>% 
                  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('rcl_')) %>% 
                  rename(precinct = district, rcl_total = ballots_cast_total_ballots_cast))
ld_15 <- bind_rows(ld_15) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2015L, office = 'Rockland County Legislature', district = as.integer(str_extract(names, '\\d+$')),
         across(starts_with('rcl'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, rcl_total, starts_with('rcl')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 15 Sheriff ----
shf_15 <- Filter(\(x) x$names[1] == '15_SHERIFF', el_l)[[1]]
shf_15 <- shf_15 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'sheriff_', 'shf_15_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shf_')) %>% 
  rename(precinct = district, shf_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2015L, office = 'Sherrif', district = NA_integer_,
         across(starts_with('shf'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shf_total, starts_with('shf')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 15 Supreme Court Justice ----
spc_15 <- Filter(\(x) x$names[1] == '15_SUP_COURT_JUST', el_l)[[1]]
spc_15 <- spc_15 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'supreme_court_justice_9th_judicial_district_', 'spc_15_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('spc_')) %>% 
  rename(precinct = district, spc_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2015L, office = 'Supreme Court', district = NA_integer_,
         across(starts_with('spc'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 16 SSD ----
ssd_16 <- Filter(\(x) str_detect(x$names[1], '16_\\d+TH_SENATE'), el_l)
ssd_16 <- lapply(ssd_16, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'state_senator_\\d+th_senatorial_district_', 'ssd_16_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ssd_')) %>% 
                   rename(precinct = district, ssd_total = ballots_cast_total_ballots_cast))
ssd_16 <- bind_rows(ssd_16) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2016L, office = 'NY Senate', district = as.integer(str_extract(names, '\\d+$')),
         across(starts_with('ssd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ssd_total, starts_with('ssd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 16 SHD ----
shd_16 <- Filter(\(x) str_detect(x$names[1], '16_\\d+TH_ASSEM'), el_l)
shd_16 <- lapply(shd_16, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'member_of_assembly_\\d+th_assembly_district_', 'shd_16_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shd_')) %>% 
                   rename(precinct = district, shd_total = ballots_cast_total_ballots_cast))
shd_16 <- bind_rows(shd_16) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2016L, office = 'NY Assembly', district = as.integer(str_extract(str_extract(names, '\\d+TH_ASSEM$'), '\\d+')),
         across(starts_with('shd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shd_total, starts_with('shd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )


# 16 County Court Justice ----
jud_16 <- Filter(\(x) x$names[1] == '16_COUNTY_COURT_JUST', el_l)[[1]]
jud_16 <- jud_16 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_court_justice_', 'jud_16_')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('jud_')) %>% 
  rename(precinct = district, jud_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS')%>%  
  mutate(year = 2016L, office = 'County Court Judge', district = NA_integer_,
         across(starts_with('jud'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, jud_total, starts_with('jud')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

jud_16 <- jud_16 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = c(jud_16_wri_in, jud_16_ove_vot, jud_16_und_vot)
  )

# 16 president ----
pre_16 <- Filter(\(x) x$names[1] == '16_PRESIDENT', el_l)[[1]]
pre_16 <- pre_16 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'presidential_electors_for_president_and_vice_president_', 'pre_16_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('pre_')) %>% 
  rename(precinct = district, pre_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2016L, office = 'President', district = NA_integer_,
         across(starts_with('pre_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, pre_total, starts_with('pre')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 16 US Congress ---
ush_16 <- Filter(\(x) x$names[1] == '16_REP_CONGRESS', el_l)[[1]]
ush_16 <- ush_16 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_', 'ush_16_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) %>% 
  rename(precinct = district, ush_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2016L, office = 'US House', district = 97L,
         across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ush_total, starts_with('ush')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 16 Supreme Court Justice ---
spc_16 <- Filter(\(x) x$names[1] == '16_SSCJ', el_l)[[1]]
spc_16 <- spc_16 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'justice_of_the_supreme_court_9th_judicial_district_', 'spc_16_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, -4), .cols = ends_with('_ii')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('spc_')) %>% 
  rename(precinct = district, spc_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2016L, office = 'US House', district = 97L,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

spc_16 <- spc_16 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = ends_with(c('_wri_in', '_ove_vot', '_und_vot'))
  )

# 16 USS ----
uss_16 <- Filter(\(x) x$names[1] == '16_US_SENATOR', el_l)[[1]]
uss_16 <- uss_16 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'united_states_senator_', 'uss_16_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('uss_')) %>% 
  rename(precinct = district, uss_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2016L, office = 'US Senate', district = NA_integer_,
         across(starts_with('uss_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, uss_total, starts_with('uss')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 17 County Clerk ----
clk_17 <- Filter(\(x) x$names[1] == '17_COUNTY_CLK', el_l)[[1]]
clk_17 <- clk_17 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_clerk_', 'clk_17_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('clk_')) %>% 
  rename(precinct = district_name, clk_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2017L, office = 'County Clerk', district = NA_integer_,
         across(starts_with('clk_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, clk_total, starts_with('clk')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )


# 17 County Executive ----
exe_17 <- Filter(\(x) x$names[1] == '17_COUNTY_EXEC', el_l)[[1]]
exe_17 <- exe_17 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_executive_', 'exe_17_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('exe_')) %>% 
  rename(precinct = district_name, exe_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2017L, office = 'County Executive', district = NA_integer_,
         across(starts_with('exe_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, exe_total, starts_with('exe')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 17 Supreme Court Justice ----
spc_17 <- Filter(\(x) x$names[1] == '17_SUP_CRT_JUST', el_l)[[1]]
spc_17 <- spc_17 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'supreme_court_justice_9th_judicial_district_', 'spc_17_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('spc_')) %>% 
  rename(precinct = district_name, spc_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2017L, office = 'Supreme Court Justice', district = NA_integer_,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 17 Surrogate Judge ----
srj_17 <- Filter(\(x) x$names[1] == '17_SUR_JUDGE', el_l)[[1]]
srj_17 <- srj_17 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'surrogate_judge_', 'srj_17_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('srj_')) %>% 
  rename(precinct = district_name, srj_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2017L, office = 'Surrogate Judge', district = NA_integer_,
         across(starts_with('srj_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, srj_total, starts_with('srj')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 SSD ----
ssd_18 <- Filter(\(x) str_detect(x$names[1], '18_\\d+TH_SENATORIAL'), el_l)
ssd_18 <- lapply(ssd_18, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'state_senator_\\d+th_senatorial_district_', 'ssd_18_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ssd_')) %>% 
                   rename(precinct = district_name, ssd_total = ballots_cast_total_ballots_cast))
ssd_18 <- bind_rows(ssd_18) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'NY Senate', district = as.integer(str_extract(str_extract(names, '\\d+TH_SENATORIAL$'), '\\d+')),
         across(starts_with('ssd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ssd_total, starts_with('ssd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 SHD ----
shd_18 <- Filter(\(x) str_detect(x$names[1], '18_\\d+TH_ASM'), el_l)
shd_18 <- lapply(shd_18, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'member_of_assembly_\\d+th_assembly_district_', 'shd_18_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shd_')) %>% 
                   rename(precinct = district_name, shd_total = ballots_cast_total_ballots_cast))
shd_18 <- bind_rows(shd_18) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'NY Assembly', district = as.integer(str_extract(str_extract(names, '\\d+TH_ASM$'), '\\d+')),
         across(starts_with('shd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shd_total, starts_with('shd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 Attorney General ----
atg_18 <- Filter(\(x) x$names[1] == '18_ATT_GEN', el_l)[[1]]
atg_18 <- atg_18 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'attorney_general_', 'atg_18_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('atg_')) %>% 
  rename(precinct = district_name, atg_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'Attorney General', district = NA_integer_,
         across(starts_with('atg_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, atg_total, starts_with('atg')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 Comptroller ----
com_18 <- Filter(\(x) x$names[1] == '18_COMPTROLLER', el_l)[[1]]
com_18 <- com_18 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'comptroller_', 'com_18_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('com_')) %>% 
  rename(precinct = district_name, com_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'Comptroller', district = NA_integer_,
         across(starts_with('com_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, com_total, starts_with('com')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 LD 4 ----
ld_18 <- Filter(\(x) str_sub(x$names[1], 1, 5) == '18_LD', el_l)
ld_18 <- lapply(ld_18, \(x) x %>% 
                  janitor::clean_names() %>% 
                  rename_with(.fn = \(x) str_replace(x, 'county_legislator_legislative_district_\\d+_', 'rcl_18_')) %>% 
                  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('rcl_')) %>% 
                  rename(precinct = district_name, rcl_total = ballots_cast_total_ballots_cast))
ld_18 <- bind_rows(ld_18) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'Rockland County Legislature', district = as.integer(str_extract(names, '\\d+$')),
         across(starts_with('rcl'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, rcl_total, starts_with('rcl')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 Governor ----
gov_18 <- Filter(\(x) x$names[1] == '18_GOVERNOR', el_l)[[1]]
gov_18 <- gov_18 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'governor_and_lieutenant_governor_', 'gov_18_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('gov_')) %>% 
  rename(precinct = district, gov_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'Governor', district = NA_integer_,
         across(starts_with('gov_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, gov_total, starts_with('gov')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 US Congress ---
ush_18 <- Filter(\(x) x$names[1] == '18_REP_CONG', el_l)[[1]]
ush_18 <- ush_18 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_', 'ush_18_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) %>% 
  rename(precinct = district_name, ush_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2018L, office = 'US House', district = 97L,
         across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ush_total, starts_with('ush')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 18 Supreme Court ----
spc_18 <- Filter(\(x) x$names[1] == '18_SUPREME_COURT', el_l)[[1]]
spc_18 <- spc_18 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'justice_of_the_supreme_court_9th_judicial_district_', 'spc_18_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, -4), .cols = ends_with('_jr')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('spc_')) %>% 
  rename(precinct = district_name, spc_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2018L, office = 'Supreme Court Justice', district = NA_integer_,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

spc_18 <- spc_18 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = ends_with(c('_wri_in', '_ove_vot', '_und_vot'))
  )

# 18 USS ----
uss_18 <- Filter(\(x) x$names[1] == '18_US_SENATOR', el_l)[[1]]
uss_18 <- uss_18 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'united_states_senator_', 'uss_18_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('uss_')) %>% 
  rename(precinct = district_name, uss_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2018L, office = 'US Senate', district = NA_integer_,
         across(starts_with('uss_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, uss_total, starts_with('uss')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )


# 19 LD ----
ld_19 <- Filter(\(x) str_sub(x$names[1], 1, 5) == '19_LD', el_l)
ld_19 <- lapply(ld_19, \(x) x %>% 
                  janitor::clean_names() %>% 
                  rename_with(.fn = \(x) str_replace(x, 'county_legislator_legislative_district_\\d+_', 'rcl_19_')) %>% 
                  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('rcl_')) %>% 
                  rename(precinct = district, rcl_total = ballots_cast_total_ballots_cast))
ld_19 <- bind_rows(ld_19) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2019L, office = 'Rockland County Legislature', district = as.integer(str_extract(names, '\\d+$')),
         across(starts_with('rcl'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, rcl_total, starts_with('rcl')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 19 District Attorney ----
dat_19 <- Filter(\(x) x$names[1] == '19_DA', el_l)[[1]]
dat_19 <- dat_19 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'district_attorney_', 'dat_19_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('dat_')) %>% 
  rename(precinct = district_name, dat_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%   
  mutate(year = 2019L, office = 'District Attorney', district = NA_integer_,
         across(starts_with('dat'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, dat_total, starts_with('dat')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 19 Supreme Court ----
spc_19 <- Filter(\(x) x$names[1] == '19_JUSTICE_SC', el_l)[[1]]
spc_19 <- spc_19 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'justice_of_the_supreme_court_9th_judicial_district_', 'spc_19_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, -4), .cols = ends_with('_jr')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('spc_')) %>% 
  rename(precinct = district_name, spc_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2019L, office = 'Supreme Court Justice', district = NA_integer_,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )
spc_19 <- spc_19 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = ends_with(c('_wri_in', '_ove_vot', '_und_vot'))
  )

# 19 Sheriff ----
shf_19 <- Filter(\(x) x$names[1] == '19_SHERIFF', el_l)[[1]]
shf_19 <- shf_19 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'sheriff_', 'shf_19_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shf_')) %>% 
  rename(precinct = district_name, shf_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2019L, office = 'Sherrif', district = NA_integer_,
         across(starts_with('shf'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shf_total, starts_with('shf')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 20 SSD ----
ssd_20 <- Filter(\(x) str_detect(x$names[1], '20_\\d+TH_SENATE'), el_l)
ssd_20 <- lapply(ssd_20, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'state_senator_\\d+th_senatorial_district_', 'ssd_20_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ssd_')) %>% 
                   rename(precinct = district, ssd_total = ballots_cast_total_ballots_cast))
ssd_20 <- bind_rows(ssd_20) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2020L, office = 'NY Senate', district = as.integer(str_extract(str_extract(names, '\\d+TH_SENATE$'), '\\d+')),
         across(starts_with('ssd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ssd_total, starts_with('ssd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 20 SHD ----
shd_20 <- Filter(\(x) str_detect(x$names[1], '20_\\d+TH_ASSEMBLY'), el_l)
shd_20 <- lapply(shd_20, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'member_of_assembly_\\d+th_assembly_district_', 'shd_20_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shd_')) %>% 
                   rename(precinct = district, shd_total = ballots_cast_total_ballots_cast))
shd_20 <- bind_rows(shd_20) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2020L, office = 'NY Assembly', district = as.integer(str_extract(str_extract(names, '\\d+TH_ASSEMBLY$'), '\\d+')),
         across(starts_with('shd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shd_total, starts_with('shd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 20 County Clerk ----
clk_20 <- Filter(\(x) x$names[1] == '20_COUNTY_CLERK', el_l)[[1]]
clk_20 <- clk_20 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_clerk_', 'clk_20_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('clk_')) %>% 
  rename(precinct = district, clk_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2020L, office = 'County Clerk', district = NA_integer_,
         across(starts_with('clk_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, clk_total, starts_with('clk')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )    

# 20 President ----
pre_20 <- Filter(\(x) x$names[1] == '20_PRESIDENT', el_l)[[1]]
pre_20 <- pre_20 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'presidential_electors_for_president_and_vice_president_', 'pre_20_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('pre_')) %>% 
  rename(precinct = district, pre_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2020L, office = 'President', district = NA_integer_,
         across(starts_with('pre_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, pre_total, starts_with('pre')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 20 US Congress ---
ush_20 <- Filter(\(x) x$names[1] == '20_REP_IN_CONGRESS', el_l)[[1]]
ush_20 <- ush_20 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_', 'ush_20_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) %>% 
  rename(precinct = district, ush_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2020L, office = 'US House', district = 97L,
         across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, ush_total, starts_with('ush')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 20 Supreme Court ----
spc_20 <- Filter(\(x) x$names[1] == '20_SUPREME_COURT', el_l)[[1]]
spc_20 <- spc_20 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'justice_of_the_supreme_court_9th_judicial_district_', 'spc_20_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, -4), .cols = ends_with('_jr')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('spc_')) %>% 
  rename(precinct = district, spc_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2020L, office = 'Supreme Court Justice', district = NA_integer_,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )
spc_20 <- spc_20 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = ends_with(c('_wri_in', '_ove_vot', '_und_vot'))
  )

# 21 County Executive ----
exe_21 <- Filter(\(x) x$names[1] == '21_COUNTY_EXE', el_l)[[1]]
exe_21 <- exe_21 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'county_executive_', 'exe_21_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('exe_')) %>% 
  rename(precinct = precinct_name, exe_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2021L, office = 'County Executive', district = NA_integer_,
         across(starts_with('exe_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, exe_total, starts_with('exe')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 21 Family Court ----
fam_21 <- Filter(\(x) x$names[1] == '21_FAMILY_COURT_JUDGE', el_l)[[1]]
fam_21 <- fam_21 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'family_court_judge_', 'fam_21_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('fam_')) %>% 
  rename(precinct = precinct_name, fam_total = ballots_cast_total_ballots_cast) %>% 
  filter(precinct != 'COUNTY TOTALS') %>%   
  mutate(year = 2021L, office = 'Family Court Judge', district = NA_integer_,
         across(starts_with('fam'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, fam_total, starts_with('fam')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 21 Supreme Court ----
spc_21 <- Filter(\(x) x$names[1] == '21_JUSTICE_OF_SUPREME_COURT', el_l)[[1]]
spc_21 <- spc_21 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'justice_of_the_supreme_court_9th_judicial_district_', 'spc_21_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, -4), .cols = ends_with('_jr')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('spc_')) %>% 
  rename(precinct = precinct_name, spc_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2021L, office = 'Supreme Court Justice', district = NA_integer_,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )
spc_21 <- spc_21 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = ends_with(c('_wri_in', '_ove_vot', '_und_vot'))
  )

# 21 Propositions ----
prop_21 <- Filter(\(x) x$names[1] == '21_PROPS', el_l)[[1]] %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) x %>% 
                str_replace('proposal_one_an_amendment_',   'pr1_21_') %>% 
                str_replace('proposal_two_an_amendment_',   'pr2_21_') %>% 
                str_replace('proposal_three_an_amendment_', 'pr3_21_') %>% 
                str_replace('proposal_four_an_amendment_',  'pr4_21_') %>% 
                str_replace('proposal_five_an_amendment_',  'pr5_21_') %>% 
                str_replace('_over_votes',  '_ove') %>% 
                str_replace('_under_votes',  '_und')
                ) %>% 
  rename(precinct = precinct_name, pr_total = ballots_cast_total_ballots_cast) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2021L, office = 'Propositions', district = NA_integer_,
         across(starts_with('pr\\d_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, matches('pr\\d')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 22 SHD (excel) ---
shd_22_a <- Filter(\(x) str_detect(x$names[1], '22_\\d+th_Assembly_District'), el_l)
shd_22_a <- lapply(shd_22_a, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'member_of_assembly_\\d+th_assembly_district_', 'shd_22_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shd_')) %>% 
                   rename(precinct = precinct_name, shd_total = ballots_cast_total_ballots_cast))
shd_22_a <- bind_rows(shd_22_a) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2022L, office = 'NY Assembly', district = as.integer(str_extract(str_extract(names, '\\d+th_Assembly_District$'), '\\d+')),
         across(starts_with('shd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  select(precinct, office, district, year, shd_total, starts_with('shd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# CSV Files ----
el_csv <- lapply(
  dir_ls('data-raw/rockland_elections', glob = '*.csv'), 
  function(x) {
    z <- read_csv(x) |>#, .name_repair = \(x) vctrs::vec_as_names(x, repair = 'unique', quiet = TRUE)) |> 
      select(-starts_with('...'))
    
    noms <- paste(
      names(z),
      sapply(z |> slice(1:2), \(y) paste0(na.omit(y), collapse = '_')),
      sep = '_'
    ) |> 
      str_remove_all('\\.{3}\\d+')
    
    z |> 
      slice(-c(1:2)) |> 
      setNames(noms) |> 
      mutate(file = x)
  }
)

noms_csv <- names(el_csv) |> 
  path_file() |> 
  path_ext_remove() |> 
  tibble(names = _) |> 
  mutate(
    names = ifelse(str_detect(names, 'COUNTYWIDE'), paste0('21', names), names),
    names = ifelse(str_detect(names, 'COUNTWIDE'), paste0('21', names), names),
    names = str_remove(names, 'GNYROCK'),
    names = str_remove(names, 'COUNTYWIDE'),
    names = str_remove(names, 'COUNTWIDE'),
    names = str_remove(names, '_GE2014'),
    names = str_remove(names, 'GENYRO'),
    names = case_when(
      str_starts(names, '0') ~ paste0('2', names),
      str_starts(names, '2') ~ names,
      TRUE ~ paste0('1', names)
    )
  ) %>% 
  bind_cols(tibble(file = names(el_csv)))
names(el_csv) <- noms_csv$names
el_csv <- lapply(el_csv, \(x) left_join(x, noms_csv, by = 'file'))

# 22 SHD (csv) ---
shd_22_b <- Filter(\(x) str_detect(x$names[1], '22_\\d+TH_ASSEM'), el_csv)
shd_22_b <- lapply(shd_22_b, \(x) x %>% 
                     janitor::clean_names() %>% 
                     rename_with(.fn = \(x) str_replace(x, 'member_of_assembly_\\d+th_assembly_district_', 'shd_22_')) %>% 
                     rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('shd_')) %>% 
                     rename(precinct = precinct_name))
shd_22_b <- bind_rows(shd_22_b) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(
    year = 2022L, office = 'NY Assembly', district = as.integer(str_extract(str_extract(names, '\\d+TH_ASSEM$'), '\\d+')),
    across(starts_with('shd'), \(x) replace_na(as.integer(x), 0L))
  ) %>% 
  mutate(shd_total = sum(c_across(starts_with('shd_'))), .by = precinct) %>% 
  select(precinct, office, district, year, shd_total, starts_with('shd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

shd_22 <- bind_rows(shd_22_a, shd_22_b)

# 22 SSD ---
ssd_22 <- Filter(\(x) str_detect(x$names[1], '22_\\d+TH_SEN'), el_csv)
ssd_22 <- lapply(ssd_22, \(x) x %>% 
                   janitor::clean_names() %>% 
                   rename_with(.fn = \(x) str_replace(x, 'state_senator_\\d+th_senatorial_district_', 'ssd_22_')) %>% 
                   rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ssd_')) %>% 
                   rename(precinct = precinct_name))
ssd_22 <- bind_rows(ssd_22) %>% 
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2022L, office = 'NY Senate', district = as.integer(str_extract(str_extract(names, '\\d+TH_SEN$'), '\\d+')),
         across(starts_with('ssd'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(ssd_total = sum(c_across(starts_with('ssd_'))), .by = precinct) %>% 
  select(precinct, office, district, year, ssd_total, starts_with('ssd')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 22 US House ----
ush_22 <- Filter(\(x) x$names[1] == '22_17TH_CONGRESS', el_csv)[[1]]
ush_22 <- ush_22 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'representative_in_congress_17th_congressional_district_', 'ush_22_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('ush_')) %>% 
  rename(precinct = precinct_name) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2022L, office = 'US House', district = 17L,
         across(starts_with('ush_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(ush_total = sum(c_across(starts_with('ush_'))), .by = precinct) %>% 
  select(precinct, office, district, year, ush_total, starts_with('ush')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 22 Attorney General ----
atg_22 <- Filter(\(x) x$names[1] == '22_ATT_GEN', el_csv)[[1]]
atg_22 <- atg_22 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'attorney_general_', 'atg_22_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('atg_')) %>% 
  rename(precinct = precinct_name) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2022L, office = 'Attorney General', district = NA_integer_,
         across(starts_with('atg_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(atg_total = sum(c_across(starts_with('atg_'))), .by = precinct) %>% 
  select(precinct, office, district, year, atg_total, starts_with('atg')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 22 Comptroller ----
com_22 <- Filter(\(x) x$names[1] == '22_COMPTROLLER', el_csv)[[1]]
com_22 <- com_22 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'comptroller_', 'com_22_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('com_')) %>% 
  rename(precinct = precinct_name) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2022L, office = 'Comptroller', district = NA_integer_,
         across(starts_with('com_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(com_total = sum(c_across(starts_with('com_'))), .by = precinct) %>% 
  select(precinct, office, district, year, com_total, starts_with('com')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 22 Governor ----
gov_22 <- Filter(\(x) x$names[1] == '22_GOV_LT_GOV', el_csv)[[1]]
gov_22 <- gov_22 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'governor_and_lieutenant_governor_', 'gov_22_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('gov_')) %>% 
  rename(precinct = precinct_name) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2022L, office = 'Governor', district = NA_integer_,
         across(starts_with('gov_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(gov_total = sum(c_across(starts_with('gov_'))), .by = precinct) %>% 
  select(precinct, office, district, year, gov_total, starts_with('gov')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# 22 Supreme Court ----
spc_22 <- Filter(\(x) x$names[1] == '22_JUS_SUPREME', el_csv)[[1]]
spc_22 <- spc_22 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'justices_of_the_supreme_court_9th_judicial_district_', 'spc_22_')) %>%
  rename_with(.fn = \(x) str_sub(x, 1, -4), .cols = ends_with('_jr')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, -5), .cols = ends_with('_iii')) %>% 
  rename_with(.fn = \(x) paste0(str_sub(x, 1, 10), '_', str_sub(word(x, start = -1, sep = '_'), 1, 3)), .cols = starts_with('spc_')) %>% 
  rename(precinct = precinct_name) %>%  
  filter(precinct != 'COUNTY TOTALS') %>% 
  mutate(year = 2022L, office = 'Supreme Court Justice', district = NA_integer_,
         across(starts_with('spc_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(spc_total = sum(c_across(starts_with('spc_'))), .by = precinct) %>% 
  select(precinct, office, district, year, spc_total, starts_with('spc')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )
spc_22 <- spc_22 %>% 
  rename_with(
    .fn = \(x) str_sub(x, 1, 10),
    .cols = ends_with(c('_wri_in', '_ove_vot', '_und_vot'))
  )

# 22 US Senate ----
uss_22 <- Filter(\(x) x$names[1] == '22_US_SEN', el_csv)[[1]]
uss_22 <- uss_22 %>% 
  janitor::clean_names() %>% 
  rename_with(.fn = \(x) str_replace(x, 'united_states_senator_', 'uss_22_')) %>% 
  rename_with(.fn = \(x) str_sub(x, 1, 10), .cols = starts_with('uss_')) %>% 
  rename(precinct = precinct_name) %>%  
  filter(precinct != 'COUNTY TOTALS') %>%  
  mutate(year = 2022L, office = 'US Senate', district = NA_integer_,
         across(starts_with('uss_'), \(x) replace_na(as.integer(x), 0L))) %>% 
  mutate(uss_total = sum(c_across(starts_with('uss_'))), .by = precinct) %>% 
  select(precinct, office, district, year, uss_total, starts_with('uss')) %>% 
  mutate(
    precinct = paste0(
      str_sub(precinct, 1, 1), 
      str_pad(str_extract(precinct, '\\d+'), side = 'left', pad = '0', width = 2)
    )
  )

# bring it all together ----
d_12_l <- lst(
  atg_14, atg_18,  clk_17, clk_20, 
  com_14, com_18, dat_15, dat_19,
  exe_17, fam_15, gov_18, jud_14, 
  jud_16, ld_14, ld_15, ld_18, ld_19,
  pre_16, pre_20, shd_14, shd_16, shd_18, 
  shd_20, shf_15, shf_19, 
  spc_14, spc_15, spc_16, spc_17, spc_18, spc_19, spc_20, 
  srj_17, ssd_14, ssd_16, ssd_18, ssd_20, 
  ush_14, ush_16, ush_18, ush_20, uss_16, 
  uss_18
)

d_21_l <- lst(
  exe_21, fam_21, prop_21, spc_21,
)

d_22_l <- lst(
  atg_22, com_22, gov_22, shd_22, ssd_22, spc_22, ush_22, uss_22
)

# read in eds ----
ed_16 <- read_sf(here('data-raw/rockland_gis/Cty_ED_Jan_15_2016.shp')) %>% 
  rename(
    town = TOWN,
    ed_num = ED_NUM,
    precinct = ED_KEY
  )
ed_21 <- read_sf(here('data-raw/rockland_gis/Cty_ED_Mar_5_2021.shp')) %>% 
  rename(
    town = TOWN,
    ed_num = ED_NUM,
    precinct = ED_KEY
  ) %>% 
  select(-starts_with('Shape'))
ed_22 <- read_sf(here('data-raw/rockland_gis/Cty_ED_Feb_16_2022.shp')) %>% 
  rename(
    town = TOWN,
    ed_num = ED_NUM,
    precinct = ED_KEY
  ) %>% 
  select(-starts_with('Shape'))

d_12 <- map(d_12_l, \(x) x %>% select(-office, -district, -year, -ends_with('_total'))) %>% 
  purrr::reduce(left_join, by = 'precinct')
d_21 <- map(d_21_l, \(x) x %>% select(-office, -district, -year, -ends_with('_total'))) %>% 
  purrr::reduce(left_join, by = 'precinct')
d_22 <- map(d_22_l, \(x) x %>% select(-office, -district, -year, -ends_with('_total'))) %>% 
  purrr::reduce(left_join, by = 'precinct')

ed_16 <- ed_16 %>% 
  left_join(d_12, by = 'precinct') %>% 
  relocate(geometry, .after = everything())
ed_21 <- ed_21 %>% 
  left_join(d_21, by = 'precinct') %>% 
  relocate(geometry, .after = everything())
ed_22 <- ed_22 %>% 
  left_join(d_22, by = 'precinct') %>% 
  relocate(geometry, .after = everything())

st_write(ed_16, here('data/rockland_2012-2020.geojson'))
st_write(ed_21, here('data/rockland_2021-2021.geojson'))
st_write(ed_22, here('data/rockland_2022-2022.geojson'))
