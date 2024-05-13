# --------------------------------------------------------------------------------------------------
# CAI Robot pour Nord Kivu
#
# author : Cat Eisenhauer
# date : Mai 2024
# --------------------------------------------------------------------------------------------------

# PACKAGES -----------------------------------------------------------------------------------------
library(tidyverse)
library(gt)

# IMPORT AND CLEAN ---------------------------------------------------------------------------------

# map of health zones
map <- rio::import(here::here('data', 'reference', 'COD_adm2.rds')) |>
  sf::st_as_sf() |>
  filter(adm1_name == 'Nord-Kivu')


# importer les donnees
df <- rio::import(here::here("data", "raw", "data_nk.xls"),
                  skip = 1)

# nettoyer les donnees
df <- df |>
  janitor::clean_names() |>                         # nettoyer les en-tetes
  select(zone = organisationunitname,               # selectioner et renommer
         period = periodname,
         naissances = a_2_3_naissances_vivantes,
         penta = b_8_4_dtc_hep_b_hib1,
         cpon3 = a_2_6_c_po_n3_42eme_jour,
         diarrhee = a_1_7_diarrhee_simple) |>
  mutate(zone = str_sub(zone, 4, -15),             # enlever "nk" et "Zone de Sante"
         mois = str_sub(period, 1, -6),
         annee = as.numeric(str_sub(period, -4, -1)),
         period = dmy(str_c(1, period, sep = ' '),
                      locale = c("LC_TIME" = "fr_FR.utf8"))) |>
  filter(!(is.na(naissances) & is.na(penta) & is.na(cpon3) & is.na(diarrhee)))


# EVALUATE CRITERIA --------------------------------------------------------------------------------
df <- df |>
  # wash criteria (simple diarrhea)
  mutate(.by = period,
         diarrhee_prov = sum(diarrhee, na.rm = TRUE),
         diarrhee_thresh = mean(diarrhee, na.rm = TRUE) + sd(diarrhee, na.rm = TRUE),
         ind_diarrhee = diarrhee / diarrhee_prov,
         crit_diarrhee = diarrhee > diarrhee_thresh) |>
  
  # vaccination criteria (penta)
  mutate(.by = zone,
         ind_penta = penta / lag(naissances),
         crit_penta = ind_penta > 1 | ind_penta < 0.5) |>

  # maternal follow-up indicator (cpon3)
  mutate(ind_cpon3 = cpon3 / penta,
         crit_cpon3 = ind_cpon3 > 1.2 | ind_cpon3 < 0.8) |>

  # ajouter le score
  tidytable::mutate_rowwise(score = sum(crit_cpon3, crit_penta, crit_diarrhee, na.rm = TRUE))



# BUILD OUTPUTS ------------------------------------------------------------------------------------
# output 1 : table
tmp <- df |>
  filter(period == max(period)) |>
  mutate(across(starts_with('crit_'), ~ ifelse(., 'Alerte', ''))) |>
  arrange(desc(score)) |>
	select(zone, score, ends_with('_cpon3'), ends_with('_penta'), ends_with('_diarrhee'),
         naissances, cpon3, penta, diarrhee)

tbl <- tmp |>
	gt(rowname_col = 'zone') |>
  fmt_number(c(ind_cpon3),
             decimals = 1) |>
  fmt_number(c(naissances, cpon3, penta, diarrhee),
             decimals = 0) |>
  fmt_percent(c(ind_penta, ind_diarrhee),
              decimals = 0) |>
  sub_missing() |>
  tab_header(title = 'Tableau de Priorité',
             subtitle = format(max(df$period), '%B %Y')) |>
  tab_spanner(label = 'Suivi Maternelle',
              columns = c(ends_with('_cpon3'))) |>
  tab_spanner(label = 'Vaccination',
              columns = c(ends_with('_penta'))) |>
  tab_spanner(label = 'WASH',
              columns = c(ends_with('_diarrhee'))) |>
  tab_spanner(label = 'Valeurs Bruts',
              columns = c(naissances, cpon3, penta, diarrhee)) |>
  cols_align(align = 'center',
             columns = everything()) |>
  cols_label(zone = 'Zone de Santé',
             crit_cpon3 = 'Alerte',
             ind_cpon3 = 'Indicateur',
             crit_penta = 'Alerte',
             ind_penta = 'Indicateur',
             crit_diarrhee = 'Alerte',
             ind_diarrhee = 'Indicateur',
             naissances = 'Naissances Vivantes',
             cpon3 = 'CPON3',
             penta = 'Penta',
             diarrhee = 'Diahree Simple',
             score = 'Score') |>
  tab_footnote(footnote = 'Visites CPON3 / Vaccinations Penta',
               locations = cells_column_labels(columns = ind_cpon3)) |>
  tab_footnote(footnote = 'Indicateur suivi maternelle > 1.2 ou < 0.8',
               locations = cells_column_labels(columns = crit_cpon3)) |>
  tab_footnote(footnote = 'Vaccinations Penta / Naissances Vivantes',
               locations = cells_column_labels(columns = ind_penta)) |>
  tab_footnote(footnote = 'Indicateur vaccination > 1 ou < 0.5',
               locations = cells_column_labels(columns = crit_penta)) |>
  tab_footnote(footnote = 'Cas par Zone / Cas Total dans la Province',
               locations = cells_column_labels(columns = ind_diarrhee)) |>
  tab_footnote(footnote = 'Cas dans le Zone > Moyen dans la Province + 1 Deviation Standard',
               locations = cells_column_labels(columns = crit_diarrhee))

tbl |>
  gtsave(here::here('outputs', 'summary_table.html'))


# output 2: history heatmap
tmp <- df |>
  filter(period > max(period) %m-% months(6)) |>
  select(zone, period, starts_with('crit_')) |>
  pivot_longer(cols = starts_with('crit_')) |>
  mutate(name = recode(name,
                       'crit_diarrhee' = 'WASH',
                       'crit_penta' = 'Vaccination',
                       'crit_cpon3' = 'Maternelle'))

plt_heatmap <- tmp |>
  ggplot(aes(x = period,
             y = name,
             fill = value)) +
  geom_tile(color = '#ffffff',
            linewidth = 5) +
  scale_fill_manual(values = c('#ffffff', '#400e0e'),
                    labels = c('', 'Alerte')) +
  theme_classic(base_size = 15) +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  facet_wrap(vars(zone))

ggsave(here::here('outputs', 'heatmap_plot.png'),
       plot = plt_heatmap,
       width = 20,
       height = 10.9)


# output 3: map !
tmp <- map |>
  left_join(df |> filter(period == max(period)),
            by = c('adm2_name' = 'zone'))

plt_map <- tmp |>
  ggplot(aes(fill = score)) +
  geom_sf(color = '#000000') +
  scale_fill_gradient(name = 'Score',
                      low = '#ffffff',
                      high = '#660d0d') +
  theme_void(base_size = 15) +
  labs(title = 'Carte de Priorité',
			 subtitle = format(max(df$period), '%B %Y'))

ggsave(here::here('outputs', 'map.png'),
       plot = plt_map,
       width = 4.54,
       height = 4.38)

