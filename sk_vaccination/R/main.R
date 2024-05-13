# --------------------------------------------------------------------------------------------------
# sk vaccination analysis
#
# author : cat eisenhauer
# date : may 2024
# --------------------------------------------------------------------------------------------------

# packages -----------------------------------------------------------------------------------------
library(tidyverse)
library(gt)


# import and clean ---------------------------------------------------------------------------------
df <- rio::import(here::here('data', 'raw', 'sk_vaccination_data.xls'))

# clean
df <- df |>
  janitor::clean_names() |> 
  select(zone = organisationunitname,
         date = periodname,
         naissances = a_2_3_naissances_vivantes,
         penta = b_8_4_dtc_hep_b_hib1,
         vpo = b_8_4_vpo_0,
         bcg = b_8_4_bcg,
         var = b_8_4_var) |>
  mutate(zone = str_sub(zone, 4, -15),
         date = dmy(str_c(1, date, sep = ' '),
                      locale = c("LC_TIME" = "fr_FR.utf8"))) |>
  filter(!(is.na(naissances) & is.na(penta) & is.na(vpo) & is.na(bcg) & is.na(var)),
         date < max(date))


# evaluate indicators ------------------------------------------------------------------------------
df <- df |>
  mutate(.by = zone,

  # adjust vax to be by live birth
         penta = penta / lag(naissances),
         across(c(vpo, bcg, var), ~ . / naissances),

  # smooth vax over 6 months
         across(c(penta, vpo, bcg, var),
                ~ zoo::rollapply(.,
                                 width = 4,
                                 FUN = mean,
                                 na.rm = TRUE,
                                 align = 'center',
                                 fill = NA,
                                 partial = TRUE),
                .names = '{.col}_smooth'),

  # add change over 3 months
         across(ends_with('_smooth'), ~ (. - lag(., 3)) / lag(., 3),
                .names = '{.col}_3m'),

  # add change over 6 months
         across(ends_with('_smooth'), ~ (. - lag(., 6)) / lag(., 6),
                .names = '{.col}_6m'),
  
  # identify those decareasing (over 3 months)
         across(ends_with('_3m'), ~ . <= -0.25,
                .names = '{.col}_baisse')) |>

  # add score
  tidytable::mutate_rowwise(score = sum(penta_smooth_3m_baisse, vpo_smooth_3m_baisse,
                                        bcg_smooth_3m_baisse, var_smooth_3m_baisse,
                                        na.rm = TRUE) / 4)


# table ! ------------------------------------------------------------------------------------------
# build data for table
tmp <- df |>
  filter(date == max(date)) |>
  arrange(desc(score)) |>
	select(zone, score, starts_with('penta'), starts_with('vpo'), starts_with('bcg'),
         starts_with('var'), -ends_with('_baisse'), -ends_with('_smooth'))

# format table
tbl <- tmp |>
	gt(rowname_col = 'zone') |>
  fmt_number(c(penta, vpo, bcg, var),
             decimals = 1) |>
  fmt_percent(c(score, ends_with('_3m'), ends_with('_6m')),
              decimals = 0) |>
  sub_missing() |>
  tab_header(title = 'Statut de la Vaccination au Sud Kivu',
             subtitle = format(max(df$date), '%B %Y')) |>
  tab_spanner(label = 'DTC',
              columns = starts_with('penta')) |>
  tab_spanner(label = 'VPO 0',
              columns = starts_with('vpo')) |>
  tab_spanner(label = 'BCG',
              columns = starts_with('bcg')) |>
  tab_spanner(label = 'VAR',
              columns = starts_with('var')) |>
  cols_align(align = 'center',
             columns = everything()) |>
  cols_label(zone = 'Zone de Santé',
             penta = 'Couverture',
             penta_smooth_3m = '3 Mois',
             penta_smooth_6m = '6 Mois',
             vpo = 'Couverture',
             vpo_smooth_3m = '3 Mois',
             vpo_smooth_6m = '6 Mois',
             bcg = 'Couverture',
             bcg_smooth_3m = '3 Mois',
             bcg_smooth_6m = '6 Mois',
             var = 'Couverture',
             var_smooth_3m = '3 Mois',
             var_smooth_6m = '6 Mois',
             score = 'Score') |>
  tab_footnote(footnote = 'Pourcentage des vaccins a la baisse sur les 3 derniers mois',
               locations = cells_column_labels(columns = score))

# save table
tbl |>
  gtsave(here::here('out', 'summary_table.html'))

