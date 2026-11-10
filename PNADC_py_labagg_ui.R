######################################## Libraries #####################################################################


library(zoo)
library(readxl)
library(tidyverse)
library(data.table)
library(fixest)
library(modelsummary)
library(tinytable)
library(pdftools)
library(WeightIt)
library(cobalt)
library(glue)
library(ggplot2)
library(marginaleffects)
library(MatchIt)
library(tibble)



##################################### Paths ############################################################################

path_work = "mv"  

if (path_work == "yes") {
  
  base_load  = "C:/Users/caiorcb1/OneDrive - Insper/Pesquisa/PNADC_py"
  base_save  = "C:/Users/caiorcb1/OneDrive - Insper/Pesquisa/PNADC_py/Paper_ui"
  setwd(base_save)
  
} else if (path_work == "no") {
  
  base_load  = "D:/OneDrive - Insper/Pesquisa/PNADC_py"
  base_save  = "D:/OneDrive - Insper/Pesquisa/PNADC_py/Paper_ui"
  setwd(base_save)
  
} else if (path_work == "mv") {
  
  base_load  = "C:/Users/caiorcb/OneDrive - Insper/Pesquisa/PNADC_py"
  base_save  = "C:/Users/caiorcb/OneDrive - Insper/Pesquisa/PNADC_py/Paper_ui"
  setwd(base_save)
  
} else {
  stop("path_work must be 'yes' or 'no' or 'mv'")
}
gc()

##################################### Database Loading #################################################################

pnadc_q_pd = readRDS(file = file.path(base_load, "pnadc_py_base.rds"))
gc()


################################################ Creating some variables and deleting others (PBF, UI, hours and BPC) #####################################


pnadc_q_pd = pnadc_q_pd |>
  mutate(ui_main = case_when(
    ui %in% 1 | ui_two %in% 1 ~ 1L,   
    ui_two %in% 2             ~ 0L,   
    TRUE                      ~ NA_integer_)) %>%  
  select(-c(ui, ui_two))
gc()


pnadc_q_pd = pnadc_q_pd |>
  mutate(bpc_main = case_when(
    bpc %in% 1 | bpc_two %in% 1 ~ 1L,  
    bpc_two %in% 2              ~ 0L,   
    TRUE                        ~ NA_integer_)) %>%  
  select(-c(bpc, bpc_two))
gc()


pnadc_q_pd = pnadc_q_pd |>
  mutate(othesoci_main = case_when(
    othesoci     %in% 1 | othesoci_two %in% 1 ~ 1L,  
    othesoci_two %in% 2                       ~ 0L,   
    TRUE                                      ~ NA_integer_)) %>%  
  select(-c(othesoci, othesoci_two))
gc()


pnadc_q_pd = pnadc_q_pd |>
  mutate(ui_value_main       = coalesce(ui_value,  ui_value_two),
         bpc_value_main      = coalesce(bpc_value, bpc_value_two),
         othesoci_value_main = coalesce(othesoci_value, othesoci_value_two)) %>% 
  select(-c(ui_value,  
            ui_value_two, 
            bpc_value, 
            bpc_value_two, 
            othesoci_value, 
            othesoci_value_two))
gc()


pnadc_q_pd = pnadc_q_pd |> 
  mutate(hours_main_e = coalesce(hours_main_e_one, hours_main_e_two)) %>% 
  select(-c(hours_main_e_one, hours_main_e_two))
gc()


######################################### Creating some variables about formal and informal employment (by CNPJ and social security) ########################################


pnadc_q_pd = pnadc_q_pd |>
  mutate(laborforce = case_when(labor_force == 1 ~ 1L,
                                labor_force == 2 ~ 0L, TRUE ~ NA_integer_),
         out_lf     = case_when(labor_force == 2 ~ 1L,
                                labor_force == 1 ~ 0L, TRUE ~ NA_integer_),
         occupied   = case_when(unemp == 1 ~ 1L,
                                unemp == 2 ~ 0L, TRUE ~ NA_integer_),
         unempl     = case_when(unemp == 2 ~ 1L,
                                unemp == 1 | labor_force == 2 ~ 0L, TRUE ~ NA_integer_),
         informal   = case_when(unemp == 1 & (    coalesce(priv_inf            == 1, FALSE) |
                                                  coalesce(empdom_inf          == 1, FALSE) |
                                                  coalesce(emppub_inf          == 1, FALSE) |
                                                  coalesce(selfemp_inf_cnpj == 1, FALSE) ) ~ 1L, unemp == 1 ~ 0L, TRUE ~ NA_integer_),
         formal     = case_when(unemp == 1 & informal == 0L ~ 1L,
                                unemp == 1 & informal == 1L ~ 0L, TRUE ~ NA_integer_),
         state_s     = case_when(formal   == 1L ~ 1L,
                                 informal == 1L ~ 2L,
                                 unempl   == 1L ~ 3L,
                                 out_lf   == 1L ~ 4L, TRUE ~ NA_integer_))
gc()



################################## Droping variables ###################################################################

pnadc_q_pd = pnadc_q_pd |>
  select(-c(soc_sec, 
            soc_sec_two,
            literate, 
            type_school,
            enrollment,
            hs_drop,
            type_hs,
            dropout_age,
            dropout_school,
            reasf_unemp,
            reasf_unemp_two,
            reas_unemp_two,
            reas_unemp,
            time_unemp,
            college,
            tenure,
            tenure_m,
            tenure_y,
            tenure_yy))
gc()

################################################################ Creating Dummy Married #####################################################

 # setDT(pnadc_q_pd)   # converte por referência, sem copiar
 # 
 # composicoes = pnadc_q_pd[
 #   , .(comp_sig  = paste(sort(charact_hh), collapse = "-"),
 #       n_obs     = uniqueN(indid),
 #       n_oficial = first(number_hh_2)),
 #   by = .(houseid, state, date_q)
 # ][, roster_completo := (n_obs == n_oficial)]
 # 
 # setDF(pnadc_q_pd)                    # volta a data.frame para o dplyr
 # composicoes = as_tibble(composicoes)
 # 
 # # Diagnóstico rápido: quantos domicílios têm roster incompleto?
 # composicoes |>
 #   count(roster_completo) |>
 #   mutate(share = n / sum(n)) |>
 #   print()
 # 
 # # ── 2. Combinações distintas observadas nos dados ─────────────────────────────
 # combos = composicoes |>
 #   count(comp_sig, name = "n_hh") |>
 #   arrange(desc(n_hh))
 # 
 # message("Distinct: ", nrow(combos))
 # 
 # 
 # combos$head_married = NA_integer_
 # combos$son_married  = NA_integer_
 # 
 # for (i in seq_len(nrow(combos))) {
 # 
 #   codes = as.integer(strsplit(combos$comp_sig[i], "-")[[1]])
 # 
 #   # Evidências POSITIVAS de casamento (suas classificações):
 #   has_spouse    = any(codes %in% c(2, 3))
 #   has_soncouple = any(codes == 4)
 #   has_enteado   = any(codes == 6)
 #   has_nora      = any(codes == 7)
 #   n_filhos      = sum(codes %in% c(4, 5))
 # 
 #   # Chefe: casado se vê cônjuge, filho-do-casal ou enteado (sua regra)
 #   combos$head_married[i] = as.integer(has_spouse | has_soncouple | has_enteado)
 # 
 #   # Filho (4/5): casado se há genro/nora e ele é o único filho
 #   combos$son_married[i]  = as.integer(has_nora & n_filhos == 1)
 # }
 # 
 # 
 # pnadc_q_pd = pnadc_q_pd |>
 #   left_join(composicoes |>
 #               select(houseid, state, date_q, comp_sig, roster_completo),
 #             by = c("houseid", "state", "date_q")) |>
 #   left_join(combos |> select(comp_sig, head_married, son_married),
 #             by = "comp_sig")
 # gc()
 # 
 # 
 # pnadc_q_pd = pnadc_q_pd |>
 #   mutate(married = case_when(charact_hh %in% c(2, 3, 7)                  ~ 1L,
 #                              charact_hh == 1        & head_married == 1L ~ 1L,
 #                              charact_hh %in% c(4,5) & son_married  == 1L ~ 1L,
 #                              roster_completo                             ~ 0L,
 #                             TRUE                                         ~ NA_integer_)) |>
 #   select(-comp_sig, -head_married, -son_married, roster_completo)
 # rm(combos, composicoes)
 # gc()

############################### Creating some variables about the region of the country (capital, metropolitan region and others) ########################################

pnadc_q_pd = pnadc_q_pd |>
  mutate(capital         = as.integer(geo_area == 1),
         RM              = as.integer(geo_area %in% c(2,3)))
gc()

##################################### Filters ##########################################################################

## age and hours ##
pnadc_q_pd = pnadc_q_pd |>
  filter(age >= 25 & age < 65,
         hours_main_e >= 20 | is.na(hours_main_e))
gc()

## by year ##

pnadc_q_pd = pnadc_q_pd |>
  filter(year >= 2016 & year <= 2019)
gc()


## filter by interview
pnadc_q_pd = pnadc_q_pd |>
  filter(interview == 1 | interview == 5)
gc()


## filter by hh characteristic

pnadc_q_pd = pnadc_q_pd |>
  filter(charact_hh <= 14)
gc()


## filter by people in the labor force
pnadc_q_pd = pnadc_q_pd           |>
  filter(laborforce == 1)               
gc()


## filter by wage_e
pnadc_q_pd = pnadc_q_pd |>
  filter(wage_e > 0 | is.na(wage_e))
gc()


##################################### Selecting Key Variables ########################################

pnadc_q_pd = pnadc_q_pd |>
  select(indid,
         houseid,
         date_q, 
         state,
         state_s,
         urban,
         capital,
         RM,
         geo_area,
         weight_1y,
         weight_2y,
         weight_3y,
         weight_4y,
         lhs, 
         hs,
         coll,
         age, 
         race, 
         gender,
         year, 
         quarter,
         occup,
         occupied,
         firm_act,
         interview,
         formal, 
         informal,
         unempl,
         laborforce,
         schooling_years,
         ui_main,
         ui_value_main,
         wage_e,
         wage_h,
         wage_all_e)
gc()

########################### Dropping variables that education fell and select the max education in the panel ##############################


 pnadc_q_pd = pnadc_q_pd                                   |>
 mutate(educ   = case_when(lhs  == 1 ~ 1L,
                           hs   == 1 ~ 2L,
                           coll == 1 ~ 3L))                |>
   arrange(indid, date_q)                                  |>
   group_by(indid)                                         |>
   mutate(educ_fell = any(educ < lag(educ), na.rm = TRUE)) |>
   ungroup()                                               |>
   filter(!educ_fell)                                      |>
   select(-educ_fell)
 gc()



 pnadc_q_pd = pnadc_q_pd                                                                 |>
   arrange(indid, date_q)                                                                |>
   group_by(indid)                                                                       |>
   mutate(max_educ = case_when(any(coll[age >= 25 & age <= 64] == 1, na.rm = TRUE) ~ 3L,
                               any(hs  [age >= 25 & age <= 64] == 1, na.rm = TRUE) ~ 2L,
                               any(lhs [age >= 25 & age <= 64] == 1, na.rm = TRUE) ~ 1L),
          coll = if_else(is.na(max_educ), coll, as.integer(max_educ == 3)),
          hs   = if_else(is.na(max_educ), hs,   as.integer(max_educ == 2)),
          lhs  = if_else(is.na(max_educ), lhs,  as.integer(max_educ == 1)))               |>
   select(-max_educ)                                                                      |>
   ungroup()
 gc()


#_____________________________________________________________________________________________________________________
########################################### NOW, THE ANALYSIS IS ABOUT UI BENEFICIARIES #####################################
#______________________________________________________________________________________________________________________


pnadc_q_pd = pnadc_q_pd |>
  mutate(female          = as.integer(gender   == 2),
         male            = as.integer(gender   == 1),
         black           = as.integer(race     == 2),
         urban_c         = as.integer(urban    == 1),
         age_sqr         = age^2,
         age_cub         = age^3,
         age_4th         = age^4,
         school_years    = schooling_years,
         school_years_sqr= schooling_years^2,
         firm_act_two    = substr(as.character(firm_act), 1, 2),
         firm_act_five   = substr(as.character(firm_act), 1, 5),
         date_q_f        = as.factor(date_q),
         year_f          = as.factor(year),
         quarter_f       = as.factor(quarter),
         occup_f         = as.factor(occup),
         race_f          = as.factor(race),
         firm_act_two_f  = as.factor(firm_act_two),
         firm_act_five_f = as.factor(firm_act_five),
         state_f         = as.factor(state),
         geo_area_f      = as.factor(geo_area),
         urban_f         = as.factor(urban))
gc()

############################################### Starting some analysis ############################################


### Educational Shares

educ_shares = pnadc_q_pd |>
  filter(educ %in% 1:3, !is.na(weight_3y), weight_3y > 0) |>
  summarise(share_lhs  = weighted.mean(as.integer(educ == 1), w = weight_3y),
            share_hs   = weighted.mean(as.integer(educ == 2), w = weight_3y),
            share_coll = weighted.mean(as.integer(educ == 3), w = weight_3y))
print(educ_shares)


## Labor market shares: unconditional

labor_shares = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_1y), weight_1y > 0) |>
  summarise(share_formal     = weighted.mean(as.integer(state_s == 1),w = weight_1y),
            share_informal   = weighted.mean(as.integer(state_s == 2),w = weight_1y),
            share_unemployed = weighted.mean(as.integer(state_s == 3),w = weight_1y))
print(labor_shares)


###############################################
# Labor market shares by quarter
###############################################

labor_shares_q = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_1y), weight_1y > 0) |>
  group_by(year, quarter) |>
  summarise(share_formal     = weighted.mean(as.integer(state_s == 1),w = weight_1y),
            share_informal   = weighted.mean(as.integer(state_s == 2),w = weight_1y),
            share_unemployed = weighted.mean(as.integer(state_s == 3),w = weight_1y)) |>
  mutate(time = year + (quarter - 1) / 4) |>
  arrange(year, quarter)
print(labor_shares_q)



## Plot of the labor market by year

###############################################
# Labor shares evolution
###############################################

labor_shares_q_long = labor_shares_q |>
  pivot_longer(cols = starts_with("share_"),names_to = "labor_status",values_to = "share") |>
  mutate(labor_status = factor(labor_status,levels = c("share_formal","share_informal","share_unemployed"),
      labels = c("Formal","Informal","Unemployed")))

ggplot(labor_shares_q_long,
  aes(x = time, y = 100 * share,color = labor_status)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Formal"     = "#21618C",
                                "Informal"   = "#E67E22",
                                "Unemployed" = "#C0392B")) +
  scale_x_continuous(breaks = 2012:2019 ) +
  labs(x = "Year",
       y = "Share of labor force (%)",
       color = NULL,
       title = "Labor market composition in Brazil",
       subtitle = "PNADC, 2012–2019") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")



# Share of UI recipients working informally

ui_informal_time = pnadc_q_pd |>
  filter(ui_main == 1,!is.na(state_s),!is.na(weight_4y),weight_4y > 0) |>
  group_by(year, quarter) |>
  summarise(n_ui = n(),
            share_informal = weighted.mean(as.integer(state_s == 2), w = weight_4y),.groups = "drop") |>
  mutate(time = year + (quarter - 1) / 4)
gc()

ggplot(ui_informal_time,aes(x = time, y = 100*share_informal)) +
  geom_line(linewidth = 0.9, color = "red") +
  geom_point(size = 1.5, color = "red") +
  labs(x     = "Year",
       y     = "UI recipients working informally (%)",
       title = "Share of UI takers in the Informal Sector",
    subtitle = "Share among all individuals collecting UI") +
  theme_minimal(base_size = 12)



ui_labor_status = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s),!is.na(weight_1y),weight_1y > 0) |>
  mutate(labor_status = factor(state_s, levels = 1:3,labels = c("Formal","Informal","Unemployed"))) |>
  group_by(labor_status) |>
  summarise(n = n(),weighted_n = sum(weight_1y),.groups = "drop") |>
  mutate(share = weighted_n / sum(weighted_n),
     share_pct = 100 * share)
print(ui_labor_status)


ggplot(
  ui_labor_status,
  aes(x = labor_status, y = share_pct)
) +
  geom_col(fill = "#21618C", width = 0.65) +
  geom_text(
    aes(label = sprintf("%.1f%%", share_pct)),
    vjust = -0.4
  ) +
  labs(
    x = NULL,
    y = "Share of UI recipients (%)",
    title = "Labor market status of UI recipients",
    subtitle = "PNADC, 2012–2019"
  ) +
  theme_minimal(base_size = 12)






































































































########################################### Regression  - Labor Force (LF) #####################################

# ──  Regressions business cycle ─────────────────────────────────────────────────────────────
# Progressive specification: each model adds a layer of fixed effects
# so you can track how much of the raw gap is explained by composition

ctrl = "age + 
       age_sqr + 
       age_cub + 
       age_4th + 
       school_years     + 
       school_years_sqr +
       male     +
       black    + 
       child_n  + 
       teen_n   + 
       adults_n + 
       urban_c  + 
       capital  + 
       RM"

fe   = "state_f + quarter_f"

# Lista de fórmulas — só muda o controle de renda

formulas_lf  = list("No Controls"   = glue("laborforce ~ pbf_hh_final"),
                    "With Controls" = glue("laborforce ~ pbf_hh_final + {ctrl} + ear_group_hh_pc_pg_post | {fe}")) 

formulas_oc  = list("No Controls"   = glue("occupied   ~ pbf_hh_final" ),
                    "With Controls" = glue("occupied   ~ pbf_hh_final + {ctrl} + ear_group_hh_pc_pg_post | {fe}")) 

formulas_un  = list("No Controls"   = glue("unempl     ~ pbf_hh_final" ),
                    "With Controls" = glue("unempl     ~ pbf_hh_final + {ctrl} + ear_group_hh_pc_pg_post | {fe}")) 

formulas_for = list("No Controls"   = glue("formal     ~ pbf_hh_final"),
                    "With Controls" = glue("formal     ~ pbf_hh_final + {ctrl} + ear_group_hh_pc_pg_post | {fe}")) 

formulas_inf = list("No Controls"   = glue("informal   ~ pbf_hh_final"),
                    "With Controls" = glue("informal   ~ pbf_hh_final + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))


# Roda todas de uma vez → lista de modelos
regs_lf_pbf  = map(formulas_lf,  ~ feols(as.formula(.x),  data = pnadc_q_pd,            cluster = ~indid, split = ~year))
regs_oc_pbf  = map(formulas_oc,  ~ feols(as.formula(.x),  data = pnadc_u_pbf_case1  ,   cluster = ~indid, split = ~year))
regs_un_pbf  = map(formulas_un,  ~ feols(as.formula(.x),  data = pnadc_u_pbf_case1  ,   cluster = ~indid, split = ~year))
regs_inf_pbf = map(formulas_inf, ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case1,   cluster = ~indid, split = ~year))
regs_for_pbf = map(formulas_for, ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case1,   cluster = ~indid, split = ~year))
gc()

etable(regs_inf_pbf)


# modelsummary(regs_lf_pbf,
#              title = "Probabilidade de estar na LF - PBF",
#              stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
#              coef_map = c(
#                "age"              = "Age"   ,
#                "age_sqr"          = "Age^2" ,
#                "age_cub"          = "Age^3" ,
#                "age_4th"          = "Age^4" ,
#                "male"             = "Male"  ,
#                "black"            = "Black" ,
#                "urban_c"          = "Urban" ,
#                "capital"          = "Capital",
#                "RM"               = "RM",
#                "school_years"     = "Years of Study",
#                "school_years_sqr" = "Years of Study^2",
#                "hs"               = "High school",
#                "coll"             = "College",
#                "child_n"          = "N. children",
#                "teen_n"           = "N. teens",
#                "adults_n"         = "N. adults",
#                "ear_hh_e_exante_pbf_pc_post" = "Log HH inc. p.c. (ex-PBF)",
#                "ear_group_1_hh_pc_pg_post" = "Log Group 1 HH inc. p.c. (ex-PBF)",
#                "year::2012:pbf_hh" = "PBF x 2012",
#                "year::2013:pbf_hh" = "PBF x 2013",
#                "year::2014:pbf_hh" = "PBF x 2014",
#                "year::2015:pbf_hh" = "PBF x 2015",
#                "year::2016:pbf_hh" = "PBF x 2016",
#                "year::2017:pbf_hh" = "PBF x 2017",
#                "year::2018:pbf_hh" = "PBF x 2018",
#                "year::2019:pbf_hh" = "PBF x 2019",
#                "year::2020:pbf_hh" = "PBF x 2020",
#                "year::2021:pbf_hh" = "PBF x 2021",
#                "year::2022:pbf_hh" = "PBF x 2022",
#                "year::2023:pbf_hh" = "PBF x 2023",
#                "year::2024:pbf_hh" = "PBF x 2024",
#                "year::2025:pbf_hh" = "PBF x 2025"),
#              gof_map  = list(
#                list(raw = "nobs",          clean = "Observations", fmt = 0),
#                list(raw = "r.squared",     clean = "R2",           fmt = 3),
#                list(raw = "adj.r.squared", clean = "Adj. R2",      fmt = 3),
#                list(raw = "FE: year_f"   , clean = "Year FE",      fmt = NULL),
#                list(raw = "FE: quarter_f", clean = "Quarter FE",   fmt = NULL),
#                list(raw = "FE: race_f",    clean = "Race FE",      fmt = NULL),
#                list(raw = "FE: state_f",   clean = "State FE",     fmt = NULL),
#                list(raw = "FE: geo_area_f",clean = "City FE",      fmt = NULL),
#                list(raw = "FE: urban_f",   clean = "Urban FE",     fmt = NULL))) |> 
#   group_tt(j = list("No Controls"  = 2, 
#                     "All Controls" = 3,
#                     "All Controls + HH Income" = 4))


#################################### Graph Coefficients Labor Force (LF) ###################################################



extract_coef_split <- function(m, regname, coef_name = "pbf_hh_final") {
  
  model_info <- fixest::models(m)
  model_list <- as.list(m)
  
  stopifnot(nrow(model_info) == length(model_list))
  
  map2_dfr(
    model_list,
    model_info$sample,
    function(modelo_ano, ano_i) {
      
      resultado <- broom::tidy(modelo_ano, conf.int = TRUE) |>
        filter(term == coef_name)
      
      if (nrow(resultado) == 0) {
        
        return(
          tibble(
            year      = as.integer(as.character(ano_i)),
            estimate  = NA_real_,
            conf.low  = NA_real_,
            conf.high = NA_real_,
            version   = regname
          )
        )
        
      } else {
        
        resultado |>
          transmute(
            year      = as.integer(as.character(ano_i)),
            estimate  = estimate,
            conf.low  = conf.low,
            conf.high = conf.high,
            version   = regname
          )
      }
    }
  )
}

# ── 1. Rótulos: name = chave em formulas_lf, value = texto do gráfico ──────
rotulos <- c("No Controls"    = "No Controls","With Controls"  = "With Controls")


organiza_coeficientes <- function(regs) {imap_dfr(regs,~ extract_coef_split(m = .x,regname = .y,coef_name = "pbf_hh_final")) |>
    mutate(version = factor(version,
                            levels = names(rotulos),
                            labels = unname(rotulos)))}

# ── 2. Extração já automática via imap_dfr ───────────────────────────────────
coef_lf_pbf  = organiza_coeficientes(regs_lf_pbf)
coef_un_pbf  = organiza_coeficientes(regs_un_pbf)
coef_oc_pbf  = organiza_coeficientes(regs_oc_pbf)
coef_for_pbf = organiza_coeficientes(regs_for_pbf)
coef_inf_pbf = organiza_coeficientes(regs_inf_pbf)

gc()

# ── 3. Gráfico — idêntico ao seu, sem nenhuma mudança ───────────────────────
ggplot(coef_lf_pbf, aes(year, estimate, color = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.3, linewidth = 0.4,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title    = "Probability of being in the Labor Force",
       subtitle = "PBF Beneficiary vs. Non-Beneficiary (in p.p.)",
       x = NULL, y = NULL, color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave("prob_lf_pbf.pdf", width = 11, height = 7, device = cairo_pdf)


ggplot(coef_un_pbf, aes(year, estimate, color = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.3, linewidth = 0.4,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title    = "Probability of being Unemployed",
       subtitle = "PBF Beneficiary vs. Non-Beneficiary (in p.p.)",
       x = NULL, y = NULL, color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave("prob_un_pbf.pdf", width = 11, height = 7, device = cairo_pdf)


ggplot(coef_oc_pbf, aes(year, estimate, color = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.3, linewidth = 0.4,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.25)) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title    = "Probability of being Employed",
       subtitle = "PBF Beneficiary vs. Non-Beneficiary (in p.p.)",
       x = NULL, y = NULL, color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave("prob_oc_pbf.pdf", width = 11, height = 7, device = cairo_pdf)


ggplot(coef_for_pbf, aes(year, estimate, color = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.3, linewidth = 0.4,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.25)) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title    = "Probability of being in the Formal Sector",
       subtitle = "PBF Beneficiary vs. Non-Beneficiary (in p.p.)",
       x = NULL, y = NULL, color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave("prob_for_pbf.pdf", width = 11, height = 7, device = cairo_pdf)


ggplot(coef_inf_pbf, aes(year, estimate, color = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.3, linewidth = 0.4,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(title    = "Probability of being in the Informal Sector",
       subtitle = "PBF Beneficiary vs. Non-Beneficiary (in p.p.)",
       x = NULL, y = NULL, color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave("prob_inf_pbf.pdf", width = 11, height = 7, device = cairo_pdf)



################################################## Now, we are going to see the interaction between PBF and the labor market in the next interview (interview 5) ##################################################


pnadc_q_pd = pnadc_q_pd             |>
  arrange(indid, date_q, interview) |>
  group_by(indid)                   |>
  mutate(next_interview = lead(interview),
         state_s_5  = if_else(interview == 1 & next_interview == 5, dplyr::lead(state_s),      NA_integer_),
         pbf_hh_5   = if_else(interview == 1 & next_interview == 5, dplyr::lead(pbf_hh_final), NA_integer_),
         pbf_ind_5  = if_else(interview == 1 & next_interview == 5, dplyr::lead(pbf_main),     NA_integer_)) |>
  ungroup()
gc()



pnadc_q_pd = pnadc_q_pd |>
  mutate(formal_5    = case_when(state_s_5 == 1            ~ 1,
                                 state_s_5 %in% c(2, 3, 4) ~ 0,
                                 TRUE ~ NA_integer_),
         informal_5   = case_when(state_s_5 == 2            ~ 1,
                                  state_s_5 %in% c(1, 3, 4) ~ 0,
                                  TRUE ~ NA_integer_),
         unempl_5     = case_when(state_s_5 == 3            ~ 1,
                                  state_s_5 %in% c(1, 2, 4) ~ 0,
                                  TRUE ~ NA_integer_), 
         out_lf_5     = case_when(state_s_5 == 4            ~ 1,
                                  state_s_5 %in% c(1, 2, 3) ~ 0,
                                  TRUE ~ NA_integer_),
         laborforce_5 = case_when(state_s_5 %in% c(1, 2, 3) ~ 1,
                                  state_s_5 == 4            ~ 0,
                                  TRUE ~ NA_integer_),
         occupied_5   = case_when(state_s_5 %in% c(1, 2)    ~ 1,
                                  state_s_5 == 3            ~ 0,
                                  state_s_5 == 4            ~ NA_integer_,
                                  TRUE ~ NA_integer_))
gc()



pnadc_q_pd = pnadc_q_pd |>
  mutate(pbf_informal         = pbf_hh_final * informal,
         pbf_formal           = pbf_hh_final * formal,
         pbf_unempl           = pbf_hh_final * unempl,  
         pbf_laborforce       = pbf_hh_final * laborforce)
gc()



######################################### Redoing the groups #############################################################

pnadc_q_pd_case2 = pnadc_q_pd                  |>
  filter(interview == 1, !is.na(state_s_5))
gc()

pnadc_u_pbf_case2 = pnadc_q_pd_case2           |>
  filter(laborforce == 1, laborforce_5 == 1)               
gc()

pnadc_occ_pbf_case2 = pnadc_q_pd_case2         |>
  filter(occupied == 1, occupied_5 == 1)   
gc()


########################## Now, we are going to see the interaction between PBF and the labor market in the next interview (interview 5) ##########################

ctrl = "age             + 
       age_sqr          + 
       age_cub          + 
       age_4th          + 
       school_years     + 
       school_years_sqr +
       male             +
       black            + 
       child_n          + 
       teen_n           + 
       adults_n         + 
       urban_c          + 
       capital          + 
       RM"

fe   = "state_f + quarter_f"


formulas_lf_next         = list("No Controls"   = glue("laborforce_5 ~ pbf_hh_final + laborforce + pbf_laborforce"),
                                "With Controls" = glue("laborforce_5 ~ pbf_hh_final + laborforce + pbf_laborforce + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_unempl_next     = list("No Controls"   = glue("unempl_5 ~ pbf_hh_final + unempl + pbf_unempl"),
                                "With Controls" = glue("unempl_5 ~ pbf_hh_final + unempl + pbf_unempl + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_for_next        = list("No Controls"   = glue("formal_5 ~ pbf_hh_final + formal + pbf_formal"),
                                "With Controls" = glue("formal_5 ~ pbf_hh_final + formal + pbf_formal + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_inf_next        = list("No Controls"   = glue("informal_5 ~ pbf_hh_final + informal + pbf_informal"),
                                "With Controls" = glue("informal_5 ~ pbf_hh_final + informal + pbf_informal + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_lf_pbf_next     = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + laborforce + pbf_laborforce"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + laborforce + pbf_laborforce + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_unempl_pbf_next = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + unempl + pbf_unempl"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + unempl + pbf_unempl + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_inf_pbf_next    = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + informal + pbf_informal"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + informal + pbf_informal + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_for_pbf_next    = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + formal + pbf_formal"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + formal + pbf_formal + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

regs_lf_next         = map(formulas_lf_next,      ~ feols(as.formula(.x),  data = pnadc_q_pd_case2,    cluster = ~indid, split = ~ year))
regs_unempl_next     = map(formulas_unempl_next,  ~ feols(as.formula(.x),  data = pnadc_u_pbf_case2,   cluster = ~indid, split = ~ year))
regs_for_next        = map(formulas_for_next,     ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case2, cluster = ~indid, split = ~ year))
regs_inf_next        = map(formulas_inf_next,     ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case2, cluster = ~indid, split = ~ year))
gc()


regs_lf_pbf_next     = map(formulas_lf_pbf_next,     ~ feols(as.formula(.x),  data = pnadc_q_pd_case2,    cluster = ~indid, split = ~ year))
regs_unempl_pbf_next = map(formulas_unempl_pbf_next, ~ feols(as.formula(.x),  data = pnadc_u_pbf_case2,   cluster = ~indid, split = ~ year))
regs_inf_pbf_next    = map(formulas_inf_pbf_next,    ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case2, cluster = ~indid, split = ~ year))
regs_for_pbf_next    = map(formulas_for_pbf_next,    ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case2, cluster = ~indid, split = ~ year))
gc()



###################################### Getting the coeficients ###################################################



coef_lf_next_0 = purrr::imap_dfr(regs_lf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final + pbf_laborforce = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_lf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_lf_next_1 = purrr::imap_dfr(regs_lf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_lf_next))) |>
  dplyr::arrange(version, year)
gc()


coef_unempl_next_0 = purrr::imap_dfr(regs_unempl_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final + pbf_unempl = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_unempl_next))) |>
  dplyr::arrange(version, year)
gc()

coef_unempl_next_1 = purrr::imap_dfr(regs_unempl_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_unempl_next))) |>
  dplyr::arrange(version, year)
gc()


coef_for_next_0 = purrr::imap_dfr(regs_for_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final + pbf_formal = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_for_next))) |>
  dplyr::arrange(version, year)
gc()

coef_for_next_1 = purrr::imap_dfr(regs_for_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_for_next))) |>
  dplyr::arrange(version, year)
gc()

coef_inf_next_0 = purrr::imap_dfr(regs_inf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final + pbf_informal = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_inf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_inf_next_1 = purrr::imap_dfr(regs_inf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "pbf_hh_final = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_inf_next))) |>
  dplyr::arrange(version, year)
gc()



######################################### Now, we are going to generate the graphs #############################################



graph_lf_next_0 = ggplot(coef_lf_next_0,
                         aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in the Labor Force",
       subtitle = "Effect of PBF among workers who were in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_lf_next_0.pdf",plot = graph_lf_next_0, width = 11,height = 7,device = cairo_pdf)


graph_lf_next_1 = ggplot(coef_lf_next_1,
                         aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to the Labor Force",
       subtitle = "Effect of PBF among workers who were not in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_lf_next_1.pdf",plot = graph_lf_next_1, width = 11,height = 7,device = cairo_pdf)



graph_unempl_next_0 = ggplot(coef_unempl_next_0,
                             aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in Unemployment",
       subtitle = "Effect of PBF among workers who were unemployed",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_unempl_next_0.pdf",plot = graph_unempl_next_0, width = 11,height = 7,device = cairo_pdf)


graph_unempl_next_1 = ggplot(coef_unempl_next_1,
                             aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to Unemployment",
       subtitle = "Effect of PBF among workers who were not unemployed",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_unempl_next_1.pdf",plot = graph_unempl_next_1, width = 11,height = 7,device = cairo_pdf)


graph_for_next_0 = ggplot(coef_for_next_0,
                          aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in Formality",
       subtitle = "Effect of PBF among workers who were in the Formal sector",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_for_next_0.pdf",plot = graph_for_next_0, width = 11,height = 7,device = cairo_pdf)


graph_for_next_1 = ggplot(coef_for_next_1,
                          aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to Formality",
       subtitle = "Effect of PBF among workers who were not in the Formal sector",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_for_next_1.pdf",plot = graph_for_next_1, width = 11,height = 7,device = cairo_pdf)



graph_inf_next_0 = ggplot(coef_inf_next_0,
                          aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in Informality",
       subtitle = "Effect of PBF among workers who were in the Informal sector",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_inf_next_0.pdf",plot = graph_inf_next_0, width = 11,height = 7,device = cairo_pdf)


graph_inf_next_1 = ggplot(coef_inf_next_1,
                          aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to Informality",
       subtitle = "Effect of PBF among workers who were not in the Informal sector",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_inf_next_1.pdf",plot = graph_inf_next_1, width = 11,height = 7,device = cairo_pdf)


####################################


formulas_lf_pbf_next     = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + laborforce + pbf_laborforce"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + laborforce + pbf_laborforce + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_unempl_pbf_next = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + unempl + pbf_unempl"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + unempl + pbf_unempl + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_inf_pbf_next    = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + informal + pbf_informal"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + informal + pbf_informal + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))

formulas_for_pbf_next    = list("No Controls"   = glue("pbf_hh_5 ~ pbf_hh_final + formal + pbf_formal"),
                                "With Controls" = glue("pbf_hh_5 ~ pbf_hh_final + formal + pbf_formal + {ctrl} + ear_group_hh_pc_pg_post | {fe}"))



regs_lf_pbf_next     = map(formulas_lf_pbf_next,     ~ feols(as.formula(.x),  data = pnadc_q_pd_case2,    cluster = ~indid, split = ~ year))
regs_unempl_pbf_next = map(formulas_unempl_pbf_next, ~ feols(as.formula(.x),  data = pnadc_u_pbf_case2,   cluster = ~indid, split = ~ year))
regs_inf_pbf_next    = map(formulas_inf_pbf_next,    ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case2, cluster = ~indid, split = ~ year))
regs_for_pbf_next    = map(formulas_for_pbf_next,    ~ feols(as.formula(.x),  data = pnadc_occ_pbf_case2, cluster = ~indid, split = ~ year))
gc()


coef_lf_next_pbf_0 = purrr::imap_dfr(regs_lf_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "laborforce + pbf_laborforce = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_lf_pbf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_lf_next_pbf_1 = purrr::imap_dfr(regs_lf_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "laborforce = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_lf_pbf_next))) |>
  dplyr::arrange(version, year)
gc()



coef_unempl_next_pbf_0 = purrr::imap_dfr(regs_unempl_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "unempl + pbf_unempl = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_unempl_pbf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_unempl_next_pbf_1 = purrr::imap_dfr(regs_unempl_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "unempl = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_unempl_pbf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_for_next_pbf_0 = purrr::imap_dfr(regs_for_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "formal + pbf_formal = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_for_pbf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_for_next_pbf_1 = purrr::imap_dfr(regs_for_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "formal = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_for_pbf_next))) |>
  dplyr::arrange(version, year)
gc()


coef_inf_next_pbf_0 = purrr::imap_dfr(regs_inf_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "informal + pbf_informal = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_inf_pbf_next))) |>
  dplyr::arrange(version, year)
gc()

coef_inf_next_pbf_1 = purrr::imap_dfr(regs_inf_pbf_next,\(modelos, nome_especificacao) {
  identificacao = fixest::models(modelos)
  purrr::map2_dfr(as.list(modelos),identificacao$sample,\(modelo, ano) {marginaleffects::hypotheses(
    modelo,
    hypothesis = "informal = 0",
    vcov = TRUE,
    df = fixest::degrees_freedom(modelo, type = "t")) |>
      as.data.frame() |>
      dplyr::transmute(year      = as.integer(as.character(ano)),
                       estimate  = estimate,
                       conf.low  = conf.low,
                       conf.high = conf.high,
                       std.error = std.error, p.value,version   = nome_especificacao)})}) |>
  dplyr::mutate(version = factor(version,levels = names(regs_inf_pbf_next))) |>
  dplyr::arrange(version, year)
gc()



########################### Now, let's compute the graphs ######################




graph_lf_next_pbf_0 = ggplot(coef_lf_next_pbf_0,
                             aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in the Labor Force",
       subtitle = "Effect of PBF among workers who were in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_lf_next_pbf_0.pdf",plot = graph_lf_next_pbf_0, width = 11,height = 7,device = cairo_pdf)


graph_lf_next_pbf_1 = ggplot(coef_lf_next_pbf_1,
                             aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to the Labor Force",
       subtitle = "Effect of PBF among workers who were not in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_lf_next_pbf_1.pdf",plot = graph_lf_next_pbf_1, width = 11,height = 7,device = cairo_pdf)


graph_unempl_next_pbf_0 = ggplot(coef_unempl_next_pbf_0,
                                 aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in the Labor Force",
       subtitle = "Effect of PBF among workers who were in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_unempl_next_pbf_0.pdf",plot = graph_unempl_next_pbf_0, width = 11,height = 7,device = cairo_pdf)


graph_unempl_next_pbf_1 = ggplot(coef_unempl_next_pbf_1,
                                 aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to the Labor Force",
       subtitle = "Effect of PBF among workers who were not in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_unempl_next_pbf_1.pdf",plot = graph_unempl_next_pbf_1, width = 11,height = 7,device = cairo_pdf)


graph_for_next_pbf_0 = ggplot(coef_for_next_pbf_0,
                              aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in the Labor Force",
       subtitle = "Effect of PBF among workers who were in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_for_next_pbf_0.pdf",plot = graph_for_next_pbf_0, width = 11,height = 7,device = cairo_pdf)


graph_for_next_pbf_1 = ggplot(coef_for_next_pbf_1,
                              aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to the Labor Force",
       subtitle = "Effect of PBF among workers who were not in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_for_next_pbf_1.pdf",plot = graph_for_next_pbf_1, width = 11,height = 7,device = cairo_pdf)




graph_inf_next_pbf_0 = ggplot(coef_inf_next_pbf_0,
                              aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of staying in the Labor Force",
       subtitle = "Effect of PBF among workers who were in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_inf_next_pbf_0.pdf",plot = graph_inf_next_pbf_0, width = 11,height = 7,device = cairo_pdf)


graph_inf_next_pbf_1 = ggplot(coef_inf_next_pbf_1,
                              aes(x = year, y = estimate, color = version, group = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, linewidth = 0.4, position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(breaks = seq(2012, 2025, 1),
                     expand = expansion(add = 0.5)) +
  guides(color = guide_legend(ncol = 2,byrow = TRUE)) +
  labs(title    = "Probability of going to the Labor Force",
       subtitle = "Effect of PBF among workers who were not in the Labor Force",
       x = NULL,
       y = "Percentage points",
       color = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position   = "bottom",
        legend.text       = element_text(size = 9),
        legend.key.height = unit(12, "pt"),
        legend.margin     = margin(t = -5),
        plot.margin       = margin(5, 12, 5, 5))
ggsave(filename = "prob_inf_next_pbf_1.pdf",plot = graph_inf_next_pbf_1, width = 11,height = 7,device = cairo_pdf)





################################################## Now, we are going to run the PSM exercise #################################


ln_extr_pvrt_2012 = 70
ln_extr_pvrt_2013 = 70
ln_extr_pvrt_2014 = 77
ln_extr_pvrt_2015 = 77
ln_extr_pvrt_2016 = 85
ln_extr_pvrt_2017 = 85
ln_extr_pvrt_2018 = 89
ln_extr_pvrt_2019 = 89
ln_extr_pvrt_2020 = 89
ln_extr_pvrt_2021 = 105
ln_extr_pvrt_2022 = 105
ln_extr_pvrt_2023 = 218
ln_extr_pvrt_2024 = 218
ln_extr_pvrt_2025 = 218
ln_extr_pvrt_2026 = 218

ln_pvrt_2012 = 140
ln_pvrt_2013 = 140
ln_pvrt_2014 = 154
ln_pvrt_2015 = 154
ln_pvrt_2016 = 170
ln_pvrt_2017 = 170
ln_pvrt_2018 = 178
ln_pvrt_2019 = 178
ln_pvrt_2020 = 178
ln_pvrt_2021 = 210
ln_pvrt_2022 = 210



########################################  Let's do the PSM preparation to do the graphs #####################


############### Preparing the base #########################

pnadc_psm = pnadc_q_pd |>
  mutate(row_id_psm = row_number()) |>
  filter(pbf_hh_final %in% c(0, 1))
gc()

ctrl = "age             + 
       age_sqr          + 
       age_cub          + 
       age_4th          + 
       school_years     + 
       school_years_sqr +
       male             +
       black            + 
       child_n          + 
       teen_n           + 
       adults_n         + 
       urban_c          + 
       capital          + 
       RM"

fe = "state_f"



################################# Running the regression ###############


formula_pbf_psm = glue("pbf_hh_final ~ {ctrl} + ear_hh_e_exante_pbf_pc_post_ln |{fe}")
reg_pbf_psm_logit = feglm(as.formula(formula_pbf_psm), data = pnadc_psm, family = binomial(link = "logit"), weights = ~ weight_2y, split = ~ year)
gc()





################################# Getting the coeficients ##########################


pnadc_psm = pnadc_psm |>
  mutate(pscore_logit = NA_real_)

models_pbf_psm_logit = as.list(reg_pbf_psm_logit)
gc()


for (i in seq_along(models_pbf_psm_logit)) {
  models = models_pbf_psm_logit[[i]]
  lines_models = fixest::obs(models)
  pnadc_psm$pscore_logit[lines_models] = predict(models, type ="response")}   # The prediction of the model for each line in the database
gc()



#################### some check on Logit's results #############################


pnadc_psm |>
  group_by(year) |>
  summarise(n = n(),
            n_pscore         = sum(!is.na(pscore_logit)),
            mean_pscore      = mean(pscore_logit,  na.rm = TRUE),
            mean_pscore_pond = weighted.mean(pscore_logit, weight_2y,  na.rm = TRUE),
            min_pscore       = min( pscore_logit,  na.rm = TRUE),
            max_pscore       = max( pscore_logit,  na.rm = TRUE),
            .groups      = "drop")



ggplot(pnadc_psm,aes(x = pscore_logit, weight = weight_2y, fill = factor(pbf_hh_final))) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ year, scales = "free_y") +
  labs(x = "Propensity score Estimated - Logit",,
       fill = "PBF",
       title = "Propensity Score Distribution by Group") +
  scale_fill_manual(values = c("0" = "#377EB8", "1" = "#E41A1C"),
                    labels = c("0" = "Non-beneficiary", "1" = "Beneficiary")) +
  theme_minimal()




######################## Let's do the matching #######################################


years_psm = c(2016, 2017, 2018, 2019, 2022, 2023, 2024, 2025)


pnadc_psm = pnadc_psm |>
  mutate(PSM_logit = NA_integer_)
match_graph_logit = list()


for (ano in years_psm) {
  
  nome_ano = as.character(ano)
  
  linhas_logit = which(pnadc_psm$year == ano & !is.na(pnadc_psm$pbf_hh_final) &!is.na(pnadc_psm$pscore_logit))
  
  dados_logit = pnadc_psm[linhas_logit, ] |>
    transmute(tratamento = as.integer(pbf_hh_final),
              pscore_logit = as.numeric(pscore_logit)) |>
    as.data.frame()
  
  if (nrow(dados_logit) > 0 &&n_distinct(dados_logit$tratamento) == 2) {
    
    match_graph_logit[[nome_ano]] = MatchIt::matchit(
      tratamento ~ pscore_logit,
      data     = dados_logit,
      distance = dados_logit$pscore_logit,
      method   = "nearest",
      estimand = "ATT",
      ratio    = 1,
      replace  = FALSE,
      discard  = "both",
      m.order  = "data")
    
    pnadc_psm$PSM_logit[linhas_logit] = as.integer(match_graph_logit[[nome_ano]]$weights > 0)}
}
gc()



################################ Now, we are going to prepare the base to do the graphs of PSM ######################


limits_pbf = tribble(~year, ~L_min_wage, ~L_pvrt, ~L_extr_pvrt,
                     2016,  880, 170, 77,
                     2017,  937, 170, 85,
                     2018,  954, 178, 85,
                     2019,  998, 178, 89,
                     2022, 1212, 210, 100,
                     2023, 1302, 210, 105,
                     2024, 1412, 218, 105,
                     2025, 1518, 218, 105) |>
  mutate(L_min_wage_half = L_min_wage/2)

pnadc_psm = pnadc_psm |>
  select(-any_of(c("L_min_wage", "L_pvrt", "L_extr_pvrt", "L_min_wage_half"))) |>
  left_join(limits_pbf,by = "year")
gc()

years_graph = c(2016,2017,2018,2019,2022,2023,2024,2025)

base_graph = pnadc_psm       |>
  filter(year %in% years_graph,
         PSM_logit == 1)   |>
  mutate(model = "Logit")  |>
  transmute(year,
            model,
            pbf_hh_final,
            ear_hh_e_exante_pbf_pc_post,
            L_min_wage,
            L_min_wage_half,
            L_pvrt,
            L_extr_pvrt) |>
  filter(!is.na(pbf_hh_final),
         !is.na(ear_hh_e_exante_pbf_pc_post),
         !is.na(L_min_wage),
         ear_hh_e_exante_pbf_pc_post <= L_min_wage)

data_graph = base_graph                                                            |>
  mutate(largura_bin = L_min_wage/20,
         bin_x = floor(ear_hh_e_exante_pbf_pc_post/largura_bin)*largura_bin,
         bin_center  = bin_x + largura_bin / 2)                                    |>
  filter(bin_x < L_min_wage)                                                       |>
  group_by(model, year, bin_x, bin_center) |>
  summarise(probability_pbf = mean(pbf_hh_final),
            n_bin = n(),.groups = "drop") |>
  mutate(year = factor(year,levels = years_graph))
gc()



limits_x = ceiling(max(limits_pbf$L_min_wage)/100)*100



######################## The graphs #########################################################

graph_probability_pbf = ggplot(data_graph,
                               aes(x     = bin_center,
                                   y     = probability_pbf,
                                   color = year,
                                   group = year))                        +
  geom_line(linewidth = 0.85, alpha = 0.85) +
  geom_point(size = 2, alpha = 0.9)          +
  scale_color_viridis_d(option = "B", begin = 0.05, end = 0.9, name = "Years") +
  scale_x_continuous(limits = c(0, limite_x),breaks = seq(0,limite_x,by = 200),
                     labels = label_number(decimal.mark = ",",big.mark = ".",accuracy = 1),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, 0.1), labels = label_number(decimal.mark = ",",accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.03))) +
  
  labs(title   = "Probability of being PBF beneficiary",
       subtitle = "By Household Per-capita Income (Ex-ante of PBF)",
       x = "Household Per-capita Income (Ex-ante of PBF)",
       y = "Probability") +
  
  guides(color = guide_legend(nrow = 2,byrow = TRUE)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 17, face = "bold"),
        plot.subtitle  = element_text(size = 11, margin = margin(b = 10)), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88", linewidth = 0.35),
        legend.position = "inside", legend.position.inside = c(0.6,0.85), plot.caption = element_text(size = 8.5, hjust = 0, color = "grey35"))
ggsave(filename = "psm_pbf_all_years.pdf", plot = graph_probability_pbf, width = 12, height = 7, device = cairo_pdf)

graph_probability_pbf



########################## Now, we are going to do the analysis by minimum wage #############################

data_graph = base_graph                       |>
  mutate(income_min_wage = ear_hh_e_exante_pbf_pc_post / L_min_wage,
         largura_bin = 0.05,
         bin_x       = floor(income_min_wage/largura_bin)*largura_bin,
         bin_center  = bin_x + largura_bin/2) |>
  filter(bin_x < 1)                           |>
  group_by(year, bin_x, bin_center)           |>
  summarise(probability_pbf = mean(pbf_hh_final),
            n_bin = n(),
            .groups = "drop") |>
  mutate(year = factor(year,levels = years_graph))
graph_probability_min_wage = ggplot(data_graph,
                                    aes(x   = bin_center,
                                        y   = probability_pbf,
                                        color = year,
                                        group = year)) +
  geom_line(linewidth = 0.8,
            alpha     = 0.9) +
  geom_point(size = 2,
             alpha = 0.95) +
  scale_color_viridis_d(name = "Years", option = "B", begin = 0.05, end = 0.95) +
  scale_x_continuous(limits = c(0, 1),breaks = seq(0, 1, 0.1),
                     labels = label_percent(accuracy = 1, decimal.mark = ","),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits  = c(0, 1),breaks = seq(0, 1, 0.1),labels = label_number(decimal.mark = ",",accuracy = 0.1),
                     expand = expansion(mult  = c(0, 0.03))) +
  labs(title = "Probability of being PBF beneficiary",
       subtitle = "By household per-capita income as a percentage of the minimum wage",
       x = "Household per-capita income Ex-ante PBF (% of the minimum wage)",
       y = "Probability",
       color = "Years") +
  guides(color = guide_legend(ncol = 2, byrow = TRUE, title.position = "top",override.aes = list(linewidth = 1, size = 2.5, alpha = 1))) +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(size = 18, face = "bold",hjust = 0),
        plot.subtitle    = element_text(size = 11,hjust = 0,margin = margin(b = 10)),
        axis.title       = element_text(size = 11),
        axis.text        = element_text(size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey88",linewidth = 0.35),legend.position = "inside",legend.position.inside = c(0.98, 0.98),
        legend.justification = c("right","top"),
        legend.direction = "vertical",
        legend.background = element_rect(fill = scales::alpha("white", 0.85),color = NA),
        legend.title = element_text(size = 10,face = "bold"),
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 5,r = 7,b = 5,l = 7),
        plot.margin = margin(10, 15, 10, 10))
ggsave(filename = "psm_pbf_mw.pdf", plot = graph_probability_min_wage, 
       width = 12,
       height = 7,
       device = cairo_pdf)



data_graph = base_graph |>
  mutate(largura_bin = L_min_wage/20,
         bin_x = floor(ear_hh_e_exante_pbf_pc_post/largura_bin)*largura_bin,
         bin_center = bin_x + largura_bin/2) |>
  filter(bin_x < L_min_wage) |>
  group_by(year, bin_x, bin_center) |>
  summarise(probability_pbf = mean(pbf_hh_final), n_bin = n(), .groups = "drop")

limite_x = ceiling(max(base_graph$L_min_wage, na.rm = TRUE)/100)*100
graphs_by_year = list()





for (ano in years_graph) {
  data_year = data_graph |> filter(year == ano)
  
  graph_year = ggplot(data_year, aes(x = bin_center, y = probability_pbf)) +
    geom_line(color = "#006D77", linewidth = 0.9) +
    geom_point(color = "#006D77", size = 2.5) +
    scale_x_continuous(limits = c(0, limite_x), breaks = seq(0, limite_x, 200),
                       labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 1),
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                       labels = label_number(decimal.mark = ",", accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.03))) +
    labs(title = paste0("Probability of being PBF beneficiary — ", ano),
         subtitle = "By household per-capita income (ex-ante of PBF)",
         x = "Household per-capita income (ex-ante of PBF)",
         y = "Probability") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey88", linewidth = 0.35))
  
  graphs_by_year[[as.character(ano)]] = graph_year
  
  ggsave(filename = paste0("psm_pbf_", ano, ".pdf"),
         plot = graph_year, width = 10, height = 6, device = cairo_pdf)
}


data_graph = base_graph |>
  mutate(income_min_wage = ear_hh_e_exante_pbf_pc_post/L_min_wage,
         largura_bin = 0.05,
         bin_x = floor(income_min_wage/largura_bin)*largura_bin,
         bin_center = bin_x + largura_bin/2) |>
  filter(bin_x < 1) |>
  group_by(year, bin_x, bin_center) |>
  summarise(probability_pbf = mean(pbf_hh_final), n_bin = n(), .groups = "drop")

graphs_min_wage_by_year = list()



for (ano in years_graph) {
  data_year = data_graph |> filter(year == ano)
  
  graph_year = ggplot(data_year, aes(x = bin_center, y = probability_pbf)) +
    geom_line(color = "#006D77", linewidth = 0.9) +
    geom_point(color = "#006D77", size = 2.5) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                       labels = label_percent(accuracy = 1, decimal.mark = ","),
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                       labels = label_number(decimal.mark = ",", accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.03))) +
    labs(title = paste0("Probability of being PBF beneficiary — ", ano),
         subtitle = "By household per-capita income as a percentage of the minimum wage",
         x = "Household per-capita income (% of the minimum wage)",
         y = "Probability") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey88", linewidth = 0.35))
  
  graphs_min_wage_by_year[[as.character(ano)]] = graph_year
  ggsave(filename = paste0("psm_mw_", ano, ".pdf"),
         plot = graph_year, width = 10, height = 6, device = cairo_pdf)
}









################################ Now, we are going to reproduce the other figures of income ##############################


base_formal_graph = pnadc_psm |>
  filter(year %in% years_graph) |>
  transmute(year, pbf = pbf_hh_final,
            renda_exante = ear_hh_e_exante_pbf_pc_post,
            formal_income_share = ear_hh_e_for_share,
            L1 = L_pvrt, L2 = L_min_wage_half, L3 = L_min_wage) |>
  filter(!is.na(pbf), !is.na(renda_exante), !is.na(formal_income_share),
         renda_exante >= 0, renda_exante <= L3)


data_formal_graph = base_formal_graph |>
  mutate(largura_bin = L3/20,
         bin_x = floor(renda_exante/largura_bin)*largura_bin,
         bin_center = bin_x + largura_bin/2,
         grupo_pbf = factor(pbf, levels = c(0, 1),
                            labels = c("Non-PBF", "PBF beneficiary"))) |>
  filter(bin_x < L3) |>
  group_by(year, grupo_pbf, bin_x, bin_center) |>
  summarise(formal_income_share = mean(formal_income_share),
            n_bin = n(), .groups = "drop")

cortes_formal = base_formal_graph |>
  distinct(year, L1, L2, L3)

anos_formal = sort(unique(data_formal_graph$year))

for (ano in anos_formal) {
  dados_ano = data_formal_graph |> filter(year == ano)
  cortes_ano = base_formal_graph |> filter(year == ano) |> distinct(L1, L2, L3)
  
  graph_formal = ggplot(dados_ano,
                        aes(x = bin_center, y = formal_income_share,
                            color = grupo_pbf, group = grupo_pbf)) +
    geom_point(size = 2.5) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 4, raw = TRUE),
                se = FALSE, linewidth = 0.8) +
    scale_color_manual(values = c("Non-PBF" = "#A65628",
                                  "PBF beneficiary" = "#174A73")) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       labels = label_number(decimal.mark = ",", accuracy = 0.1)) +
    coord_cartesian(xlim = c(0, cortes_ano$L3[1]), ylim = c(0, 0.9)) +
    labs(title = paste0("Percentage of formal household income — ", ano),
         x = "Household per-capita income (ex-ante of PBF)",
         y = "Percentage of formal household income", color = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  
  ggsave(paste0("formal_income_share_", ano, ".pdf"),
         plot = graph_formal, width = 8, height = 6, device = cairo_pdf)
}


base_informal_graph = pnadc_psm |>
  filter(year %in% years_graph) |>
  transmute(year, pbf = pbf_hh_final,
            renda_exante = ear_hh_e_exante_pbf_pc_post,
            informal_income_share = ear_hh_e_inf_share,
            L1 = L_pvrt, L2 = L_min_wage_half, L3 = L_min_wage) |>
  filter(!is.na(pbf), !is.na(renda_exante), !is.na(informal_income_share),
         renda_exante >= 0, renda_exante <= L3)

data_informal_graph = base_informal_graph |>
  mutate(largura_bin = L3/20,
         bin_x = floor(renda_exante/largura_bin)*largura_bin,
         bin_center = bin_x + largura_bin/2,
         grupo_pbf = factor(pbf, levels = c(0, 1),
                            labels = c("Non-PBF", "PBF beneficiary"))) |>
  filter(bin_x < L3) |>
  group_by(year, grupo_pbf, bin_x, bin_center) |>
  summarise(informal_income_share = mean(informal_income_share, na.rm = TRUE),
            n_bin = n(), .groups = "drop")



anos_informal = sort(unique(data_informal_graph$year))

for (ano in anos_informal) {
  dados_ano = data_informal_graph |> filter(year == ano)
  cortes_ano = base_informal_graph |> filter(year == ano) |> distinct(L1, L2, L3)
  
  graph_informal = ggplot(dados_ano,
                          aes(x = bin_center, y = informal_income_share,
                              color = grupo_pbf, group = grupo_pbf)) +
    geom_point(size = 2.5) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 4, raw = TRUE),
                se = FALSE, linewidth = 0.8) +
    scale_color_manual(values = c("Non-PBF" = "#A65628",
                                  "PBF beneficiary" = "#174A73")) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       labels = label_number(decimal.mark = ",", accuracy = 0.1)) +
    coord_cartesian(xlim = c(0, cortes_ano$L3[1]), ylim = c(0.1, 0.9)) +
    labs(title = paste0("Percentage of informal household income — ", ano),
         x = "Household per-capita income (ex-ante of PBF)",
         y = "Percentage of informal household income", color = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  
  ggsave(paste0("informal_income_share_", ano, ".pdf"),
         plot = graph_informal, width = 8, height = 6, device = cairo_pdf)
}




##################################### The last set of graphs ##################################################

base_composition_graph = pnadc_psm |>
  filter(year %in% years_graph, PSM_logit == 1) |>
  transmute(year,
            pbf = pbf_hh_final,
            renda_exante = ear_hh_e_exante_pbf_pc_post,
            formal = ear_hh_e_for_pc_post,
            informal = ear_hh_e_inf_pc_post,
            outras = ear_other_hh_pc,
            pbf_income = pbf_value_pc,
            L1 = L_pvrt,
            L2 = L_min_wage_half,
            L3 = L_min_wage) |>
  filter(!is.na(pbf), !is.na(renda_exante), !is.na(L3),renda_exante >= 0, renda_exante <= L3)

data_composition_graph = base_composition_graph |>
  mutate(largura_bin = L3/20,
         bin_x = floor(renda_exante/largura_bin)*largura_bin,
         bin_center = bin_x + largura_bin/2,
         grupo_pbf = factor(pbf,levels = c(0, 1),labels = c("Non-PBF", "PBF beneficiary"))) |>
  filter(bin_x < L3) |>
  group_by(year, grupo_pbf, bin_x, bin_center, L1, L2, L3) |>
  summarise(
    formal = mean(formal, na.rm = TRUE),
    informal = mean(informal, na.rm = TRUE),
    outras = mean(outras, na.rm = TRUE),
    pbf_income = mean(pbf_income, na.rm = TRUE),
    n_bin = n(),
    .groups = "drop") |>
  mutate(
    formal = formal/L3,
    informal = informal/L3,
    outras = outras/L3,
    pbf_income = pbf_income/L3
  ) |>
  pivot_longer(
    cols = c(formal, informal, outras, pbf_income),
    names_to = "componente",
    values_to = "income_min_wage"
  ) |>
  mutate(
    componente = factor(
      componente,
      levels = c("formal", "informal", "outras", "pbf_income"),
      labels = c("Formal income", "Informal income", "Others", "PBF")
    )
  ) |>
  filter(is.finite(income_min_wage))

dir.create("income_composition", showWarnings = FALSE)

anos_composition = sort(unique(data_composition_graph$year))

for (ano in anos_composition) {
  dados_ano = data_composition_graph |> filter(year == ano)
  cortes_ano = dados_ano |> distinct(L1, L2, L3)
  largura_ano = cortes_ano$L3[1]/20
  
  graph_composition = ggplot(
    dados_ano,
    aes(x = bin_center, y = income_min_wage, fill = componente)
  ) +
    geom_col(width = largura_ano*0.9) +
    geom_vline(data = cortes_ano, aes(xintercept = L1),
               inherit.aes = FALSE, color = "#E83E5B",
               linetype = "dashed", linewidth = 0.7) +
    geom_vline(data = cortes_ano, aes(xintercept = L2),
               inherit.aes = FALSE, color = "#E83E5B",
               linetype = "dashed", linewidth = 0.7) +
    facet_wrap(~ grupo_pbf, nrow = 1) +
    scale_fill_manual(
      values = c(
        "Formal income" = "#76964B",
        "Informal income" = "#37688E",
        "Others" = "#D22E55",
        "PBF" = "#FF8C1A"
      )
    ) +
    scale_x_continuous(
      limits = c(0, cortes_ano$L3[1]),
      breaks = seq(0, cortes_ano$L3[1], length.out = 6),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      breaks = seq(0, 1.2, 0.2),
      labels = label_number(decimal.mark = ",", accuracy = 0.1),
      expand = expansion(mult = c(0, 0.03))
    ) +
    coord_cartesian(ylim = c(0, 1.2)) +
    labs(
      title = paste0("Household income composition — ", ano),
      x = "Household per-capita income (ex-ante of PBF)",
      y = "Income/Minimum wage",
      fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(size = 13, face = "bold"),
          legend.position  = "bottom")
  ggsave(filename = paste0("income_composition/income_composition_", ano, ".pdf"),
         plot     = graph_composition,
         width    = 11,
         height   = 5.5,
         device   = cairo_pdf)
}
