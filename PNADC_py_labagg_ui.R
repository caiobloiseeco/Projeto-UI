######################################## Libraries #####################################################################

library(zoo)
library(tidyverse)
library(data.table)
library(modelsummary)
library(pdftools)
library(ggplot2)
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


# pnadc_q_pd = pnadc_q_pd |>
#   mutate(bpc_main = case_when(
#     bpc %in% 1 | bpc_two %in% 1 ~ 1L,  
#     bpc_two %in% 2              ~ 0L,   
#     TRUE                        ~ NA_integer_)) %>%  
#   select(-c(bpc, bpc_two))
# gc()
# 
# 
# pnadc_q_pd = pnadc_q_pd |>
#   mutate(othesoci_main = case_when(
#     othesoci     %in% 1 | othesoci_two %in% 1 ~ 1L,  
#     othesoci_two %in% 2                       ~ 0L,   
#     TRUE                                      ~ NA_integer_)) %>%  
#   select(-c(othesoci, othesoci_two))
# gc()


pnadc_q_pd = pnadc_q_pd |>
  mutate(ui_value_main       = coalesce(ui_value,  ui_value_two),
         bpc_value_main      = coalesce(bpc_value, bpc_value_two),
         othesoci_value_main = coalesce(othesoci_value, othesoci_value_two)) %>% 
  select(-c(ui_value,  
            ui_value_two, 
            bpc_value, 
            bpc_value_two, 
            othesoci_value, 
            othesoci_value_two,
            othesoci,
            othesoci_two,
            bpc,
            bpc_two,
            bpc_value_main,
            othesoci_value_main))
gc()


pnadc_q_pd = pnadc_q_pd |> 
  mutate(hours_main_e = coalesce(hours_main_e_one, hours_main_e_two)) %>% 
  select(-c(hours_main_e_one, 
            hours_main_e_two))
gc()


######################################### Creating some variables about formal and informal employment (by CNPJ and social security) ########################################


pnadc_q_pd = pnadc_q_pd |>
  mutate(laborforce = case_when(labor_force == 1 ~ 1L,
                                labor_force == 2 ~ 0L, TRUE ~ NA_integer_),
         occupied   = case_when(unemp == 1 ~ 1L,
                                unemp == 2 ~ 0L, TRUE ~ NA_integer_),
         unempl     = case_when(unemp == 2 ~ 1L,
                                unemp == 1 ~ 0L, TRUE ~ NA_integer_),
         informal   = case_when(unemp == 1 & (    coalesce(priv_inf            == 1, FALSE) |
                                                  coalesce(empdom_inf          == 1, FALSE) |
                                                  coalesce(emppub_inf          == 1, FALSE) |
                                                  coalesce(selfemp_inf_cnpj    == 1, FALSE) ) ~ 1L, 
                                unemp == 1 ~ 0L, TRUE ~ NA_integer_),
         formal     = case_when(unemp == 1 & informal == 0L ~ 1L,
                                unemp == 1 & informal == 1L ~ 0L, TRUE ~ NA_integer_),
         state_s     = case_when(formal   == 1L ~ 1L,
                                 informal == 1L ~ 2L,
                                 unempl   == 1L ~ 3L,
                                 TRUE ~ NA_integer_))
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



##################################### Filters ##########################################################################

## age and hours ##

pnadc_q_pd = pnadc_q_pd |>
  filter(age >= 25 & age < 65,
         hours_main_e >= 15 | is.na(hours_main_e))
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

## filter by wage_e and earnings

pnadc_q_pd = pnadc_q_pd |>
  filter(wage_e > 0     | is.na(wage_e)) |> 
  filter(ear_ind_e > 0  | is.na(ear_ind_e))
gc()

## filter by formal and UI

pnadc_q_pd = pnadc_q_pd %>%
  filter(!(ui_main == 1 & state_s == 1) | is.na(ui_main) | is.na(state_s))
gc()


##################################### Selecting Key Variables ########################################

pnadc_q_pd = pnadc_q_pd |>
  select(indid,
         houseid,
         date_q, 
         state_s,
         weight_1y,
         weight_2y,
         weight_3y,
         weight_4y,
         lhs, 
         hs,
         coll,
         age, 
         year, 
         quarter,
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
         wage_all_e,
         selfemp_tot,
         selfemp_inf_cnpj,
         selfemp_for_cnpj)
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


############################################### Starting some analysis ############################################

###############################################
## Labor market shares: unconditional
###############################################

labor_shares = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  summarise(share_formal     = weighted.mean(as.integer(state_s == 1),w = weight_4y),
            share_informal   = weighted.mean(as.integer(state_s == 2),w = weight_4y),
            share_unemployed = weighted.mean(as.integer(state_s == 3),w = weight_4y))
print(labor_shares)

###############################################
# Labor market shares by quarter
###############################################

labor_shares_q = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  group_by(year, quarter) |>
  summarise(share_formal     = weighted.mean(as.integer(state_s == 1),w = weight_4y),
            share_informal   = weighted.mean(as.integer(state_s == 2),w = weight_4y),
            share_unemployed = weighted.mean(as.integer(state_s == 3),w = weight_4y)) |>
  mutate(time = year + (quarter - 1) / 4) |>
  arrange(year, quarter)
print(labor_shares_q)


###############################################
# Self-employment
###############################################

self_employment_q = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  filter(selfemp_tot == 1) |>
  group_by(year, quarter) |>
  summarise(share_formal     = weighted.mean(as.integer(selfemp_for_cnpj == 1), w = weight_4y),
            share_informal   = weighted.mean(as.integer(selfemp_inf_cnpj == 1), w = weight_4y)) |>
  mutate(time = year + (quarter - 1) / 4) |>
  arrange(year, quarter)
print(self_employment_q)


#### ___ Plots: Labor shares evolution and Self employment ___ ####

ggplot(labor_shares_q, aes(x = time, y = 100*share_unemployed)) +
  geom_line(linewidth = 1.5,color = "black") +
  scale_x_continuous(breaks = seq(2016, 2019.75, by = 0.25),labels = function(x) {quarter = round((x - floor(x)) * 4) + 1
  paste0("Q", quarter,"\n",ifelse(quarter == 2, floor(x), ""))}) +
  labs(title    = "Unemployment rate (%)",
       caption  = "Source: PNADC - IBGE", 
       x        = "Date", 
       y        = "Unemployment rate (%)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 10,lineheight = 1.5))
ggsave("labagg_unemp.pdf")

ggplot(labor_shares_q, aes(x = time, y = 100*share_formal)) +
  geom_line(linewidth = 1.5,color = "black") +
  scale_x_continuous(breaks = seq(2016, 2019.75, by = 0.25),labels = function(x) {quarter = round((x - floor(x)) * 4) + 1
  paste0("Q", quarter,"\n",ifelse(quarter == 2, floor(x), ""))}) +
  labs(title    = "Formality rate (%)",
       caption  = "Source: PNADC - IBGE", 
       x        = "Date", 
       y        = "Formality rate (%)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 10,lineheight = 1.5))
ggsave("labagg_formal.pdf")

ggplot(labor_shares_q, aes(x = time, y = 100*share_informal)) +
  geom_line(linewidth = 1.5,color = "black") +
  scale_x_continuous(breaks = seq(2016, 2019.75, by = 0.25),labels = function(x) {quarter = round((x - floor(x)) * 4) + 1
  paste0("Q", quarter,"\n",ifelse(quarter == 2, floor(x), ""))}) +
  labs(title    = "Informality rate (%)",
       caption  = "Source: PNADC - IBGE", 
       x        = "Date", 
       y        = "Informality rate (%)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 10,lineheight = 1.5))
ggsave("labagg_informal.pdf")


ggplot(self_employment_q, aes(x = time, y = 100*share_informal)) +
  geom_line(linewidth = 1.5,color = "black") +
  scale_x_continuous(breaks = seq(2016, 2019.75, by = 0.25),labels = function(x) {quarter = round((x - floor(x)) * 4) + 1
  paste0("Q", quarter,"\n",ifelse(quarter == 2, floor(x), ""))}) +
  labs(title = "Informality rate of self-employment (%)",
       caption = "Source: PNADC - IBGE",
       x = "Date",
       y = "Informal self-employment rate (%)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 10,lineheight = 1.5))
ggsave("labagg_self_informal.pdf")


ggplot(self_employment_q,aes(x = time, y = 100*share_formal)) +
  geom_line(linewidth = 1.5,color = "black") +
  scale_x_continuous(breaks = seq(2016, 2019.75, by = 0.25),labels = function(x) {quarter = round((x - floor(x)) * 4) + 1
  paste0("Q", quarter,"\n",ifelse(quarter == 2, floor(x), ""))}) +
  labs(title = "Formality Rate of self-employment (%)",
     caption = "Source: PNADC - IBGE",
           x = "Date",
           y = "Formal Self-employment rate (%)") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size = 10,lineheight = 1.5))
ggsave("labagg_self_formal.pdf")



######################## Now, let's do it the analyses by educational share ######################


### Educational Shares

educ_shares = pnadc_q_pd |>
  filter(educ %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  summarise(share_lhs  = weighted.mean(as.integer(educ == 1), w = weight_4y),
            share_hs   = weighted.mean(as.integer(educ == 2), w = weight_4y),
            share_coll = weighted.mean(as.integer(educ == 3), w = weight_4y))
print(educ_shares)


labor_shares_lhs = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  filter(lhs == 1) |> 
  summarise(share_formal     = weighted.mean(as.integer(state_s == 1),w = weight_4y),
            share_informal   = weighted.mean(as.integer(state_s == 2),w = weight_4y),
            share_unemployed = weighted.mean(as.integer(state_s == 3),w = weight_4y))
print(labor_shares_lhs)


labor_shares_hs = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  filter(hs == 1) |>
summarise(share_formal     = weighted.mean(as.integer(state_s == 1),w = weight_4y),
          share_informal   = weighted.mean(as.integer(state_s == 2),w = weight_4y),
          share_unemployed = weighted.mean(as.integer(state_s == 3),w = weight_4y))
print(labor_shares_hs)


labor_shares_coll = pnadc_q_pd |>
  filter(state_s %in% 1:3, !is.na(weight_4y), weight_4y > 0) |>
  filter(coll == 1) |>
summarise(share_formal     = weighted.mean(as.integer(state_s == 1), w = weight_4y),
          share_informal   = weighted.mean(as.integer(state_s == 2), w = weight_4y),
          share_unemployed = weighted.mean(as.integer(state_s == 3), w = weight_4y))
print(labor_shares_coll)




############################# Now, we are going to do the analysis about UI #######################



###### Share of UI recipients working formally, informally or unemployed #########


ui_formal = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s), !is.na(weight_4y), weight_4y > 0) |>
  summarise(share_formal = weighted.mean(as.integer(state_s == 1),   w = weight_4y),  .groups = "drop")
print(ui_formal)
gc()

ui_informal = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s), !is.na(weight_4y), weight_4y > 0) |>
  summarise(share_informal = weighted.mean(as.integer(state_s == 2), w = weight_4y),.groups = "drop")
print(ui_informal)
gc()

ui_unemployment = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s), !is.na(weight_4y), weight_4y > 0) |>
  summarise(share_unemp = weighted.mean(as.integer(state_s == 3),    w = weight_4y),   .groups = "drop")
print(ui_unemployment)
gc()


ui_informal_time = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s),  !is.na(weight_4y), weight_4y > 0) |>
  group_by(year, quarter) |>
  summarise(share_informal = weighted.mean(as.integer(state_s == 2), w = weight_4y),.groups = "drop") |>
  mutate(time = year + (quarter - 1) / 4)
gc()



ggplot(ui_informal_time, aes(x = time, y = 100 * share_informal)) +
  geom_line(linewidth = 1.5, color = "black") +
  scale_x_continuous(breaks = seq(2016, 2019.75, by = 0.25),
  labels = function(x) {quarter = round((x - floor(x))*4) + 1 
  paste0("Q", quarter, "\n",ifelse(quarter == 2, floor(x), ""))}) +
  labs(title = "Share of UI takers in informality (%)",
    subtitle = "By quarters",
     caption = "Source: PNADC - IBGE",
           x = NULL,
           y = "Share of informal UI takers (%)") +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 16, lineheight = 1.5), plot.title = element_text(face = "bold"))
ggsave("labagg_ui_informal.pdf",width = 9, height = 6, units = "in")
gc()



ui_informal_lhs = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s), !is.na(weight_4y), weight_4y > 0, lhs == 1) |>
  summarise(share_informal = weighted.mean(as.integer(state_s == 2), w = weight_4y),.groups = "drop")
print(ui_informal_lhs)
gc()


ui_informal_hs = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s), !is.na(weight_4y), weight_4y > 0, hs == 1) |>
  summarise(share_informal = weighted.mean(as.integer(state_s == 2), w = weight_4y),.groups = "drop")
print(ui_informal_hs)
gc()


ui_informal_coll = pnadc_q_pd |>
  filter(ui_main == 1, !is.na(state_s), !is.na(weight_4y), weight_4y > 0, coll == 1) |>
  summarise(share_informal = weighted.mean(as.integer(state_s == 2), w = weight_4y),.groups = "drop")
print(ui_informal_coll)
gc()


##################### Generating a dataframe to plot a bar graph ###########


ui_informal_educ = tibble(
  education = factor(c("All", "LHS", "HS", "COLL"),levels = c("All", "LHS", "HS", "COLL")),
  share = 100*c(ui_informal$share_informal,
                ui_informal_lhs$share_informal,
                ui_informal_hs$share_informal,
                ui_informal_coll$share_informal))
print(ui_informal_educ)



###############################################
# Bar plot: UI recipients in informal employment
###############################################

ggplot(ui_informal_educ,aes(x = education, y = share)) +
  geom_col(aes(fill = education),width = 0.65 ) +
  scale_fill_manual(values = c("All" = "black","LHS" = "red","HS" = "red","COLL" = "red"),guide = "none") +
  geom_text(aes(label = sprintf("%.1f%%", share)),vjust = -0.5,size = 4.5) +
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, 10)) +
  labs(title = "Share of UI takers in informality (%)",
    subtitle = "All individuals and by educational level",
    caption = "Source: PNADC - IBGE",
    x = NULL,
    y = "Share of informal UI takers (%)") +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(size = 16),plot.title = element_text(face = "bold"))
ggsave("labagg_ui_informal_educ.pdf",width = 9,height = 6)
gc()





























































































