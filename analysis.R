# Presets ----
setwd("C:/University/MS II/")
options(scipen = 999)    # disable scientific mode
library(tidyverse)
library(lubridate)
library(plotly)
library(ordinal)
library(brant)
library(VGAM)
library(boot)
library(tictoc)
df <- read_csv("dane.csv") # https://ess-search.nsd.no/en/study/172ac431-2a06-41df-9dab-c1fd8f3877e7

# Functions to reduce number of categories ----
farea <- function(x) {
  case_when(
    x == 1 ~ "city",
    x == 2 ~ "suburbs",
    x == 3 ~ "town",
    x == 4 ~ "village",
    x == 5 ~ "farm house",
    TRUE ~ "other"
  )
}

fpint <- function(x) {
  case_when(
    x == 1 ~ "very",
    x == 2 ~ "modern",
    x == 3 ~ "modern",
    x == 4 ~ "not at all",
    TRUE ~ "other"
  )
}

fvote <- function(x) {
  case_when(
    x == 1 ~ "yes",
    x == 2 ~ "no",
    TRUE ~ "other"
  )
}

finf <- function(x) {
  case_when(
    x == 1 ~ "no",
    x == 2 ~ "modern",
    x == 3 ~ "modern",
    x == 4 ~ "yes",
    x == 5 ~ "yes",
    TRUE ~ "other"
  )
}

ftrust <- function(x) {
  ifelse(x<2, "no", ifelse(x > 8 & x < 11, "yes", "other"))
}

fsoc <- function(x) {
  ifelse(x<=2, "no", ifelse(x %in% c(5, 6), "yes", "other"))
}

ftol <- function(x) {
  ifelse(x<=2, "yes", ifelse(x %in% c(4, 5), "no", "other"))
}

ftolim <- function(x) {
  ifelse(x<=1, "yes", ifelse(x %in% c(4), "no", "other"))
}

fsafe <- function(x) {
  ifelse(x %in% c(1, 2), "yes", ifelse(x %in% c(3, 4), "no", "other"))
}

fdisc <- function(x) {
  case_when(
    x == 1 ~ "yes",
    x == 2 ~ "no",
    TRUE ~ "other"
  )
}

fgendr <- function(x) {
  case_when(
    x == 1 ~ "male",
    x == 2 ~ "female",
    TRUE ~ "other"
  )
}

feduc <- function(x) {
  case_when(
    x == 0 ~ "other",
    x < 6 ~ "school",
    x == 6 ~ "BA",
    x == 7 ~ "MA",
    TRUE ~ "other"
  )
}

fempl <- function(x) {
  case_when(
    x == 1 ~ "employee",
    x <= 3 ~ "enterpreneur",
    x == 6 ~ "unemployed",
    TRUE ~ "other"
  )
}

fsize <- function(x) {
  case_when(
    x == 1 ~ "small",
    x < 5 ~ "medium",
    x == 5 ~ "big",
    TRUE ~ "other"
  )
}

fctype <- function(x) {
  case_when(
    x <= 3 ~ "goverment",
    x <= 5 ~ "private",
    TRUE ~ "other"
  )
}

fpclose <- function(x) {
  case_when(
    x <= 2 ~ "yes",
    x == 3 ~ "medium",
    x <= 5 ~ "no",
    TRUE ~ "other"
  )
}

fcountry <- function(x) {
  case_when(
    x %in% c("BE", "CH", "FR", "GB", "IE", "NL") ~ "western",
    x %in% c("BG", "CZ", "EE", "HR", "HU", "LT", "ME", "MK", "SK", "SI") ~ "eastern",
    x %in% c("GR", "IT", "PT") ~ "southern",
    x %in% c("FI", "IS", "NO") ~ "northern",
    TRUE ~ "other"
  )
}

fhinc <- function(x) {
  case_when(
    x <= 9 ~ "0",
    x <= 10 ~ "1",
    TRUE ~ "0"
  )
}

fage <- function(x) {
  case_when(
    x <= 17 ~ "child",
    x <= 24 ~ "young",
    x <= 35 ~ "young adult",
    x <= 45 ~ "adult",
    x <= 55 ~ "senior adult",
    x <= 65 ~ "pre-senior",
    TRUE ~ "senior"
  )
}

fage2 <- function(x) {
  case_when(
    x <= 17 ~ "child",
    x <= 35 ~ "young",
    x <= 45 ~ "adult",
    x <= 55 ~ "senior adult",
    TRUE ~ "senior"
  )
}

fage3 <- function(x) {
  case_when(
    x <= 17 ~ "child",
    x <= 35 ~ "young",
    x <= 55 ~ "adult",
    TRUE ~ "senior"
  )
}

fage4 <- function(x) {
  case_when(
    x <= 35 ~ "young",
    x <= 55 ~ "adult",
    TRUE ~ "senior"
  )
}

fage5 <- function(x) {
  case_when(
    x <= 35 ~ "1",
    TRUE ~ "0"
  )
}

# Initial variable selection ----
df <- df %>% 
  filter(vdcond != 9) %>% 
  select(
    party = lrscale,
    country = cntry,
    area = domicil,
    date = proddate,
    duration = inwtm,
    interview_type = vdcond,
    net_usage = netusoft,
    contacted_gov = contplt,
    politics_interested = polintr,
    public_demostration = pbldmna,
    voted = vote,
    influence_on_politics = psppipla,
    trust_people = ppltrst,
    trust_politics = trstplt,
    trust_scientists = trstsci,
    social = sclmeet,
    religion = rlgdnm,
    is_sitizen = ctzcntr,
    is_born = brncntr,
    year_started_living = livecnta, yrbrn,
    facntr, mocntr,
    satisifed = stflife,
    gov_satisfied = stfgov,
    economics_satisifed = stfeco,
    is_tolerant = freehms,
    for_integration = euftf,
    is_tolerant_imm = imdfetn,
    climate_change = ccnthum,
    free_media = medcrgv,
    is_safe = aesfdrk,
    is_descriminated = dscrgrp,
    gender = gndr,
    age = agea,
    hh_size = hhmmb,
    gndr2, rshpsts,
    educ = eisced,
    empl_type = emplrel,
    comp_size = estsz,
    comp_type = tporgwk,
    is_manager = jbspv,
    hh_income = hinctnta,
    emprf14, emprm14,
    parents_alive = livpnt, 
    live_with_parents = hhlipnt,
    close_to_parents = closepnt,
    is_conspirancy = secgrdec,
    satissfied_with_job = stfmjob
  )

# Reduce number of categories ----
to_chr_lst <- c(
  "contacted_gov", "is_sitizen", "is_born", "is_manager", "live_with_parents", "is_conspirancy", "wday",
  "month", "use_internet", "demonstrated", "is_religious", "is_pimmigrant", "is_climate_change", "is_single_raised",
  "is_parents_alive", "is_hs", "year"
)

df <- df %>% 
  mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
  mutate(
    is_interested = fpint(politics_interested),
    wday = wday(date),
    month = month(date),
    year = year(date),
    party_extra = ifelse(party %in% c(0, 10), "1", "0"),
    area = farea(area),
    use_internet = ifelse(net_usage == 1, 0, 1),
    contacted_gov = ifelse(contacted_gov == 1, 1, 0),
    demonstrated = ifelse(public_demostration == 1, 1, 0),
    voted = fvote(voted),
    influence = finf(influence_on_politics),
    trust_people = ftrust(trust_people),
    trust_politics = ftrust(trust_politics),
    trust_scientists = ftrust(trust_scientists),
    is_social = fsoc(social),
    is_religious = ifelse(religion > 8, 0, 1),
    is_sitizen = ifelse(is_sitizen == 1, 1, 0),
    is_born = ifelse(is_born == 1, 1, 0),
    years_lived = year_started_living-yrbrn,
    is_pimmigrant = ifelse(facntr==1|mocntr==1, 1, 0),
    is_satisfied = ftrust(satisifed),
    is_satisfied_gov = ftrust(gov_satisfied),
    is_satisfied_ec = ftrust(economics_satisifed),
    is_tolerant = ftol(is_tolerant),
    for_integration = ftrust(for_integration),
    is_tolerant_imm = ftolim(is_tolerant_imm),
    is_climate_change = ifelse(climate_change == 55, 0, 1),
    for_free_media = ftrust(free_media),
    is_safe = fsafe(is_safe),
    is_descriminated = fdisc(is_descriminated),
    gender = fgendr(gender),
    gender2 = fgendr(gndr2),
    educ = feduc(educ),
    empl_type = fempl(empl_type),
    comp_size = fsize(comp_size),
    comp_type = fctype(comp_type),
    is_manager = ifelse(is_manager == 1, 1, 0),
    is_single_raised = ifelse(emprf14 == 4 | emprm14 == 4, 1, 0),
    is_parents_alive = ifelse(parents_alive == 4, 0, 1),
    live_with_parents = ifelse(live_with_parents == 1, 1, 0),
    is_pclose = fpclose(close_to_parents),
    is_conspirancy = ifelse(is_conspirancy<=2, 1, 0),
    is_satisfied_job = ftrust(satissfied_with_job),
    .keep = "unused"
  ) %>% 
  mutate(
    is_hs = ifelse(gender == gender2 & rshpsts <= 4, 1, 0),
  ) %>% 
  mutate(across(to_chr_lst, as.character)) %>% 
  select(-gender2, -rshpsts) %>% 
  filter(is_interested != "other")

df <- df %>% 
  mutate(is_interested = ifelse(is_interested == "modern", "moderate", is_interested)) %>% 
  mutate(influence = ifelse(influence == "modern", "moderate", influence))

# EDA I ----
bar_plot <- function(var, df) {
  plt_data <- df %>% 
    group_by(across(all_of(c("is_interested", var)))) %>% 
    summarise(n = n()) %>%
    ungroup() %>% 
    left_join(group_by(., across(all_of(c("is_interested")))) %>% summarise(total = sum(n)), by = "is_interested") %>% 
    left_join(group_by(., across(all_of(c(var)))) %>% summarise(total2 = sum(n)), by = var) %>% 
    mutate(prop = round(n/total, 4)*100, .keep = "unused")
  
  p1 <- plt_data %>% filter(is_interested == "very")
  p2 <- plt_data %>% filter(is_interested == "moderate")
  p3 <- plt_data %>% filter(is_interested == "not at all") 
  subplot(
    p1 %>% plot_ly(x =~ .[[var]], y =~ prop, type = "bar", name = "very", text =~ prop) %>% layout(bargap = 0.1),
    p2 %>% plot_ly(x =~ .[[var]], y =~ prop, type = "bar", name = "moderate", text =~ prop) %>% layout(bargap = 0.1),
    p3 %>% plot_ly(x =~ .[[var]], y =~ prop, type = "bar", name = "not at all", text =~ prop) %>% layout(bargap = 0.1),
    nrows = 3
  ) %>% 
    layout(title = var)
}

get_fisher_pval <- function(x, y) {fisher.test(x, y, simulate.p.value=TRUE)$p.value} #

dff <- df %>% mutate_if(is.character, as.factor)
var_imp_tbl <- dff %>% 
  select_if(is.factor) %>% 
  select(-c(wday, month, year)) %>% 
  summarise_all(list(get_fisher_pval, DescTools::CramerV), y = df$is_interested) %>% 
  pivot_longer(colnames(.), names_to = "var", values_to = "p_val") %>% 
  mutate(
    tau_b = ifelse(str_ends(var, "\\_fn2"), p_val, NA),
    p_val = ifelse(str_ends(var, "\\_fn1"), p_val, NA)
  ) %>% 
  mutate(var = str_replace(var, "\\_fn1|\\_fn2", "")) %>% 
  group_by(var) %>% 
  summarise(p_val = sum(p_val, na.rm = TRUE), tau_b = sum(tau_b, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(tau_b) %>% 
  filter(var != "is_interested")

var_imp_tbl %>% filter(tau_b > 0.1) # the best variables according to tau b criterion



df %>% colnames() %>% sort()

df %>% summarise(me = mean(age), sd = sd(age), median = median(age), max = max(age), min = min(age))
df %>% summarise(me = mean(hh_size), sd = sd(hh_size), median = median(hh_size), max = max(hh_size), min = min(hh_size))

df %>% group_by(area) %>% summarise(n = n())

# zmienne osobiste: age, gender, hh_size, hh_income, is_parents_alive, is_pclose, is_single_raised, live_with_parents
# zmienne profesjonalne: comp_size, comp_type, empl_type, is_manager, educ
# status prawny: is_born, is_pimmigrant, is_sitizen
# poglady polityczne: for_free_media, for_integration, influence, is_conspirancy, is_satisfied_ec, is_satisfied_gov
# poglady zyciowe: is_religious, is_climate_change, is_satisfied, is_satisfied_job, is_tolerant, is_tolerant_imm, trust_people, trust_politics, trust_scientists, is_safe
# zaangazowanie polityczno-spoleczne: contacted_gov, demonstrated, is_interested, voted, use_internet, is_social, party_extra
# zmienne geograficzne: area, country




bar_plot("gender", df)
# country -- hard to say; reduce to regions
# interview_type -- not important
# contacted_gov -- important
# voted -- important
# trust_politics -- important, maybe unite other+yes
# is_tolerant_imm -- important
# educ -- important; unite MA+BA, other+school
# empl_type -- important; unite other + employee; create is_employed (may be better)
# comp_size -- hard to say; correlated mostly with empl_type, so try creating is_company_big var
# comp_type -- hard to say; rather keep
# is_manager -- important
# month, year, wday are not important as the do not have enough levels
# use_internet -- important
# demonstrated -- important
# influence -- important; unite other+moderate
# gender -- important

# is_single_raised -- not important
# is_climate_change -- not important; though 3 times more often says 'no' response within not interested at all
# area -- hard to say; suburbs and farmhouse here are important categories
# is_safe -- hard to say; those who consider their country not safe are 2 times more likely not to be interested in politics compared to those who are very interested
# is_satisfied_ec -- hard to say; recode to y|n  (no+other)
# is_satisfied_gov -- hard to say; recode to y|n (no+other)
# is_social -- hard to say; recode to y|n (yes+other)
# party_extra -- important
# trust_scientists -- important; may be corelated with politics
# for_free_media -- important; recode to y|n (no+other)
# is_tolerant -- also important; recode to y|n (no+other)
# live_with_parents -- hard to say
# trust_people -- hard to say

# Data preparation I ----
cols_num <- df %>% select_if(is.numeric) %>% colnames()
cols_chr <- c(
  "country", "contacted_gov", "voted", "trust_politics", "is_tolerant_imm", "educ", "empl_type",
  "is_manager", "use_internet", "demonstrated", "influence", 
  "area", "is_safe", "is_satisfied_ec", "is_satisfied_gov", "is_social", "comp_type", "comp_size",
  "party_extra", "trust_scientists", "for_free_media", "is_tolerant", "live_with_parents", "trust_people", "is_interested"
)

other_to_no <- function(x) {ifelse(x == "other", "no", x)}
other_to_yes <- function(x) {ifelse(x == "other", "yes", x)}

df2 <- df %>% 
  select(-any_of(c("interview_type", "is_single_raised", "is_climate_change", "wday", "month", "year", "is_hs"))) %>% 
  mutate(
    region = fcountry(country),
    trust_politics = other_to_no(trust_politics),
    trust_politics2 = other_to_yes(trust_politics),
    is_tolerant_imm2 = other_to_yes(is_tolerant_imm),
    is_high_educ = ifelse(educ %in% c("other", "school"), "0", "1"),
    educ = ifelse(educ %in% c("other", "school"), "primary", educ),
    is_employed = ifelse(empl_type == "unemployed", "0", "1"),
    empl_type = ifelse(empl_type %in% c("other"), "employee", empl_type),
    influence = ifelse(influence == "other", "moderate", influence),
    is_big = ifelse(comp_size == "big", "1", "0"),
    
    area = ifelse(area == "farm house", "farm house", ifelse(area == "suburbs", "suburbs", "other")),
    is_safe = other_to_no(is_safe),
    is_satisfied_ec2 = other_to_no(is_satisfied_ec),
    is_satisfied_gov2 = other_to_no(is_satisfied_gov),
    is_social = other_to_yes(is_social),
    for_free_media2 = other_to_yes(for_free_media),
    for_free_media = other_to_no(for_free_media),
    is_tolerant2 = other_to_no(is_tolerant),
    is_tolerant = other_to_yes(is_tolerant),
    is_satisfied_ec = other_to_yes(is_satisfied_ec),
    is_satisfied_gov = other_to_yes(is_satisfied_gov),
    is_female = ifelse(gender == "female", "1", "0")
  ) %>% 
  mutate(is_satisfied_pol = ifelse(is_satisfied_ec == "yes" | is_satisfied_gov == "yes", "yes", "no")) %>% 
  select(-gender)
# EDA II ----
dff <- df2 %>% mutate_if(is.character, as.factor)
var_imp_tbl <- dff %>% 
  select_if(is.factor) %>% 
  summarise_all(list(get_fisher_pval, DescTools::CramerV), y = df2$is_interested) %>% 
  pivot_longer(colnames(.), names_to = "var", values_to = "p_val") %>% 
  mutate(
    tau_b = ifelse(str_ends(var, "\\_fn2"), p_val, NA),
    p_val = ifelse(str_ends(var, "\\_fn1"), p_val, NA)
  ) %>% 
  mutate(var = str_replace(var, "\\_fn1|\\_fn2", "")) %>% 
  group_by(var) %>% 
  summarise(p_val = sum(p_val, na.rm = TRUE), tau_b = sum(tau_b, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(tau_b) %>% 
  filter(var != "is_interested")
var_imp_tbl %>% filter(tau_b >= 0.1)


df2 %>% select_if(is.character) %>% colnames()

bar_plot("is_satisfied_pol", df2)
# important: contacted_gov, is_safe, is_manager, live_with_parents, party_extra, use_internet, demonstrated, influence, is_social, for_integration
# is_satisfied_gov2, is_satisfied_ec2, for_free_media, region, is_tolerant_imm2, is_high_educ, is_employed, is_tolerant2
# not important: country, area, is_sitizen, is_born, trust_politics, is_descriminated, educ, empl_type, comp_size, is_conspirancy, is_religious, is_pimmigrant
# is_satisfied_gov, is_satisfied_ec, is_parents_alive, is_pclose, trust_politics2, is_big, for_free_media2, is_tolerant, is_satisfied_pol

# voted -- reduce to y|n (other+yes)
# trust_people -- recode to y|n (other+yes)
# trust_scientists -- recode to y|n (other+no|or other+yes)
# comp_type -- recode to is_gov
# is_satisfied -- recode to y|n (other+no)
# is_satisfied_job -- recode to y|n (other+no)

imp_lst <- c(
  "contacted_gov", "is_safe", "is_manager", "live_with_parents", "party_extra", "use_internet", "demonstrated", "influence", "is_social", "for_integration",
  "is_satisfied_gov2", "is_satisfied_ec2", "for_free_media", "region", "is_tolerant_imm2", "is_high_educ", "is_employed", "is_tolerant2", "is_female",
  # to recode
  "voted", "trust_people", "trust_scientists", "comp_type", "is_satisfied", "is_satisfied_job"
)

# Data preparation II ----
df3 <- df2 %>% 
  select(is_interested, all_of(c(df2 %>% select_if(is.numeric) %>% colnames(), imp_lst))) %>% 
  mutate(
    voted2 = other_to_yes(voted),
    trust_people = other_to_yes(trust_people),
    trust_scientists = other_to_no(trust_scientists),
    is_gov = ifelse(comp_type == "goverment", "1", "0"),
    is_satisfied = other_to_no(is_satisfied),
    is_satisfied_job = other_to_no(is_satisfied_job),
  ) %>% 
  select(-comp_type)
  
# EDA III ----
dff <- df3 %>% mutate_if(is.character, as.factor)
var_imp_tbl <- dff %>% 
  select_if(is.factor) %>% 
  summarise_all(list(get_fisher_pval, DescTools::CramerV), y = df3$is_interested) %>% 
  pivot_longer(colnames(.), names_to = "var", values_to = "p_val") %>% 
  mutate(
    tau_b = ifelse(str_ends(var, "\\_fn2"), p_val, NA),
    p_val = ifelse(str_ends(var, "\\_fn1"), p_val, NA)
  ) %>% 
  mutate(var = str_replace(var, "\\_fn1|\\_fn2", "")) %>% 
  group_by(var) %>% 
  summarise(p_val = sum(p_val, na.rm = TRUE), tau_b = sum(tau_b, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(tau_b) %>% 
  filter(var != "is_interested")
var_imp_tbl %>% filter(tau_b >= 0.1)


df3 %>% select_if(is.character) %>% colnames()

bar_plot("is_satisfied_job", df3)

# drop: voted, is_satisfied


# Data preparation III ----
df4 <- df3 %>% 
  select(-voted, -is_satisfied) %>% 
  rename(
    voted = voted2,
    is_tolerant = is_tolerant2,
    is_tolerant_imm = is_tolerant_imm2,
    is_satisfied_gov = is_satisfied_gov2,
    is_satisfied_ec = is_satisfied_ec2
  )

# Numeric EDA I ----
df4 %>% select_if(is.numeric) %>% colnames()

df_plt <- df4 %>% 
  mutate(
    age = ifelse(age>100, NA, age),
    duration = ifelse(duration>100, NA, duration),
    years_lived = ifelse(years_lived>100|years_lived<0, NA, years_lived),
    hh_size = ifelse(hh_size > 8, 8, hh_size)
  )

var <- 'hh_size'
subplot(
  df_plt %>% filter(is_interested == "very") %>% plot_ly(x =.[[var]], type = "histogram", name = "very", nbinsx = 30) %>% layout(bargap = 0.1),
  df_plt %>% filter(is_interested == "moderate") %>% plot_ly(x =.[[var]], type = "histogram", name = "moderate", nbinsx = 30) %>% layout(bargap = 0.1),
  df_plt %>% filter(is_interested == "not at all") %>% plot_ly(x =.[[var]], type = "histogram", name = "not at all", nbinsx = 30) %>% layout(bargap = 0.1),
  nrows = 2
)

df_plt %>% 
  group_by(is_interested) %>% 
  summarise(
    me = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    
    min = min(age, na.rm = TRUE),
    q25 = quantile(age, 0.25, na.rm = TRUE),
    med = median(age, na.rm = TRUE),
    q75 = quantile(age, 0.25, na.rm = TRUE),
    max = max(age, na.rm = TRUE),
    
    n_na = sum(is.na(age))
  )

# age -- important; try creating age groups instead
# duration -- not important
# years_lived -- not important; very 'dirty' variable, distributions seems the same
# hh_income -- hard to say; perform tests and try recoding to categorical
# hh_size -- hard to say; couples seems to be interested the most; singles and people with children the least => create 3 new categorical variables

# Data preparation IV ----
df5 <- df4 %>% 
  mutate(
    is_single = ifelse(hh_size == 1, "1", "0"),
    is_couple = ifelse(hh_size == 2, "1", "0"),
    is_parent = ifelse(hh_size >= 3, "1", "0"),
    age = ifelse(age>100, NA, age),
    is_rich = fhinc(hh_income),
    hh_income2 = ifelse(hh_income>10, 5, hh_income)
  ) %>% 
  mutate(
    age_cat = fage(age)
  ) %>% 
  select(-c(duration, hh_size))

# Numeric EDA II ----
dff <- df5 %>% mutate_if(is.character, as.factor)
var_imp_tbl <- dff %>% 
  select_if(is.factor) %>% 
  summarise_all(list(get_fisher_pval, DescTools::CramerV), y = df5$is_interested) %>% 
  pivot_longer(colnames(.), names_to = "var", values_to = "p_val") %>% 
  mutate(
    tau_b = ifelse(str_ends(var, "\\_fn2"), p_val, NA),
    p_val = ifelse(str_ends(var, "\\_fn1"), p_val, NA)
  ) %>% 
  mutate(var = str_replace(var, "\\_fn1|\\_fn2", "")) %>% 
  group_by(var) %>% 
  summarise(p_val = sum(p_val, na.rm = TRUE), tau_b = sum(tau_b, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(tau_b) %>% 
  filter(var != "is_interested")
var_imp_tbl %>% filter(tau_b >= 0.1)

bar_plot("age_cat", df5)
# is_rich -- important
# is_single -- not important
# is_couple -- somewhat important
# is_parent -- somewhat important
# age_cat -- reduce number of categories


kruskal.test(age ~ is_interested, data = df5)
df5 %>% plot_ly(y =~ age, x =~ is_interested, type = "box",  boxmean = TRUE)

kruskal.test(hh_income2 ~ is_interested, data = df5)
df5 %>% plot_ly(y =~ hh_income2, x =~ is_interested, type = "box",  boxmean = TRUE)
# keep age and hh_income as numeric columns for now

# Data preparation V ----
df6 <- df5 %>% 
  mutate(
    age_cat2 = fage2(age),
    age_cat3 = fage3(age),
    age_cat4 = fage4(age),
    age_cat5 = fage5(age)
  ) %>% 
  select(-c(is_single, hh_income))

# Numeric EDA III ----
dff <- df6 %>% mutate_if(is.character, as.factor)
var_imp_tbl <- dff %>% 
  select_if(is.factor) %>% 
  summarise_all(list(get_fisher_pval, DescTools::CramerV), y = df6$is_interested) %>% 
  pivot_longer(colnames(.), names_to = "var", values_to = "p_val") %>% 
  mutate(
    tau_b = ifelse(str_ends(var, "\\_fn2"), p_val, NA),
    p_val = ifelse(str_ends(var, "\\_fn1"), p_val, NA)
  ) %>% 
  mutate(var = str_replace(var, "\\_fn1|\\_fn2", "")) %>% 
  group_by(var) %>% 
  summarise(p_val = sum(p_val, na.rm = TRUE), tau_b = sum(tau_b, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(tau_b) %>% 
  filter(var != "is_interested")
var_imp_tbl %>% filter(tau_b >= 0.1)

bar_plot("age_cat5", df6)
# age_cat5 (is young) is the best representative

# Data preparation VI ----
recode_01 <- function(x) {ifelse(x=="no", "0", "1")}

df7 <- df6 %>% 
  select(-c(age_cat, age_cat2, age_cat3, age_cat4)) %>% 
  select(any_of(c(var_imp_tbl %>% filter(tau_b >= 0.1) %>% pull(var), "is_interested", "age", "hh_income2"))) %>%
  rename(is_young = age_cat5, hh_income = hh_income2) %>% 
  mutate(across(c("for_free_media", "trust_people", "is_tolerant", "is_tolerant_imm", "is_tolerant"), recode_01))
  
lapply(df7 %>% select(-c(age, hh_income)), function(x){unique(x) %>% sort()})


# Sparsity ----
df7 %>% 
  select(-c(is_young, is_rich, age, hh_income)) %>% 
  mutate_all(n_distinct) %>% 
  dplyr::slice(1) %>% 
  pivot_longer(colnames(.)) %>% 
  summarise(value = prod(value)) %>% 
  pull(value) %>% 
  `*`(10)
# theoretically, about 1 474 560 observations so that each unique group would contain at least 10 observations

df7 %>% 
  group_by_if(is.character) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(n, everything()) %>% 
  arrange(n()) %>% 
  summarise(
    n_bigger10 = nrow(filter(., n>10)),
    n = nrow(.)
  ) %>% 
  mutate(prop = round(n_bigger10/n, 4)*100)
# very sparse dataset => feature selection based on tau-b > 0.1

dff <- df7 %>% filter(is_interested != "other") %>% mutate_if(is.character, as.factor) 
dff %>% 
  select(-is_rich, -is_young) %>%
  select_if(is.factor) %>% 
  summarise_all(list(get_fisher_pval, DescTools::CramerV), y = df7$is_interested) %>% 
  pivot_longer(colnames(.), names_to = "var", values_to = "p_val") %>% 
  mutate(
    tau_b = ifelse(str_ends(var, "\\_fn2"), p_val, NA),
    p_val = ifelse(str_ends(var, "\\_fn1"), p_val, NA)
  ) %>% 
  mutate(var = str_replace(var, "\\_fn1|\\_fn2", "")) %>% 
  group_by(var) %>% 
  summarise(p_val = sum(p_val, na.rm = TRUE), tau_b = sum(tau_b, na.rm = TRUE)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  arrange(tau_b) %>% 
  filter(var != "is_interested") %>% 
  filter(tau_b > 0.10)

df7 %>% 
  select(-c(is_rich, is_young, demonstrated, use_internet)) %>% 
  select(-c(for_free_media, is_gov, trust_people)) %>% 
  select(-c(is_tolerant)) %>% 
  select(-c(is_tolerant_imm)) %>% 
  group_by_if(is.character) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(n, everything()) %>% 
  arrange(n()) %>% 
  summarise(
    n_bigger10 = nrow(filter(., n>10)),
    n = nrow(.)
  ) %>% 
  mutate(prop = round(n_bigger10/n, 4)*100)

# Functions for the next parts ----
show_r_sq <- function(obj){
  r2_mcf <- 1-(logLik(obj)/logLik(update(obj, .~1)))
  r2_mcfa <- 1-((logLik(obj) - obj$edf)/logLik(update(obj, .~1)))
  
  df <- tibble(
    Measure = c("McFadden R2", "McFadden R2 (Adj)"),
    Value = c(r2_mcf, r2_mcfa)
  )
  return(df)
}

finterested <- function(x){
  case_when(
    x == 1 ~ "not at all",
    x == 2 ~ "moderate",
    TRUE ~ "very"
  )
}

finterested_rev <- function(x){
  case_when(
    x == "not at all" ~ 1,
    x == "moderate" ~ 2,
    TRUE ~ 3
  )
}

show_summary <- function(m) {
  summary_table <- coef(summary(m))
  pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
  summary_table <- cbind(summary_table, "p value" = round(pval,3))
  summary_table %>% 
    as_tibble(rownames = "Variable") %>% 
    mutate_if(is.numeric, round, 4) %>% 
    `colnames<-`(c("variable", "coef", "std_error", "t_val", "p_val"))
}

show_accuracy <- function(df, model) {
  
  pred_type <- ifelse(class(model) %in% c("vglm"), "response", "prob")
  
  pred <- predict(model, type = pred_type) %>% apply(., 1, which.max) %>% finterested()
  
  if ("age" %in% colnames(df)) {
    actual <- df %>% filter(!is.na(age)) %>% pull(is_interested)
  } else {
    actual <- df %>% pull(is_interested)
  }
  pred <- factor(pred, levels = c("not at all", "moderate", "very"))
  actual <- factor(actual, levels = c("not at all", "moderate", "very"))
  
  acc_tbl <- table(actual, pred) %>% caret::confusionMatrix()
  
  model_name <- deparse(substitute(model))
  
  print(table(actual, pred))
  
  acc_tbl$byClass %>% 
    as_tibble(rownames = "Class") %>% 
    select(-c(`Pos Pred Value`, `Neg Pred Value`, Prevalence, `Detection Rate`, `Detection Prevalence`)) %>% 
    mutate(Class = str_replace(Class, "Class: ", "")) %>% 
    mutate(
      `Total Accuracy` = acc_tbl$overall[1],
      Model = model_name
    )
}

show_roc_auc <- function(df, model) {
  pred_type <- ifelse(class(model) %in% c("vglm"), "response", "prob")
  pred <- predict(model, type = pred_type) %>% apply(., 1, which.max)
  names(pred) <- NULL
  if ("age" %in% colnames(df)) {
    actual <- df %>% filter(!is.na(age)) %>% pull(is_interested)
  } else {
    actual <- df %>% pull(is_interested)
  }
  actual <- factor(actual, levels = c("not at all", "moderate", "very")) %>% as.character() %>% finterested_rev()
  
  rs_lst <- list()
  
  for (i in 1:3) {
    tmp_pred = ifelse(pred == i, 1, 0)
    tmp_actual = ifelse(actual == i, 1, 0)
    rs_lst[[i]] <- pROC::roc(tmp_pred, tmp_actual)
  }
  
  par(mfrow=c(2,2))
  pROC::plot.roc(rs_lst[[1]], main="not at all")
  pROC::plot.roc(rs_lst[[2]], main="moderate")
  pROC::plot.roc(rs_lst[[3]], main="very")
  
  auc_scores <- map_dbl(rs_lst, function(x) {x$auc %>% str_replace("Area under the curve: ", "") %>% as.numeric() %>% round(4)})
  
  tibble(class = c("not at all", "moderate", "very"), AUC = auc_scores)
}
# Functional form selection ----
df_prepr <- df7 %>%
  mutate(is_interested = factor(is_interested, levels = c("not at all", "moderate", "very"))) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-c(is_young, is_rich))

df700 <- df_prepr
df70 <- df_prepr %>% select(-c(demonstrated))
df71 <- df_prepr %>% select(-c(demonstrated, is_female))
df72 <- df_prepr %>% select(-c(demonstrated, is_female, use_internet))
df73 <- df_prepr %>% select(-c(demonstrated, is_female, use_internet, for_free_media))
df74 <- df_prepr %>% select(-c(demonstrated, is_female, use_internet, for_free_media, is_gov))
df75 <- df_prepr %>% select(-c(demonstrated, is_female, use_internet, for_free_media, is_gov, trust_people))
df76 <- df_prepr %>% select(-c(demonstrated, is_female, use_internet, for_free_media, is_gov, trust_people, is_tolerant))
df77 <- df_prepr %>% select(-c(demonstrated, is_female, use_internet, for_free_media, is_gov, trust_people, is_tolerant, is_tolerant_imm))

m700 <- clm(is_interested ~ ., data = df700, Hess = TRUE)
m70 <- clm(is_interested ~ ., data = df70, Hess = TRUE)
m71 <- clm(is_interested ~ ., data = df71, Hess = TRUE)
m72 <- clm(is_interested ~ ., data = df72, Hess = TRUE)
m73 <- clm(is_interested ~ ., data = df73, Hess = TRUE)
m74 <- clm(is_interested ~ ., data = df74, Hess = TRUE)
m75 <- clm(is_interested ~ ., data = df75, Hess = TRUE)
m76 <- clm(is_interested ~ ., data = df76, Hess = TRUE)
m77 <- clm(is_interested ~ ., data = df77, Hess = TRUE)

summary(m700)
summary(m70)
summary(m71)
summary(m72)
summary(m73)
summary(m74)
summary(m75)
summary(m76)
summary(m77)
# every coef is important

show_r_sq(m700)
show_r_sq(m70)
show_r_sq(m71)
show_r_sq(m72)
show_r_sq(m73)
show_r_sq(m74)
show_r_sq(m75)
show_r_sq(m76)
show_r_sq(m77)
# m700 seems to have the biggest pseudo-R^2; however, it is not the same as 'normal' R^2, usually alues between 0.2-0.4 are considered a good fit

AIC(m700, m70, m71, m72, m73, m74, m75, m76, m77)
BIC(m700, m70, m71, m72, m73, m74, m75, m76, m77)
# the best AIC, BIC are also for m700

lmtest::lrtest(m70, m700) # nested, complex
lmtest::lrtest(m71, m700)
lmtest::lrtest(m72, m700)
lmtest::lrtest(m73, m700)
lmtest::lrtest(m74, m700)
lmtest::lrtest(m75, m700)
lmtest::lrtest(m76, m700)
lmtest::lrtest(m77, m700)
# null hypothesis rejected => we should definitely use the complex model as it increases the accuracy of our model by a statistically substantial amount


# Brant test ----
OL <- MASS::polr(is_interested ~ ., data = df700, Hess = TRUE) # recalc in MASS package to perform brant test
brant(OL)  # which vars do not hold the Parallel Regression Assumption

best_data <- df700
best_data$is_interested <- best_data$is_interested %>% as.ordered()
best_model <- vglm(
  is_interested ~ .,
  family = cumulative(
    parallel = FALSE ~ is_female+use_internet+for_free_media+is_gov+trust_people+is_tolerant_imm+region+is_employed+is_high_educ+contacted_gov+influence+hh_income,
    reverse = FALSE
  ), 
  data = best_data
)

# Exploring best model ----
best_summary <- summary(best_model)
best_summary
exp(coef(best_model, matrix = TRUE))

show_accuracy(best_data, best_model) # in-sample
show_roc_auc(best_data, best_model)  # in-sample

# Calculate bootstrapped coefs ----
bootstrap_coef <- function(df) {
  i <- sample(nrow(df), size = nrow(df), replace = TRUE)
  m <- vglm(
    is_interested ~ .,
    family = cumulative(
      parallel = FALSE ~ is_female+use_internet+for_free_media+is_gov+trust_people+is_tolerant_imm+region+is_employed+is_high_educ+contacted_gov+influence+hh_income,
      reverse = FALSE),
    data = df[i, ]
  )
  sm <- summary(m)
  return(sm@coef3 %>% as_tibble(rownames = "var") %>% select(var, coef = Estimate, se = `Std. Error`))
}

set.seed(42)
max_iter <- 200
tic()
bootstrap_res <- list()
for (i in 1:max_iter) {
  bootstrap_res[[i]] <- bootstrap_coef(best_data)
}
toc()
# around 30 min

# Show difference in methods ----
coef_tbl <- data.table::rbindlist(bootstrap_res, use.names = FALSE) %>% 
  as_tibble()


df_orig <- best_summary@coef3 %>% as_tibble(rownames = "var") %>% select(var, coef = Estimate, se = `Std. Error`)# %>% mutate(type = "orig")
df_boot <- coef_tbl %>% group_by(var) %>% summarise(mean = mean(coef), sd = sd(coef))# %>% mutate(type = "boot")
df_diff <- df_orig %>% 
  left_join(df_boot, by = "var") %>% 
  mutate(
    co_diff = round((mean-coef)/coef, 4)*100,
    se_diff = round((sd-se)/se, 4)*100
  )


df_diff %>% plot_ly(y = ~co_diff, x = ~var, type = "bar", text = ~co_diff) %>% layout(yaxis = list(title = "% difference"), xaxis = list(title = "Variable"))
df_diff %>% plot_ly(y = ~se_diff, x = ~var, type = "bar", text = ~se_diff) %>% layout(yaxis = list(title = "% difference"), xaxis = list(title = "Variable"))
# Marginal effects ----
coef_final_tbl <- coef_tbl %>% 
  group_by(var) %>% 
  summarise(coef = mean(coef), se = mean(se)) %>% 
  mutate(
    type = ifelse(str_detect(var, "\\:2"), "P(y<=2)", NA_character_),
    type = ifelse(str_detect(var, "\\:1"), "P(y<=1)", type),
    type = ifelse(is.na(type), "both", type),
  ) %>% 
  mutate(var = str_replace(var, "\\:2|\\:1", "")) %>% 
  group_by(var, type) %>% 
  summarise(coef = sum(coef), se = sum(se)) %>% 
  ungroup() %>% 
  select(-se) %>% 
  pivot_wider(names_from = "type", values_from = c("coef")) %>% 
  mutate(
    `P(y<=1)` = ifelse(is.na(`P(y<=1)`), both, `P(y<=1)`),
    `P(y<=2)` = ifelse(is.na(`P(y<=2)`), both, `P(y<=2)`)
  ) %>% 
  select(-both) %>% 
  mutate(
    `P(y<=1)` = round(exp(`P(y<=1)`), 4)*100,
    `P(y<=2)` = round(exp(`P(y<=2)`), 4)*100
  ) %>% 
  mutate(var = str_replace(var, "1|yes", "(tak)"))
coef_final_tbl$`P(y<=2)`

se_final_tbl <- coef_tbl %>% 
  group_by(var) %>% 
  summarise(coef = mean(coef), se = mean(se)) %>% 
  mutate(
    type = ifelse(str_detect(var, "\\:2"), "P(y<=2)", NA_character_),
    type = ifelse(str_detect(var, "\\:1"), "P(y<=1)", type),
    type = ifelse(is.na(type), "both", type),
  ) %>% 
  mutate(var = str_replace(var, "\\:2|\\:1", "")) %>% 
  group_by(var, type) %>% 
  summarise(coef = sum(coef), se = sum(se)) %>% 
  ungroup() %>% 
  select(-coef) %>% 
  pivot_wider(names_from = "type", values_from = c("se")) %>% 
  mutate(
    `P(y<=1)` = ifelse(is.na(`P(y<=1)`), both, `P(y<=1)`),
    `P(y<=2)` = ifelse(is.na(`P(y<=2)`), both, `P(y<=2)`)
  ) %>% 
  select(-both) %>% 
  mutate(
    `P(y<=1)` = round(exp(`P(y<=1)`), 4)*100,
    `P(y<=2)` = round(exp(`P(y<=2)`), 4)*100
  ) %>% 
  mutate(var = str_replace(var, "1|yes", "(tak)"))
se_final_tbl$`P(y<=1)`
se_final_tbl$`P(y<=2)`
