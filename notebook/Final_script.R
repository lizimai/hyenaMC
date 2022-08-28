library(hyenaR)
library(hyenaMC)
library(tidyverse)
library(lubridate)

# Load the newest sqlite database here: change the path
load_package_database.full("~/Desktop/MEME/Work/Projects/Berlin Hyena/Database/Fisidata25092020.sqlite")

# Conceptions -------------
# find all the females in the main clans, used for basic data extraction
females <- find_pop_id.female.all(main.clans = TRUE)

# conceptions_raw
set.seed(123)
create_offspring_litterID.from.all(females) %>% #create offspring starting table
  filter(filiation != "mother_social_only" | is.na(filiation)) %>%  # keep only genetic offspring
  mutate(maleID = fetch_id_id.father(offspringID), # focal male is the father of the offspring
         date = fetch_id_date.birth(offspringID) - 110, # date of conception is considered as 110 days before the birthdate of offspring
         clan = fetch_id_clan.current(parentID, date)) %>% # fetch female's clan on the date of conception
  filter(!is.na(date), date >= find_pop_date.observation.first()) %>% # remove conception with unknown date or before the start of the study
  rename(femaleID = parentID) %>%
  select(femaleID, maleID, date, clan) %>% # drop irrelevant information
  group_by(femaleID, date, clan) %>%
  mutate(is.extra = ifelse(sum(is.na(maleID)) != n() & sum(is.na(maleID)) >= 1 & is.na(maleID), TRUE, FALSE)) %>%
  ungroup() %>%
  filter(!is.extra) %>% # remove conceptions with NA male on which date another male is confirmed to be the father
  unique() %>% # cubs sired by the same father are considered to result from one conception event
  group_by(femaleID, date) %>%  # when focal female mated with multiple males on the same date, randomly select one conception - to remove pseudo-replicates of candidate males later
  sample_n(1) %>%
  group_by(femaleID) %>%
  arrange(femaleID, date, clan, maleID) %>%
  mutate(con.order = row_number(), # ordering conception of each female by the date
         conID = paste(femaleID, formatC(con.order, width = 3, flag = "0"), sep = "_")) %>% # give each conception event an ID
  ungroup() %>%
  mutate(male.type = ifelse(fetch_dyad_is.rule.suitable.mate(femaleID, maleID), "suitable", "unsuitable")) %>% # assign type of conception: rule-suitable or unsuitable
  select(conID, everything(), -is.extra, -con.order) -> conceptions_raw

# conceptions_clean
conceptions_raw %>%
  filter(!is.na(maleID) & !is.na(male.type))  -> conceptions_clean   # clean data: remove conceptions in which maleID and type is unknown

# Candidates ------------

conceptions_raw %>%
  select(femaleID, date, clan) %>% # merge conceptions by one female on the same date, because we only need female, clan and date information to extract the candidate males
  filter(date <= find_pop_date.observation.last() - lubridate::years(1)) %>% # remove conceptions happened one year before the last observation date, since the candidate list may be incomplete (e.g  fetch_id_lifestage("E-277", at = "2020-08-23") =>> "disperser"; fetch_id_lifestage("E-277", at = "2020-08-25") =>> "NA")
  distinct() -> conceptions_diffday

candidates <- create_id_mate.candidate(female = conceptions_diffday$femaleID, date = conceptions_diffday$date, clan = conceptions_diffday$clan)

# combining the candidates and conception table
left_join(x = conceptions_raw, y = candidates, by = c("femaleID", "date", "clan")) %>%
  mutate(cand.type = ifelse(fetch_dyad_is.rule.suitable.mate(femaleID = femaleID, maleID = candidateID), "suitable", "unsuitable")) -> candidates_raw

# candidates_clean
candidates_raw %>%
  group_by(conID) %>%
  filter(all(!is.na(cand.type))) %>% # remove conception events when there is at least one candidate type that cannot be determined
  ungroup() -> candidates_clean


# Full data --------
# creating a conception dataset that include characteristics parameters of the
# females, males and candidates
# candidates_full
# add age, rank, tenure, lifestage, etc information to the conception table
thresholdrank <- c(1/3, 2/3)
candidates_raw %>%
  mutate(
    fem.age = fetch_id_age(femaleID, date),
    fem.rank = fetch_id_rank.sex.std(femaleID, date),
    fem.rank.cat = recode_rank.std_rank.cat(ranks = fem.rank, prob.ranks = thresholdrank),
    male.age = fetch_id_age(maleID, date),
    male.tenure = hyenaR::fetch_id_duration.tenure(maleID, date),
    male.rank = fetch_id_rank.sex.std(maleID, date),
    male.rank.cat = recode_rank.std_rank.cat(ranks = male.rank, prob.ranks = thresholdrank),
    male.lifstg = fetch_id_lifestage(maleID, date),
    cand.age = fetch_id_age(candidateID, date),
    cand.tenure =  hyenaR::fetch_id_duration.tenure(candidateID, date),
    cand.rank = fetch_id_rank.sex.std(candidateID, date),
    cand.rank.cat = recode_rank.std_rank.cat(ranks = cand.rank, prob.ranks = thresholdrank),
    cand.lifstg = fetch_id_lifestage(candidateID, date),
  ) -> candidates_full

# change the label of rank
candidates_full$fem.rank.cat <- factor(candidates_full$fem.rank.cat, levels = c("67--100%", "33--67%", "0--33%"), labels = c("Low", "Medium", "High"))
candidates_full$male.rank.cat <- factor(candidates_full$male.rank.cat, levels = c("67--100%", "33--67%", "0--33%"), labels = c("Low", "Medium", "High"))
candidates_full$cand.rank.cat <- factor(candidates_full$cand.rank.cat, levels = c("67--100%", "33--67%", "0--33%"), labels = c("Low", "Medium", "High"))

# conceptions_full
candidates_full %>%
  group_by(conID) %>%
  summarise(n.cand = ifelse(any(is.na(cand.type)), NA_real_, n()), # number of candidates
            n.suitable.cand = ifelse(any(is.na(cand.type)), NA_real_, sum(cand.type == "suitable")), # number of suitable candidates
            n.unsuitable.cand = n.cand - n.suitable.cand) -> sum_candidates_full # number of unsuitable candidates

candidates_full %>%
  select(-starts_with("cand")) %>%
  distinct() %>%
  left_join(x = ., y = sum_candidates_full, by = "conID") -> conceptions_full

# conceptions_full_clean
conceptions_full %>%
  drop_na() -> conceptions_full_clean # keep the conception with complete information of all the columns

# Random dataset ----------
# building a list of random datasets (time consuming)
set.seed(123)
seed_1000 <- sample(1000, 1000) # 1000 permutations
list <- vector(mode = "list", length = length(seed_1000)) # randomly choose a set of 1000 fixed seeds

for (i in 1:length(seed_1000)){
  set.seed(seed_1000[i])
  conceptions_full %>%
    select(-starts_with("male")) -> temp

  candidates_full %>%
    group_by(conID) %>%
    slice_sample(n= 1) %>% # randomly assign a candidate as the focal male
    ungroup() %>%
    mutate(maleID = candidateID,
           male.type = cand.type,
           male.rank = cand.rank,
           male.age = cand.age,
           male.lifstg = cand.lifstg,
           male.tenure = cand.tenure) %>%
    select(-starts_with("cand")) %>%
    left_join(x = ., y = temp, by = c("conID", "femaleID", "date", "clan", "fem.rank", "fem.rank.cat", "fem.age")) -> list[[i]]
}

conceptions_full_random <- list

usethis::use_data(conceptions_full_random, overwrite = TRUE)

# Offspring ---------------
create_offspring_litterID.from.all(females) %>%
  filter(.data$filiation != "social_only" | is.na(filiation)) %>%
  select(parentID, litterID, birthdate) %>%
  unique() %>%
  group_by(parentID) %>%
  mutate(gen.lit.ord = row_number()) %>% # genetic litter order - genetic litter: litters that contains at least one genetic offspring
  ungroup() -> litter_numbers


create_offspring_litterID.from.all(females) %>%
  filter(.data$filiation != "social_only") %>%  # keep only genetic offspring
  left_join(x = ., y = litter_numbers, by = c("parentID", "litterID", "birthdate")) %>%
  rename(motherID = parentID) %>%
  mutate(fatherID = fetch_id_id.father(offspringID),
         con.date = birthdate - 110,
         con.clan = fetch_id_clan.current(motherID, con.date)) %>%
  drop_na(con.date) %>%
  filter(con.date >= find_pop_date.observation.first()) %>% #keep conceptions that happened after the start of study
  select(offspringID, birthdate, litterID, gen.lit.ord, motherID, fatherID, con.date, con.clan) -> offspring

usethis::use_data(offspring, overwrite = TRUE)

# Parity-survival data ------------
conceptions_full %>%
  mutate(fem.is.cen = fetch_id_is.censored(femaleID)) %>%   # remove left censored female (cannot confirm if it is her first litter or not) and right censored female (cannot confirm the death date - thus the end of the study)
  filter(!fem.is.cen) -> conceptions_no_cen

left_join(conceptions_no_cen, offspring, by = c("femaleID" = "motherID", "maleID" = "fatherID", "date" = "con.date", "clan" = "con.clan")) -> conceptions_offspring

conceptions_offspring %>%
  mutate(litter.ord = as.numeric(str_extract(litterID, "\\d+$"))) %>%   # extract litter order
  group_by(femaleID) %>%
  filter(all(litter.ord == gen.lit.ord)) %>%  # remove females whose litter order does not equal to genetic litter (those who had pure social litters)
  ungroup() %>%
  mutate(is.primiparous = litter.ord == 1) %>%
  select(femaleID, date, litter.ord, is.primiparous) %>%
  unique() %>% # remove rows of the same litter
  group_by(femaleID) %>%
  filter(min(litter.ord) == 1) %>% # only keep females whose first litter was recorded
  mutate(date = date + 110,
         date.next.con = lead(date, 1) -1 ) %>%
  ungroup() %>% # prepare the duration when female is primiparous
  filter(is.primiparous) %>%
  select(femaleID, date, date.next.con) %>%
  rename(date1 = date,
         date2 = date.next.con) -> primi_fem

primi_fem %>%
  mutate(last_date = if_else(!is.na(fetch_id_date.death(femaleID)), fetch_id_date.death(femaleID) %m+% months(6), find_pop_date.observation.last())) %>% # the end of the sequence can be female's birth or the last observation date
  rename(ID = femaleID) %>%
  reshape_row_date.seq(ID, from = date1, to = last_date, by = "6 months") %>%
  mutate(dead = !fetch_id_is.alive(ID = ID, at = date)) %>%
  left_join(x = ., y = primi_fem, by = c("ID" = "femaleID")) %>%
  mutate(parity = if_else(date >= date1 & (date <= date2 | is.na(date2)), "primiparous", "pluriparous" )) %>%
  group_by(ID) %>%
  mutate(start = row_number(), # adding tags for the start and stop time
         stop = row_number() + 1) -> parity_survival

# save data into RDS file for faster access
save(conceptions_raw,conceptions_clean,candidates_raw,candidates_clean, candidates_full,conceptions_full,conceptions_full_clean,file = "~/Desktop/MEME/Work/Projects/Berlin Hyena/R/Zimai_ms_raw.rds")
load("~/Desktop/MEME/Work/Projects/Berlin Hyena/R/Zimai_ms_raw.rdata")


## Analysis --------------------
## Data presentation ----

## female age, male tenure and suitability
## plot the relationship between female age and male tenure
left_join(conceptions_clean, conceptions_full) %>%
  drop_na(fem.age, male.tenure) %>%
  ggplot(aes(x = male.tenure, y = fem.age, color = male.type, alpha = 0.8)) +
  geom_point() +
  scale_color_manual(values=c("suitable" = "#FCD5AB","unsuitable" ="#D4E5F1")) +
  # add reference line when female age = male tenure
  geom_abline(slope = 1, intercept = 0)+
  theme_bw()

## distribution of female age and rank in the conception events with different
## type of focal males
left_join(conceptions_clean, conceptions_full) %>%
  select(male.type, fem.rank, fem.age) -> fem_age_rank

# basic stats
fem_age_rank %>%
  group_by(male.type) %>%
  summarise(mean_age = mean(fem.age),
            sd_age = sd(fem.age),
            mean_rank = mean(fem.rank, na.rm = TRUE),
            sd_rank = sd(fem.rank, na.rm = TRUE))

# plot: female age in different type of mating
ggviolin(fem_age_rank, x = "male.type", y = "fem.age",
         fill = "male.type", color = "dimgray") +
  geom_boxplot(fill = "white", color = "dimgray", width = 0.1, outlier.shape = NA) +
  labs(x = "Female mate choice", y = "Female age") +
  guides(fill = FALSE, color = FALSE, alpha = FALSE) +
  #stat_compare_means(label.x.npc = 0.8) +
  stat_n_text(y.pos = -1.7)

# plot: female rank in different type of mating
fem_age_rank %>%
  filter(!is.na(fem.rank)) %>%
  ggviolin(x = "male.type", y = "fem.rank",
           fill = "male.type", color = "dimgray") +
  geom_boxplot(fill = "white", color = "dimgray", width = 0.1, outlier.shape = NA) +
  labs(x = "Female mate choice", y = "Female rank") +
  guides(fill = FALSE, color = FALSE, alpha = FALSE) +
  stat_n_text(y.pos = -1.7)

# stat: comparisons of female age in different type of mating
wilcox.test(formula = fem.age ~ male.type, data = fem_age_rank)
wilcox.test(formula = fem.rank ~ male.type, data = fem_age_rank)


## Part 1: mate availability ----
## Variation of mate availability in females from different categories
## In this analysis, non-parametric tests were conducted to compare the number
## and proportion of suitable males in conceptions that were carried out by
## females from different age and social rank groups
# data preparation: divide females into different age groups and calculate the
# proportion of suitable candidates
conceptions_full %>%
  drop_na(n.cand) %>%
  mutate(
    fem.age.cat = factor(ifelse(fem.age <= 5, "Young (<= 5 years old)", "Others (> 5 years old)"), levels = c("Young (<= 5 years old)", "Others (> 5 years old)")),
    prop.suitable.cand = n.suitable.cand / n.cand) %>%
  select(femaleID, date, fem.age, fem.age.cat, n.cand, n.suitable.cand, n.unsuitable.cand, prop.suitable.cand) %>%
  distinct() -> availability_fem_age # keep conception by the same female on the same date unique

# data summary
availability_fem_age %>%
  summarise(n = n(),
            mean.n = mean(n.cand),
            sd.n = sd(n.cand),
            min.n = min(n.cand),
            max.n = max(n.cand),
            mean.n.suitable = mean(n.suitable.cand),
            sd.n.suitable = sd(n.suitable.cand),
            mean.prop.suitable = mean(prop.suitable.cand),
            sd.prop.suitable = sd(prop.suitable.cand),
            n.all.suitable = sum(n.unsuitable.cand == 0),
            n.all.unsuitable = sum(n.suitable.cand == 0),
            min.n.suitable = min(n.suitable.cand),
            max.n.suitable = max(n.suitable.cand),
            min.prop.suitable = min(prop.suitable.cand),
            max.prop.suitable = max(prop.suitable.cand)) %>%
  pivot_longer(everything(), names_to = "stats")

# number of suitable candidates between different female age group
availability_fem_age %>%
  group_by(fem.age.cat) %>%
  summarise(mean.n.suitable = mean(n.suitable.cand),
            sd.n.suitable = sd(n.suitable.cand),
            min.n.suitable = min(n.suitable.cand),
            max.n.suitable = max(n.suitable.cand),
            n = n()) %>%
  ungroup()

# proportion of cases when all the candidate males are suitable
availability_fem_age %>%
  summarise(n.prop.0 = sum(prop.suitable.cand == 0),
            n.prop.1 = sum(prop.suitable.cand == 1),
            prop.prop.1 = sum(prop.suitable.cand == 1)/n())

# plot:number of suitable candidates changes with female age (continuous)
ggplot(availability_fem_age, aes(x = fem.age, y = n.suitable.cand)) +
  geom_jitter(size = 0.5, color = "darkgray") +
  geom_smooth(method = "gam", color = "#386cb0") +
  labs(x = "Female age", y = "Number of suitable candidate males") +
  guides(fill = FALSE, color = FALSE, alpha = FALSE)

# plot: comparison of number of suitable candidates between different age groups
ggviolin(availability_fem_age, x = "fem.age.cat", y = "n.suitable.cand",
         fill = "fem.age.cat", color = "dimgray") +
  geom_boxplot(fill = "white", color = "dimgray", width = 0.1, outlier.shape = NA) +
  xlab("Female age") +
  ylab("Number of suitable candidate mates") +
  scale_y_continuous(expand = expansion(add = c(3, 3))) +
  guides(fill = FALSE, color = FALSE, alpha = FALSE) +
  stat_n_text(y.pos = -4.5)

# plot: distribution of proportion of suitable candidates
ggplot(availability_fem_age, aes(x = prop.suitable.cand)) +
  geom_histogram(bins = 20) +
  geom_vline(aes(xintercept = mean(prop.suitable.cand)), color = "red") +
  xlab("Proportion of suitable candidate mates") +
  ylab("Number of conceptions")

# stat: comparison of number of suitable candidates between different age groups
wilcox.test(formula = n.suitable.cand ~ fem.age.cat, data = availability_fem_age)

# data preparation: female age at first conception between different social
# rank group
# select age at conception of first litter
conceptions_full %>%
  # remove left censored female (you do not know if it is her first litter or not)
  mutate(fem.is.left.cen = fetch_id_is.censored.left(femaleID)) %>%
  filter(!fem.is.left.cen) -> conceptions_no_left_cen

# date of first conception is the same as date of conception of first litter
conceptions_no_left_cen %>%
  group_by(femaleID) %>%
  filter(date == min(date)) %>%
  slice(1) %>%
  ungroup() %>%
  drop_na(fem.rank.cat, n.suitable.cand) -> fem_rank_first_conception

fem_rank_first_conception %>%
  select(femaleID, fem.age, fem.rank.cat) -> fem_rank_first_conception_short

# data preparation: compare the age of 2nd litter of high-ranking females and
# the age of 1st litter of low-ranking females add offspring and litter
# information in conception table
left_join(conceptions_no_left_cen, offspring, by = c("femaleID" = "motherID", "maleID" = "fatherID", "date" = "con.date", "clan" = "con.clan")) -> conceptions_offspring

# second litter of high rank female whose first litter survived til the conception of second litter
conceptions_offspring %>%
  # remove female whose rank changed during litter 1 - litter 2
  # select the first and second litter
  # mutate(litter = as.numeric(str_extract(litterID, "\\d+$"))) %>%
  filter(gen.lit.ord %in% c(1,2)) %>%
  # select female that does not change her rank
  group_by(femaleID) %>%
  mutate(is.rank.change = length(unique(fem.rank.cat)) != 1) %>%
  ungroup() %>%
  filter(!is.rank.change) %>%
  # select high rank females
  filter(fem.rank.cat == "High") %>%
  # add date of conception of second litter to the rows of first litter
  select(femaleID, date, clan) %>%
  unique() %>%
  group_by(femaleID) %>%
  mutate(date.sec.con = lead(date, 1)) %>%
  ungroup() %>%
  inner_join(x = conceptions_offspring, y = ., by = c("femaleID", "date", "clan")) %>%
  # select first litter
  filter(gen.lit.ord == 1) %>%
  ## OLD WAY filter(str_detect(litterID, "_001")) %>%
  select(conID, femaleID, maleID, date, date.sec.con, offspringID, litterID) %>%
  # if the offspring survived til second litter
  mutate(is.off.surv = fetch_id_is.alive(offspringID, date.sec.con)) %>%
  group_by(femaleID) %>%
  # select female that at least one offspring survived til second litter
  filter(any(is.off.surv)) %>%
  ungroup() %>%
  # select those females
  select(femaleID) %>%
  unique() %>%
  inner_join(x = conceptions_offspring, y = .) %>%
  # select their second litter
  filter(gen.lit.ord == 2) %>%
  select(femaleID, fem.age, fem.rank.cat) %>%
  unique() %>%
  mutate(fem.rank.cat = "High (second litter)") -> high_rank_fem_sec

# first litter of low and medium rank females
conceptions_no_left_cen %>%
  group_by(femaleID) %>%
  filter(date == min(date)) %>%
  slice(1) %>%
  ungroup() %>%
  filter(fem.rank.cat %in% c("Low", "Medium")) %>%
  select(femaleID, fem.age, fem.rank.cat) %>%
  mutate(fem.rank.cat = paste(fem.rank.cat, "(first litter)")) -> low_med_rank_fem_fir

bind_rows(low_med_rank_fem_fir, high_rank_fem_sec) -> fem_rank_first_sec_conception

fem_rank_first_sec_conception$fem.rank.cat <- factor(fem_rank_first_sec_conception$fem.rank.cat, levels = c("Low (first litter)", "Medium (first litter)", "High (second litter)"))

# female rank and number of suitable candidates
fem_rank_first_conception %>%
  drop_na(n.suitable.cand) %>%
  select(fem.rank.cat, n.suitable.cand) -> fem_rank_suit_cand

# data summary
# general summary of the date of first conception
fem_rank_first_conception_short %>%
  summarise(mean = mean(fem.age),
            sd = sd(fem.age),
            min = min(fem.age),
            max = max(fem.age),
            n = n())
# summary of the date of first conception between different female social ranks
fem_rank_first_conception_short %>%
  group_by(fem.rank.cat) %>%
  summarise(mean = mean(fem.age),
            sd = sd(fem.age),
            min = min(fem.age),
            max = max(fem.age),
            n = n())

# summary of the date of first conceptions for low and medium rankiing females
# and date of second conceptions for high ranking males
fem_rank_first_sec_conception %>%
  group_by(fem.rank.cat) %>%
  summarise(mean = mean(fem.age),
            sd = sd(fem.age),
            min = min(fem.age),
            max = max(fem.age),
            n = n())

# summary of female rank and number of suitable candidates
fem_rank_suit_cand %>%
  group_by(fem.rank.cat) %>%
  summarise(mean = mean(n.suitable.cand),
            sd = sd(n.suitable.cand),
            min = min(n.suitable.cand),
            max = max(n.suitable.cand),
            n = n())

# stat and plot
# original kruskal test: female age at first conception between different
# rank categories
kruskal.test(formula = fem.age ~ fem.rank.cat, data = fem_rank_first_conception_short)
dunn.test(fem_rank_first_conception_short$fem.age, fem_rank_first_conception_short$fem.rank.cat, method = "bonferroni", altp = TRUE, table = TRUE)
# original kruskal test: number of suitable candidate for females with different
# rank cat
kruskal.test(formula = n.suitable.cand ~ fem.rank.cat, data = fem_rank_suit_cand)
dunn.test(fem_rank_suit_cand$n.suitable.cand, fem_rank_suit_cand$fem.rank.cat, method = "bonferroni", altp = TRUE)

# test for the plotting: kruskal test and post-hoc dunn test
# comparison of age at first conception between female of different ranks
kruskal_age_first_con <- fem_rank_first_conception_short %>% kruskal_test(fem.age ~ fem.rank.cat)
dunn_age_first_con <- fem_rank_first_conception_short %>%
  dunn_test(fem.age ~ fem.rank.cat, p.adjust.method = "bonferroni")
# comparison of age at first conception for low and medium rank female and age
# at second conception for high rank females
kruskal_age_first_sec_con <- fem_rank_first_sec_conception %>% kruskal_test(fem.age ~ fem.rank.cat)
dunn_age_first_sec_con <- fem_rank_first_sec_conception %>%
  dunn_test(fem.age ~ fem.rank.cat, p.adjust.method = "bonferroni")
# comparison of number of suitable candidate for females with different rank cat
kruskal_suitcand_fem_rank <- fem_rank_suit_cand %>%
  kruskal_test(n.suitable.cand ~ fem.rank.cat)
dunn_suitcand_fem_rank <- fem_rank_suit_cand %>%
  dunn_test(n.suitable.cand ~ fem.rank.cat, p.adjust.method = "bonferroni")

# add positions
dunn_age_first_con %>%
  arrange(desc(group2), desc(group1)) %>%
  add_xy_position(x = "fem.rank.cat", y.trans = function(x){x + 1}, step.increase	= 0.3) -> dunn_age_first_con_pos
dunn_age_first_con_pos$p.adj <- ifelse(dunn_age_first_con_pos$p.adj < 0.0001, "p < 0.0001", paste0("p = ", signif(dunn_age_first_con_pos$p.adj, 2)))

dunn_age_first_sec_con %>%
  arrange(desc(group2), desc(group1)) %>%
  add_xy_position(x = "fem.rank.cat", y.trans = function(x){x + 1}, step.increase	= 0.5) -> dunn_age_first_sec_con_pos
dunn_age_first_sec_con_pos$p.adj <- ifelse(dunn_age_first_sec_con_pos$p.adj < 0.0001, "p < 0.0001", paste0("p = ", signif(dunn_age_first_sec_con_pos$p.adj, 2)))

dunn_suitcand_fem_rank %>%
  #group_by(conception_time) %>%
  arrange(desc(group2), desc(group1), .by_group = TRUE) %>%
  #ungroup() %>%
  add_xy_position(x = "female_rank_cat", fun = "max",
                  #y.trans = function(x){x+1},
                  step.increase	= 0.3) -> dunn_suitcand_fem_rank_pos
dunn_suitcand_fem_rank_pos$p.adj <- ifelse(dunn_suitcand_fem_rank_pos$p.adj < 0.0001, "p < 0.0001", paste0("p = ", signif(dunn_suitcand_fem_rank_pos$p.adj, 2)))

# showing dunn test results
dunn_age_first_con_pos %>%
  mutate("z-score" = signif(statistic, 3)) %>%
  select(group1, group2, n1, n2, `z-score`, p.adj) %>%
  knitr::kable()

dunn_age_first_sec_con_pos %>%
  mutate("z-score" = signif(statistic, 3)) %>%
  select(group1, group2, n1, n2, `z-score`, p.adj) %>%
  knitr::kable()

dunn_suitcand_fem_rank_pos %>%
  mutate("z-score" = signif(statistic, 3)) %>%
  select(group1, group2, n1, n2, `z-score`, p.adj) %>%
  knitr::kable()

# plot: comparison of age at first conception between female of different ranks
ggviolin(fem_rank_first_conception_short, x = "fem.rank.cat", y = "fem.age",
         fill = "fem.rank.cat", color = "dimgray") +
  geom_boxplot(fill = "white", color = "dimgray", width = 0.1, outlier.shape = NA) +
  xlab("Female social rank") +
  ylab("Female age at first conception") +
  stat_pvalue_manual(dunn_age_first_con_pos, label = "p.adj") +
  scale_y_continuous(expand = expansion(add = c(1, 1))) +
  stat_n_text(y.pos = 0.5) +
  rremove("legend")

# plot: comparison of age at first conception for low and medium rank female and
# age at second conception for high rank females
ggviolin(fem_rank_first_sec_conception, x = "fem.rank.cat", y = "fem.age",
         fill = "fem.rank.cat", color = "dimgray") +
  geom_boxplot(fill = "white", color = "dimgray", width = 0.1, outlier.shape = NA) +
  xlab("Female social rank") +
  ylab("Female age") +
  stat_pvalue_manual(dunn2_2_pos, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(add = c(1, 1))) +
  stat_n_text(y.pos = 0.5)

# plot: comparison of number of suitable candidate for females with different rank cat
ggviolin(fem_rank_suit_cand, x = "fem.rank.cat", y = "n.suitable.cand",
         fill = "fem.rank.cat", color = "dimgray",
         ggtheme = theme_bw()) +
  geom_boxplot(fill = "white", color = "dimgray", width = 0.1, outlier.shape = NA) +
  xlab("Female social rank") +
  ylab("Number of suitableable candidate mates at first conception") +
  stat_pvalue_manual(dunn_suitcand_fem_rank_pos, label = "p.adj", hide.ns = TRUE) +
  scale_y_continuous(expand = expansion(add = c(2, 4))) +
  stat_n_text(y.pos = -5)

## Male category and correspondent suitability
## In this section, we looked at how does males from different life-history
## (tenure, dispersal status) and social rank varies in their suitability as
## a candidate in a conception event

# data preparation
left_join(candidates_clean, candidates_full) %>%
  group_by(femaleID, date) %>%
  select(conID, femaleID, date) %>%
  slice(1) %>% # avoid pseudoreplicates, unique the data from the same conception event
  ungroup() %>%
  left_join(x = ., y = candidates_full) %>%
  group_by(conID) %>%
  filter(length(unique(cand.type)) == 2) %>% # make sure each conception we have here includes both suitable and unsuitable males
  ungroup() -> candidate_suitability

candidate_suitability %>%
  group_by(conID, cand.type) %>%
  summarise(mean.tenure.con = mean(cand.tenure)) %>%
  filter(all(!is.na(mean.tenure.con))) %>%
  ungroup() %>%
  arrange(cand.type, conID) -> candidate_suitability_tenure

candidate_suitability %>%
  group_by(conID, cand.type) %>%
  summarise(mean.rank.con = mean(cand.rank)) %>%
  ungroup() %>%
  arrange(cand.type, conID) -> candidate_suitability_rank

left_join(candidates_clean, candidates_full) %>%
  group_by(femaleID, date) %>%
  select(conID, femaleID, date) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(x = ., y = candidates_full) %>%
  # remove secondary and multi-clan dispersers
  filter(cand.lifstg != "selector_2" & cand.lifstg !=  "selector_3") %>%
  # categorising dispersal status
  mutate(dispersal.status = case_when(
    cand.lifstg %in% c("philopatric", "subadult", "natal") ~ "philopatric",
    cand.lifstg %in% c("disperser", "immigrant") ~ "immigrant",
    TRUE ~ cand.lifstg
  )) %>%
  select(conID, femaleID, date, clan, candidateID, cand.type, cand.lifstg, dispersal.status) %>%
  group_by(dispersal.status, cand.type) %>%
  count() %>%
  pivot_wider(names_from  = cand.type,
              values_from = n) %>%
  ungroup() %>%
  mutate(prob.unsuitable = unsuitable/ (suitable + unsuitable)) -> candidate_suitability_dispersal

# data summary
candidate_suitability_tenure %>%
  group_by(cand.type) %>%
  summarise(mean.tenure = mean(mean.tenure.con),
            sd.tenure = sd(mean.tenure.con),
            max.tenure = max(mean.tenure.con),
            min.tenure = min(mean.tenure.con),
            n.tenure = n())

candidate_suitability_rank %>%
  group_by(cand.type) %>%
  summarise(mean = mean(mean.rank.con),
            sd = sd(mean.rank.con),
            max = max(mean.rank.con),
            min = min(mean.rank.con),
            n = n())

# plot: pairwise comparison of tenure and rank between suitable and unsuitable candidates
ggplot(candidate_suitability_tenure, aes(x = cand.type, y = mean.tenure.con, fill = cand.type)) +
  geom_violin(color = "dimgray") +
  geom_boxplot(color = "dimgray", fill = "white", width = 0.1) +
  geom_point(color = "dimgray", size = 0.001) +
  geom_line(aes(group = conID), color = "gray", size = 0.03) +
  scale_x_discrete(labels = c("suitable", "unsuitable")) +
  xlab("suitableability of candidate mates") +
  ylab("Mean tenure") +
  guides(fill = "none")

ggplot(candidate_suitability_rank, aes(x = cand.type, y = mean.rank.con, fill = cand.type)) +
  geom_violin(color = "dimgray") +
  geom_boxplot(color = "dimgray", fill = "white", width = 0.1) +
  geom_point(color = "dimgray", size = 0.001) +
  geom_line(aes(group = conID), color = "gray", size = 0.03) +
  scale_x_discrete(labels = c("suitable", "unsuitable")) +
  xlab("suitableability of candidate mates") +
  ylab("Mean social rank") +
  guides(fill = "none")

candidate_suitability_dispersal %>%
  mutate(n = suitable + unsuitable,
         odds.unsuitable = unsuitable / suitable,
         odds.unsuitable.foreign = odds.unsuitable[dispersal.status == "immigrant"],
         odds.ratio = odds.unsuitable/odds.unsuitable.foreign) %>%
  ggplot() +
  geom_col(aes(x = dispersal.status, y = prob.unsuitable, fill = dispersal.status)) +
  #geom_text(aes(label = "Pearson's Chi-squared test\nX-squared = 658.81, df = 1, p-value < 2.2e-16"), x = 1.5, y = 0.6) +
  geom_text(aes(label = paste0("n = ", n, ""), x = dispersal.status, y = 0), vjust = -1) +
  geom_text(aes(label = paste0("OR = +", signif(odds.ratio, 3), " x"), x = dispersal.status, y = prob.unsuitable), vjust = -1) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  labs(x = "Dispersal status of candidate males", y = "Probability of being unsuitable") +
  guides(fill = "none")

# stat: pairwise comparison of tenure and rank between suitable and unsuitable candidates
wilcox.test(formula = mean.tenure.con ~ cand.type, data = candidate_suitability_tenure, paired = TRUE, alternative = "less")
wilcox.test(formula = mean.rank.con ~ cand.type, data = candidate_suitability_rank, paired = TRUE, alternative = "less")

# stat: chisquare test between the probability of being unsuitable for males with
# different dispersal status
candidate_suitability_dispersal %>%
  select(-4) %>%
  column_to_rownames("dispersal.status") %>%
  as.matrix(.) -> candidate_suitability_dispersal_matrix

chisq.test(x = candidate_suitability_dispersal_matrix)

## Part 2: Mate preference and mate choice ----
# data preparation
offspring %>%
  mutate(is.genotyped = fetch_id_is.sampled.dna(offspringID)) %>%
  group_by(litterID) %>%
  filter(all(is.genotyped)) %>% # remove litters that has cubs that were not genotyped
  summarise(n.paternity = n_distinct(fatherID)) %>%
  ungroup() %>%
  left_join(x = offspring, y = .) -> offspring_litter

# data: add information about conceptions
left_join(conceptions_clean, conceptions_full) %>%
  drop_na(n.cand) %>% # remove conceptions when number of candidates are unknown
  left_join(x = ., y = offspring_litter, by = c("femaleID" = "motherID", "maleID" = "fatherID", "date" = "con.date", "clan" = "con.clan")) %>% # add information of litter
  select(-offspringID) %>%
  unique() %>%
  mutate(litter.ord = gen.lit.ord,
         fem.age.cat = ifelse(fem.age <= 5, "Young", "Old"),
         is.primiparous = litter.ord == 1,
         choice = ifelse(male.type == "suitable",1,0),
         availability = n.suitable.cand / n.cand) %>%
  # remove conceptions on which all the candidates are (un)suitable
  filter(availability != 0 & availability != 1) -> conceptions_mate_choice

# data: calculate further conception information for analysis
conceptions_mate_choice %>%
  drop_na(fem.rank) %>%
  mutate(fem.rank.cat = ifelse(fem.rank.cat == "High", "High", "Low"),
         fem.rank.age = paste0(fem.rank.cat, "_", fem.age.cat),
         n.suitable.cand.cat = factor(cut(n.suitable.cand, breaks = c(0, quantile(n.suitable.cand, 1/3), quantile(n.suitable.cand, 2/3), max(n.suitable.cand))), labels = c("Small", "Medium", "Large")),
         is.primiparous = ifelse(is.primiparous, 1, 0)) %>%
  select(choice, femaleID, availability, n.suitable.cand.cat, is.primiparous, fem.rank.age)  %>%
  filter(availability != 1 & availability != 0) -> conceptions_mate_choice_mdl

conceptions_mate_choice_mdl$fem.rank.age <- factor(conceptions_mate_choice_mdl$fem.rank.age, levels = c("Low_Young", "Low_Old", "High_Young", "High_Old"))

# stat: glmmer model
fit_mate_choice <- glmer(choice ~ availability * n.suitable.cand.cat + is.primiparous + fem.rank.age + (1|femaleID), family = binomial(link = "logit"), data = conceptions_mate_choice_mdl, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# model comparison: whether include logit will make the model fit worse
fit_mate_choice_logit <- glmer(choice ~ logit(availability) * n.suitable.cand.cat + is.primiparous + fem.rank.age + (1|femaleID), family = binomial(link = "logit"), data = conceptions_mate_choice_mdl, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
AIC(fit_mate_choice, fit_mate_choice_logit)

# testing the assumptions
# resid_mate_choice <- simulateResiduals(fit_mate_choice_logit)
# plot(resid_mate_choice)
# testDispersion(resid_mate_choice)
# testTemporalAutocorrelation(resid_mate_choice, time = order(fitted(fit_mate_choice_logit)))
# # testing whether it is better than null model
# Anova(fit_mate_choice_logit)

# summary of models
summary(fit_mate_choice_logit)
cov2cor(vcov(fit_mate_choice_logit))
tab_model(fit_mate_choice_logit)

# presenting the estimates
# confidence interval takes too long to run, save output
est_ci_mate_choice_logit <- confint(fit_mate_choice_logit, parm = "beta_")
est_tabl_mate_choice_logit <- exp(cbind(est = fixef(fit_mate_choice_logit), est_ci_mate_choice_logit))
format(est_tabl_mate_choice_logit, scientific = FALSE)

# post-hoc test
post_hoc_mate_choice_logit <- glht(fit_mate_choice_logit, mcp(fem.rank.age = "Tukey"))
par(mar = c(4, 20, 2, 2))
plot(post_hoc_mate_choice_logit)

# null model
offspring_litter %>%
  select(motherID, con.date, con.clan, gen.lit.ord, n.paternity) %>%
  unique() -> litter_assignment

# create empty dataset for null model
conceptions_mate_choice_null <- vector(mode = "list", length = length(1000))
fit_mate_choice_logit_null_spaMM <- vector(mode = "list", length = length(1000))
pre_null <- vector(mode = "list", length = length(1000))
pre_null_res <- vector(mode = "list", length = length(1000))

for (i in 1:1000) {
  conceptions_mate_choice %>%
    select(conID, gen.lit.ord, n.paternity) %>%
    left_join(x = ., y = conceptions_full_random[[i]]) %>%
    drop_na(fem.rank) %>%
    mutate(litter = gen.lit.ord,
           fem.age.cat = ifelse(fem.age <= 5, "Young", "Old"),
           is.primiparous = litter == 1,
           choice = ifelse(male.type == "suitable",1,0),
           availability = (n.suitable.cand / n.cand),
           fem.rank.cat = ifelse(fem.rank.cat == "High", "High", "Low"),
           fem.rank.age = paste0(fem.rank.cat, "_", fem.age.cat),
           n.suitable.cand.cat = factor(cut(n.suitable.cand, breaks = c(0, quantile(n.suitable.cand, 1/3), quantile(n.suitable.cand, 2/3), max(n.suitable.cand))), labels = c("Small", "Medium", "Large")),
           is.primiparous = ifelse(is.primiparous, 1, 0)) %>%
    select(choice, femaleID, availability, n.suitable.cand.cat, is.primiparous, fem.rank.age) %>%
    filter(availability != 1 & availability != 0) -> conceptions_mate_choice_null[[i]]

  conceptions_mate_choice_null[[i]]$fem.rank.age <- factor(conceptions_mate_choice_null[[i]]$fem.rank.age, levels = c("Low_Young", "Low_Old", "High_Young", "High_Old"))

  fit_mate_choice_logit_null_spaMM[[i]] <- spaMM::fitme(choice ~ logit(availability) * n.suitable.cand.cat + is.primiparous + fem.rank.age + (1|femaleID), family = binomial(link = "logit"), data = conceptions_mate_choice_null[[i]], method = "PQL/L")
  pre_null[[i]] <- expand.grid(availability = 0.64, n.suitable.cand.cat =  levels(conceptions_mate_choice_null[[i]]$n.suitable.cand.cat), is.primiparous = c(0,1), fem.rank.age = levels(conceptions_mate_choice_null[[i]]$fem.rank.age), femaleID = "NEW")
  pre_null_res[[i]] <- spaMM::predict.HLfit(fit_mate_choice_logit_null_spaMM[[i]], newdata = pre_null[[i]], re.form = NA, binding = "pred")

}

# bind up the model predictions based on 1000 artificial dataset
pre_null_res_bind <- pre_null_res[[1]]
for (i in 2:1000) {
  pre_null_res_bind <- bind_rows(pre_null_res_bind, pre_null_res[[i]])
}

# the data can be saved for faster access
#saveRDS(pre_null_res_bind, file = "~/Insync/zimai.li@evobio.eu/Google Drive/MEME/Work/Projects/Berlin Hyena/R/hyenaMC/data/pre_null_res_bind.RDS")
# read prediction of null model
#pre_null_res_bind <- readRDS("~/Insync/zimai.li@evobio.eu/Google Drive/MEME/Work/Projects/Berlin Hyena/R/hyenaMC/data/pre_null_res_bind.RDS")

# Comparing the predictions of probability of mating with an unsuitable male
# between read data and artificial data (null model)
# make predictions
# prediction for read dataset
# influence of parity, fix other predictors
pre_real_parity <- as.tibble(ggpredict(fit_mate_choice_logit, terms = c("availability [0.64]","is.primiparous"), condition = c(fem.rank.age = "Low_Old", n.suitable.cand.cat = "Medium"), type = "fe")) %>%
  mutate(parity = ifelse(group == 1, "primiparous", "pluriparous"),
         model = "real")
# influence of rank and age, fix other predictors
pre_real_rank_age <- as.tibble(ggpredict(fit_mate_choice_logit, terms = c("availability [0.64]", "fem.rank.age", "is.primiparous [0]"), condition = c(n.suitable.cand.cat = "Medium"), type = "fe")) %>%
  mutate(model = "real")
# influence of number of suitable candidates, fix other predictors
pre_real_n_suit <- as.tibble(ggpredict(fit_mate_choice_logit, terms = c("availability [0.64]", "n.suitable.cand.cat", "is.primiparous [0]"),condition = c(fem.rank.age = "Low_Old"), type = "fe")) %>%
  mutate(model = "real")

# prediction for artificial dataset (null model)
# influence of parity, fix other predictors
pre_null_res_bind %>%
  filter(fem.rank.age == "Low_Old", n.suitable.cand.cat == "Medium") %>%
  group_by(is.primiparous) %>%
  summarise(x = 0.64,
            predicted = mean(pred),
            std.error = sd(pred),
            conf.low =  predicted - 1.96*std.error,
            conf.high = predicted + 1.96*std.error,
            group = factor(is.primiparous),
            parity = ifelse(group == 1, "primiparous", "pluriparous"),
            model = "null"
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-is.primiparous) -> pre_null_parity
# influence of rank and age, fix other predictors
pre_null_res_bind %>%
  filter(is.primiparous == 0, n.suitable.cand.cat == "Medium") %>%
  group_by(fem.rank.age) %>%
  summarise(x = 0.64,
            predicted = mean(pred),
            std.error = sd(pred),
            conf.low =  predicted - 1.96*std.error,
            conf.high = predicted + 1.96*std.error,
            group = fem.rank.age,
            model = "null"
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-fem.rank.age) -> pre_null_rank_age
# influence of number of suitable candidates, fix other predictors
pre_null_res_bind %>%
  filter(is.primiparous == 0, fem.rank.age == "Low_Old") %>%
  group_by(n.suitable.cand.cat) %>%
  summarise(x = 0.64,
            predicted = mean(pred),
            std.error = sd(pred),
            conf.low =  predicted - 1.96*std.error,
            conf.high = predicted + 1.96*std.error,
            group = n.suitable.cand.cat,
            model = "null"
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-n.suitable.cand.cat) -> pre_null_n_suit

# bind predictions of real data and random data together
pre_parity <- bind_rows(pre_real_parity, pre_null_parity)
pre_parity$parity <- factor(pre_parity$parity, levels = c("primiparous", "pluriparous"))
pre_parity$model <- factor(pre_parity$model, levels = c("real", "null"))

pre_rank_age <- bind_rows(pre_real_rank_age, pre_null_rank_age)
pre_rank_age$rank.age <- factor(pre_rank_age$group, labels = c("Low Medium and Young", "Low Medium and Old",  "High and Young",  "High and Old" ), levels = c("Low_Young",  "Low_Old", "High_Young", "High_Old"))
pre_rank_age$model <- factor(pre_rank_age$model, levels = c("real", "null"))

pre_n_suit <- as.tibble(bind_rows(pre_real_n_suit, pre_null_n_suit))
pre_n_suit$group <- factor(pre_n_suit$group, labels = c("Small (<= 7)", "Medium (> 7 & <= 11)", "Large (> 11)"), levels = c("Small", "Medium", "Large"))
pre_n_suit$model <- factor(pre_n_suit$model, levels = c("real", "null"))

# plot: comparing model predictions
ggplot(pre_parity, aes(x = parity, y = predicted, colour = parity)) +
  geom_point(aes(shape = model), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,linetype = model), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parity", y = "Probability of abiding the rule") +
  guides(shape = "none",
         colour = "none",
         linetype = "none")

ggplot(pre_rank_age, aes(x = rank.age, y = predicted, colour = rank.age)) +
  geom_point(aes(shape = model), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,linetype = model), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Parity", y = "Probability of abiding the rule") +
  labs(x = "Female rank and age", y = "Probability of abiding the rule") +
  guides(shape = "none",
         colour = "none",
         linetype = "none")

ggplot(pre_n_suit, aes(x = group, y = predicted, colour = group)) +
  geom_point(aes(shape = model), position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,linetype = model), width=0.2, position = position_dodge(width = 0.9)) +
  labs(x = "Number of suitable candidate mates", y = "Probability of abiding the rule") +
  guides(shape = "none",
         colour = "none",
         linetype = "none")

ggpredict(fit_mate_choice, terms = c("availability [all]", "n.suitable.cand.cat")) %>% plot()


# Survival rate analysis on female with different parity status
# tidy up for longevity dataset
parity_survival %>%
  select(ID, start, stop, parity, dead) -> longevity

# survival analysis and retrieve the statistics
res.cox <- coxph(Surv(start, stop, dead) ~ longevity$parity, data = longevity)
summary(res.cox)
Anova(res.cox,type="II",test="LR")

# plotting survival curves
# for all hyenas
ggsurvplot(survfit(res.cox, data = longevity), palette = c("#E7B800", "#2E9FDF"))
# differentiate primiparous and pluriparous hyenas
longevity$start.year <- longevity$start/2
longevity$stop.year <- longevity$stop/2
fit <- survfit(Surv(start.year, stop.year, dead) ~ longevity$parity, data = longevity)
summary(fit)
ggsurvplot(fit, conf.int = TRUE,
           xlab = "Year",
           legend.labs = c("Pluriparous", "Primiparous"),
           break.time.by = 2,
           palette = c("#E7B800", "#2E9FDF"))

