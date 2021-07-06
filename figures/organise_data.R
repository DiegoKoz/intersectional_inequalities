library(tidyverse)
library(janitor)
library(readxl)
library(fs)
library(glue)

update_files <- function(from = 'results/', to = 'figures/data/' ){
  
  needed_files <- path_file(dir_ls(to))
  all_results <- dir_ls(from)
  needed_results <- path_filter(all_results, regexp = paste0(needed_files, collapse = '|'))
  fs::file_copy(path = needed_results, new_path = to,overwrite = T)
}

update_files()
# update_files(from = 'results/', to='app/data/')

retrieve_table <- function(result,set){
  tables %>% 
    filter(name==result, type==set) %>% 
    unnest(cols = c(table)) %>% 
    select(-c(paths, name, type))
  
}

# organise_table <- function(df, set,wtd=FALSE){
organise_table <- function(df, set){
  
  if (set=='socsci') {
    tw <- top_words_socsci
  }
  if (set=='health') {
    tw <- top_words_health
  }
  
  df %>% pivot_longer(!topic,names_to = c("group", "gender"),
                      names_pattern = "(.*)_(.*)",
                      values_to = 'prob') %>% 
    mutate(gender = case_when(gender == 'M' ~ 'Men',
                              gender == 'F' ~ 'Women'),
           group = str_to_title(group),
           group = case_when(group=='Hispanic' ~'Latinx',
                             TRUE ~group),
           topic = factor(topic)) %>% 
    left_join(tw, by='topic') %>% 
    left_join(retrieve_tp(set), by='topic') %>%
    # left_join(retrieve_tp(set,wtd), by='topic') %>% 
    rename(topic_proportion = proportion)
}


# Social Sciences data ----------------------------------------------------

top_words_socsci <- read_csv('results/top_words_300.csv') %>% 
  mutate(topic = factor(topic))

labels <-  top_words_socsci %>% 
  mutate(labels = paste(topic, top_words, sep = ', ')) %>% 
  select(labels, topic) %>% deframe()

# Health data -------------------------------------------------------------

top_words_health <- read_csv('results/top_words_health_200.csv') %>% 
  mutate(topic = factor(topic))

labels_health <-  top_words_health %>% 
  mutate(labels = paste(topic, top_words, sep = ', ')) %>% 
  select(labels, topic) %>% deframe()


# Topic proportions -------------------------------------------------------

topic_prop_paths <- dir_ls('results/', regexp = 'topic_prop')

topic_proportion <- tibble(paths=topic_prop_paths) %>% 
  mutate(name = 'topic_proportion',
         type = ifelse(str_detect(paths, 'health'),yes ='health','socsci'),
         # weighted = str_detect(paths, 'weighted'),
         table = map(paths, ~ read_csv(.x) %>% mutate(topic = factor(topic)))) 

retrieve_tp <- function(set){
  topic_proportion %>%
    filter(type==set) %>%
    unnest(cols = c(table)) %>%
    select(-c(paths, name, type))

}


# load main tables --------------------------------------------------------



tables <- tibble(paths = dir_ls('figures/data/', regexp = 'dist|join|marginal')) %>% 
  mutate(name = str_extract(paths, '(?<=figures/data/).*(?=_gender)'),
         type = ifelse(str_detect(paths, 'health'),yes ='health','socsci'),
         table = map(paths, read_csv),
         table = map2(table,type, organise_table))


citations <- tibble( paths = dir_ls('figures/data/', regexp = 'citations')) %>% 
  mutate(name = str_extract(paths,'(?<=(health|socsci)_).*(?=.csv)'),
         type = ifelse(str_detect(paths, 'health'),yes ='health','socsci'),
         table = map(paths, read_csv))  %>% 
  select(-paths) %>% 
  mutate(table = map2(table,type, organise_table)) %>%  
  pivot_wider(names_from = name, values_from = table) %>% 
  unnest(cols = c(mean, median, sd), names_repair = 'unique') %>% 
  select(type, topic = topic...2, group= group...3, gender= gender...4,prob=prob...5, mean = prob...5, sd = prob...17,median = prob...11, top_words = top_words...6) %>% 
  group_by(type) %>% 
  nest() %>% 
  mutate(name = 'citations',
         paths = fs_path('')) %>% 
  select(paths, name, type, table=data)


tables <- tables %>% 
  bind_rows(citations)

rm(citations)

rm(topic_prop_paths)



# rm(average_citation_topics_paths)

racial_groups <- c("White", "Hispanic", "Black", "Asian" )

# general distribution (all disciplines and census) ----------------------------------------------------
#2010 census data taken from https://www.census.gov/data/tables/time-series/demo/popest/2010s-national-detail.html
census_general_dist <- read_csv('figures/data/census.csv') %>%
  filter(group!='Other') %>% 
  pivot_longer(Men:Women, names_to='gender',values_to= 'n') %>% 
  mutate(census = n/sum(n),
         group = case_when(group=='Hispanic' ~'Latinx',
                           TRUE ~group))

discipline_gender_agg <- read_csv('results/discipline_gender_agg.csv') %>% 
  mutate(group = case_when(group=='hispanic' ~'Latinx',
                           TRUE ~group))

full_dataset_general_dist <- discipline_gender_agg %>% 
  mutate(gender = case_when(gender == 'M' ~ 'Men',
                            gender == 'F' ~ 'Women'),
         group = str_to_title(group)) %>% 
  group_by(gender, group) %>% 
  summarise(full_dataset = sum(joint_prob))

soc_sci_general_dist <- retrieve_table(result = 'joint_prob',set = 'socsci') %>% 
  group_by(gender, group) %>% 
  summarise(social_sciences = sum(prob))

health_general_dist <- retrieve_table(result = 'joint_prob',set = 'health') %>% 
  group_by(gender, group) %>% 
  summarise(health = sum(prob))

avg_citations_socsci <- retrieve_table('citations', set = 'socsci') %>%
  left_join(retrieve_tp('socsci') %>% rename(topic_proportion = proportion),by = "topic") %>% 
  group_by(group, gender) %>% 
  summarise(average_cites_social_sciences = sum(mean*topic_proportion))

avg_citations_health <- retrieve_table('citations', set = 'health') %>% 
  left_join(retrieve_tp('health') %>% rename(topic_proportion = proportion),by = "topic") %>% 
  group_by(group, gender) %>% 
  summarise(average_cites_health = sum(prob*topic_proportion))

general_dist <- census_general_dist %>% 
  left_join(full_dataset_general_dist, by = c("group", "gender")) %>% 
  left_join(soc_sci_general_dist, by = c("group", "gender")) %>% 
  left_join(health_general_dist, by = c("group", "gender")) %>% 
  left_join(avg_citations_socsci, by = c("group", "gender")) %>% 
  left_join(avg_citations_health, by = c("group", "gender")) %>% 
  mutate(group = case_when(group=='Hispanic' ~'Latinx',
                           TRUE ~group))

rm(full_dataset_general_dist,census_general_dist,soc_sci_general_dist,avg_citations_socsci,avg_citations_health)
rm(topic_proportion)
# Disciplines -------------------------------------------------------------

discipline_gender_agg <- discipline_gender_agg %>% 
  group_by(EDiscipline) %>% 
  mutate(EDiscipline_prob = sum(joint_prob)) %>% 
  group_by(ESpecialite) %>% 
  mutate(ESpecialite_prob = sum(joint_prob)) %>% 
  group_by(ESpecialite,gender) %>% 
  mutate(gender_prob = sum(joint_prob)/ESpecialite_prob) %>% 
  group_by(ESpecialite,group) %>% 
  mutate(racial_group_prob = sum(joint_prob)/ESpecialite_prob) %>% 
  group_by(ESpecialite,group,gender) %>% 
  mutate(group=str_to_title(group)) %>% 
  mutate(marginal_by_speciality = sum(joint_prob)/ESpecialite_prob) %>% 
  ungroup()



# What If data ------------------------------------------------------------
# Topic proportion will differ from the aggregated topic proportion from join_prob because there are articles without inferred gender --> ignored in joint_prob. Should recalculate 
# the topic prop from joint_prob for comparability


expansion_factors <- general_dist %>% 
  group_by(group, gender) %>% 
  transmute(full_dataset = census/full_dataset,
            social_sciences = census/social_sciences,
            health = census/health)


whatif_socsci <- retrieve_table(result = 'joint_prob',set = 'socsci') %>% 
  left_join(expansion_factors, by = c("gender", "group")) %>%
  group_by(topic) %>% 
  summarise(top_words = unique(top_words),
            topic_proportion = sum(prob),
            wi_topic_proportion = sum(prob*social_sciences),
            prop_diff = wi_topic_proportion/topic_proportion -1)

whatif_health <- retrieve_table(result = 'joint_prob',set = 'health') %>% 
  left_join(expansion_factors, by = c("gender", "group")) %>%
  group_by(topic) %>% 
  summarise(top_words = unique(top_words),
            topic_proportion = sum(prob),
            wi_topic_proportion = sum(prob*health),
            prop_diff = wi_topic_proportion/topic_proportion -1)

whatif_full <- discipline_gender_agg %>% 
  mutate(gender = case_when(gender == 'M' ~ 'Men',
                            gender == 'F' ~ 'Women'),
         group = str_to_title(group)) %>% 
  left_join(expansion_factors, by = c("gender", "group")) %>% 
  group_by(EDiscipline, ESpecialite) %>% 
  summarise(topic_proportion = sum(joint_prob),
            wi_topic_proportion = sum(joint_prob*full_dataset),
            prop_diff = wi_topic_proportion/topic_proportion -1)


tables <- tables %>% 
  bind_rows(tibble(paths = fs_path(''),name = 'what_if',type = 'socsci', table =list(whatif_socsci))) %>% 
  bind_rows(tibble(paths = fs_path(''),name = 'what_if',type = 'health', table =list(whatif_health))) %>% 
  bind_rows(tibble(paths = fs_path(''),name = 'what_if',type = 'full', table =list(whatif_full))) 


rm(whatif_full,whatif_health,whatif_socsci)

# lda_robustness

lda_robustness <- read_csv('results/cosine_similarities_multiple_runs.csv')
lda_robustness_scaled <- read_csv('results/cosine_similarities_multiple_runs_scaled.csv')

 
## All fields

all_fields_paths <- dir_ls('results/lda_fields/')

all_fields_tables <- tibble(path=all_fields_paths) %>% 
  mutate(name = str_extract(path, 'dist_diff_topic|joint_prob|marginal_by_group|marginal_by_topic|top_words|topic_proportion'),
         discipline =str_extract(path,glue('(?<={name}_).*(?=.csv)')),
         table = map(path, read_csv))

get_disp_table <- function(result,set){
  all_fields_tables %>% 
    filter(name==result, discipline==set) %>% 
    unnest(cols = c(table)) %>% 
    select(-c(path, name, discipline))
  
}

organise_table_disciplines <- function(name, df, set){
  
  tw <- get_disp_table('top_words',set)
  tp <- get_disp_table('topic_proportion',set)
  
  if (name=='top_words') {
    return(tw)
  }
  if (name == 'topic_proportion') {
    tp
  }else{

    df %>% 
      pivot_longer(!topic,names_to = c("group", "gender"),
                   names_pattern = "(.*)_(.*)",
                   values_to = 'prob') %>% 
      mutate(gender = case_when(gender == 'M' ~ 'Men',
                                gender == 'F' ~ 'Women'),
             group = str_to_title(group)) %>% 
      left_join(tw, by='topic') %>% 
      left_join(tp, by='topic') %>%
      rename(topic_proportion = proportion) %>% 
      mutate(topic = factor(topic))
  }
}


all_fields_tables <- all_fields_tables %>% 
  mutate(table = pmap(list(name, table, discipline), organise_table_disciplines))


disciplines <- tibble(discipline = unique(all_fields_tables$discipline)) %>% 
  mutate(label = str_replace_all(discipline,'_',' ')) %>%
  select(label, discipline) %>% 
  deframe()


### Gini

citations_gini_socsci <- read_csv('results/citations_gini_socsci.csv') %>% 
  mutate(topic = factor(topic))
citations_gini_health <- read_csv('results/citations_gini_health.csv') %>% 
  mutate(topic = factor(topic))


## Collaborations

groups <-c("white_M","black_M","hispanic_M","asian_M","asian_F","hispanic_F", "black_F","white_F")
groups_label <- c( "White\nMen","Black\nMen","Latinx\nMen","Asian\nMen",
                   "Asian\nWomen","Latinx\nWomen","Black\nWomen","White\nWomen")

# groups <-paste(tolower(racial_groups), rep(c('M', 'F'), each=4),sep='_')
# groups_label <- paste(racial_groups, rep(c('Men', 'Women'), each=4),sep=' ')

first_last_coll_dist_diff <- read_csv('results/first_last_author_dist_diff.csv' ) %>% 
  rename(from = X1) %>% 
  pivot_longer(-from, names_to = 'to', values_to = 'value') %>% 
  mutate(to = factor(to, levels = groups, labels = groups_label),
         from = factor(from, levels = rev(groups), labels = rev(groups_label)),
         d_type = 'first_last',
         v_type = 'diff')

first_last_coll_row_prob <- read_csv('results/first_last_author_marginal_by_row.csv' ) %>% 
  rename(from = X1) %>% 
  pivot_longer(-from, names_to = 'to', values_to = 'value') %>% 
  mutate(to = factor(to, levels = groups, labels = groups_label),
         from = factor(from, levels = rev(groups), labels = rev(groups_label)),
         d_type = 'first_last',
         v_type = 'probabilty')

all_all_coll_dist_diff <- read_csv('results/all_all_dist_diff.csv' ) %>% 
  rename(from = X1) %>% 
  pivot_longer(-from, names_to = 'to', values_to = 'value') %>% 
  mutate(to = factor(to, levels = groups, labels = groups_label),
         from = factor(from, levels = rev(groups), labels = rev(groups_label)),
         d_type = 'all',
         v_type = 'diff')

all_all_coll_row_prob <- read_csv('results/all_all_marginal_by_row.csv' ) %>% 
  rename(from = X1) %>% 
  pivot_longer(-from, names_to = 'to', values_to = 'value') %>% 
  mutate(to = factor(to, levels = groups, labels = groups_label),
         from = factor(from, levels = rev(groups), labels = rev(groups_label)),
         d_type = 'all',
         v_type = 'probabilty')


collaborations <- bind_rows(list(first_last_coll_dist_diff,first_last_coll_row_prob,all_all_coll_dist_diff,all_all_coll_row_prob))

rm(list = c('first_last_coll_dist_diff','first_last_coll_row_prob','all_all_coll_dist_diff','all_all_coll_row_prob'))


# SED data ----------------------------------------------------------------

parse_sed <- function(sed){
  
  sed %>% 
    rename(headers=1) %>% 
    filter(!headers %in% c("Unknown citizenship","Not Hispanic or Latino")) %>% 
    mutate(iscitizenship = str_detect(headers,'(U.S. citizen or permanent resident)|(Temporary visa holder)'),
           citizenship = case_when(iscitizenship ~headers,
                                   TRUE ~'total'),
           group = case_when(citizenship == "total"~ headers,
                             citizenship == "U.S. citizen or permanent resident" ~lag(headers),
                             citizenship == "Temporary visa holder" ~lag(headers,n = 2),
           ),
           group = case_when(group == "Hispanic or Latino" ~'Latinx',
                             group == "Black or African American" ~'Black',
                             TRUE ~group)) %>% 
    filter(group %in% c('Latinx','Black','White','Asian'), citizenship != 'total') %>% 
    select(group,citizenship, everything(), -c(iscitizenship,headers)) %>% 
    mutate_at(vars(-group,-citizenship), as.numeric) %>%
    pivot_longer(cols = -c(group,citizenship),
                 names_to = "year",
                 values_to = 'n') %>% 
    group_by(group, citizenship) %>% 
    summarise(n = sum(n)) %>% 
    group_by(group) %>% 
    summarise(citizenship,
              n=n,
              p = n/sum(n))
  
}

# sed_total <- readxl::read_excel('../github_repo/data/nsf21308-tab019.xlsx', skip = 3)  %>% parse_sed()
sed_men <- readxl::read_excel('../github_repo/data/nsf21308-tab020.xlsx', skip = 3)  %>% parse_sed()
sed_women <- readxl::read_excel('../github_repo/data/nsf21308-tab021.xlsx', skip = 3)  %>% parse_sed()

sed_fields <- read_csv("~/GIT/1_scinet/race/github_repo/data/SED_export_table_2021-05-19T10_35_21.706Z.csv", 
                       skip = 7) %>% 
  select(-X18) %>% 
  rename(year=1) %>%
  filter(year %in% 2008:2019) %>% 
  pivot_longer(-year) %>% 
  mutate(temporary_visa_holder = str_detect(name,'_1'),
         citizenship = case_when(temporary_visa_holder ~ 'Temporary visa holder',
                                 !temporary_visa_holder ~ 'U.S. citizen or permanent resident'),
         field = str_remove_all(name,'_1'),
         value = as.numeric(sub(",", "", value, fixed = TRUE))) %>% 
  group_by(field, citizenship) %>%
  summarise(n = sum(value)) %>% 
  group_by(citizenship) %>%
  mutate(field = factor(field, levels = c("Mathematics and computer sciences",
                                          "Physical sciences and earth sciences",
                                          "Engineering",
                                          "Life sciences",
                                          "Psychology and social sciences",
                                          "Education",
                                          "Humanities and arts",
                                          "Other non-S&E")),
         p = n/sum(n))




sed <- sed_men %>% 
  mutate(gender = 'Men') %>% 
  bind_rows(sed_women %>% 
              mutate(gender = 'Women') )

ratio_sed <- sed %>%
  select(-n) %>% 
  pivot_wider(names_from = citizenship,values_from = p) %>% 
  mutate(ratio = `U.S. citizen or permanent resident`/(`Temporary visa holder`+`U.S. citizen or permanent resident`)) %>% 
  select(group, gender, ratio)

general_dist <- general_dist %>%
  left_join(ratio_sed,by = c("group", "gender")) %>% 
  mutate(norm_wos = full_dataset*ratio,
         norm_wos = norm_wos/sum(norm_wos))

rm(sed_men, sed_women,ratio_sed)
# save image --------------------------------------------------------------

save.image('data.Rdata')




