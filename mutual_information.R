library(here)
library(tidyverse)
library(feather)
library(entropy)
library(philentropy)

epsilon = 0.000000000001

ldp_raw <- read_csv(here("data/ldp_speech_acts.csv"))
act_codes <- read.csv(here("act_codes.csv"))

subj_info <- read_csv(here("ldp_subject_info.csv")) %>%
  mutate(tbi_status = if_else(!is.na(lesion), "traumatic_brain_injury", "typically_developing"))

ldp_acts <- ldp_raw %>%
  rename(act_code = speech_act) %>%
  left_join(act_codes, by = "act_code") %>%
  left_join(subj_info %>% select(id, tbi_status), by = c("child_id" = "id")) %>%
  # only include typically developing kids (excludes kids with a traumatic brain injury)
  filter(tbi_status == "typically_developing") %>%
  group_by(child_id, age_months) %>%
  mutate(prior_act = lag(meaning)) %>%
  ungroup() %>%
  rename(file_id = transcript_file)

ldp_keep <- ldp_acts %>%
  mutate(speaker_role = if_else(speaker_code == "ADU",
                                "Parent", "Child")) %>%
  group_by(file_id, speaker_role) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(speaker_role, file_id, fill = list(n = 0)) %>%
  arrange(file_id) %>%
  pivot_wider(names_from = "speaker_role", values_from = "n") %>%
  filter(Parent > 10, Child > 10)

ldp_acts <- ldp_acts %>%
  mutate(speaker_role = if_else(speaker_code == "ADU",
                                "Parent", "Child")) %>%
  filter(file_id %in% ldp_keep$file_id) %>%
  mutate(age_bin = age_months) 

num_acts <- n_distinct(ldp_acts$act_code)
act_nums <- c(1:num_acts)

joint_dist <- ldp_acts %>%
  group_by(age_bin) %>%
  count(speaker_role, prior_act, meaning) %>%
  complete(speaker_role, prior_act, meaning, fill = list(n = 0)) %>%
  mutate(n = n + epsilon,
         prob = n/sum(n)) %>%
  ungroup()

#code for checking various joint prob tables
# joint_probs <- chunks_nested %>%
#   select(transcript_id, age_bin, matrix) %>%
#   unnest(matrix) %>%
#   pivot_longer(cols = topic_probs, names_to = "current_topic", values_to = "prob") %>%
#   mutate(prob = if_else(prior_topic == current_topic, 100, 0.01)) %>%
#   pivot_wider(names_from = current_topic, values_from = prob)


get_mis <- function(joint) {
  
  prior_act_marginal <- joint %>%
    group_by(age_bin, speaker_role, prior_act) %>%
    summarise(prior_act_prob = sum(prob)) %>% ungroup()
  
  this_act_marginal <- joint %>%
    group_by(age_bin, speaker_role, meaning) %>%
    summarise(this_act_prob = sum(prob)) %>% ungroup()
  
  all_probs <- joint %>%
    rename(joint_prob = prob) %>%
    left_join(prior_act_marginal, 
              by = c("age_bin", "speaker_role", "prior_act")) %>%
    left_join(this_act_marginal, 
              by = c("age_bin", "speaker_role", "meaning"))
  
  parent_given_child <- all_probs %>%
    filter(speaker_role == "Parent") %>%
    mutate(term = joint_prob * log2(joint_prob/(prior_act_prob * this_act_prob))) %>%
    group_by(age_bin) %>%
    summarise(mi = sum(term)) %>%
    mutate(type = "parent_given_child")
  
  child_given_parent <- all_probs %>%
    filter(speaker_role == "Child") %>%
    mutate(term = joint_prob * log2(joint_prob/(prior_act_prob * this_act_prob))) %>%
    group_by(age_bin) %>%
    summarise(mi = sum(term)) %>%
    mutate(type = "child_given_parent")
  
  all_mis <- rbind(child_given_parent, parent_given_child)
  return(all_mis)
}


mis <- get_mis(joint_dist)


#write_csv(mis_all, here("data/mutual_information_vals.csv"))

mis %>%
  ggplot(aes(x = age_bin, y = mi, color = type)) +
  geom_line()

  