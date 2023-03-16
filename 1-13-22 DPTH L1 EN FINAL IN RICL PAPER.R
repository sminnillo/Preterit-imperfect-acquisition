#4-9-21 Sophia Minnillo

Author: Sophia Minnillo
Copyright 2023
Distributed under terms of GNU General Public License v3

#In this script, I analyze the longitudinal COWS-L2H
#data in order to test the Default Past Tense Hypothesis.

#The 26 participants are L1 English, L2 Spanish learners.

#set up
library(tidyverse)
library(Rmisc) #for SummarySE

## I. Metadata ####

#read in metadata
long_beg_meta <- read_csv("long_beg_meta.csv")%>%
  filter(level_clean_factor != 21)%>% #filtered so only SPA 1,2,3
  filter(ID != 152099) #removed participant who had done SPA 2 twice

#export this to use in python
#write_csv(long_beg_meta, "long_beg_meta_041821.csv")

#get number of essays by course level
long_beg_meta_sum <- long_beg_meta %>%
  group_by(level_clean_factor) %>%
  summarize(num_essays = n())
#52 essays for each level: what expected

#get number of essays by participant
long_beg_meta_sum1 <- long_beg_meta %>%
  group_by(ID) %>%
  summarize(num_essays = n())
#6 essays per participant: what expected

## 1.1 Demographics ####
#all of the data
tout <- read_csv("metadata_counts_complexity_measures.csv")%>%
  select(-...1)

#filter so only looking at longitudinal group
dem_long <- long_beg_meta %>%
  left_join(tout)%>%
  mutate(level = level_clean_factor)%>%
  mutate( #textual genre
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)
#View(dem_long)

#View(dem_long)
#calculate average word length for all the essays
mean(dem_long$NumberW)
#229

## II. Tokens: Python Data Collection ####
#using long_beg_meta
#read pret
new_pret_df <- read_csv("long_pret_tokens_041821.csv")%>%
  select(-...1)
##combine the two: I changed this to be a left join
combined_pret <- long_beg_meta %>%
  left_join(new_pret_df)
#do pivot longer
combined_pret1 <- combined_pret %>% #max is 30 columns here
  pivot_longer('0':'30', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#977 total preterit forms

#read imperfect
new_imp_df <- read_csv("long_imp_tokens_041821.csv")%>%
  select(-...1)

# View(new_imp_df)

##combine the two
combined_imp <- long_beg_meta %>%
  left_join(new_imp_df) #162 is right
#do pivot longer
combined_imp1 <- combined_imp %>% #max is 24 columns here
  pivot_longer('0':'24', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#572 total imp tokens

#orient dfs by stage (level and essay genre) for PRET
combined_pret3 <- combined_pret1 %>%
  mutate(level = level_clean_factor)%>%
  mutate( #textual genre
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)

#View(combined_pret3)

#number of verbs produced by course level in PRET
combined_pret4 <- combined_pret3 %>%
  group_by(stage)%>%
  summarize(num_pret_tokens = n())

#orient dfs by stage (level and essay genre) for IMP
combined_imp3 <- combined_imp1 %>%
  mutate(level = level_clean_factor)%>%
  mutate(
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)

#View(combined_imp3)

#number of verbs produced by course level in IMP
combined_imp4 <- combined_imp3 %>%
  group_by(stage)%>%
  summarize(num_imp_tokens = n())

#bring these two dfs together
token_count_df_python <- combined_pret4 %>%
  left_join(combined_imp4)

# View(token_count_df_python)

## III. Lemmas: Python Data Collection ####

#read pret
new_pret_df_lemmas <- read_csv("long_pret_lemmas_041821.csv")%>%
  select(-...1)
##combine the two
combined_pret_lemmas <- long_beg_meta %>%
  left_join(new_pret_df_lemmas)
#do pivot longer
combined_pret1_lemmas <- combined_pret_lemmas %>% #max is 30 columns here
  pivot_longer('0':'30', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#977 total preterit forms

#read imperfect
new_imp_df_lemmas <- read_csv("long_imp_lemmas_041821.csv")%>%
  select(-...1)

#View(new_imp_df_lemmas)

##combine the two
combined_imp_lemmas <- long_beg_meta %>%
  left_join(new_imp_df_lemmas) #162 is right
#do pivot longer
combined_imp1_lemmas <- combined_imp_lemmas %>% #max is 24 columns here
  pivot_longer('0':'24', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)

#View(combined_imp1_lemmas)
#572 total imp tokens

#orient dfs by stage (level and essay genre) for PRET
combined_pret3_lemmas <- combined_pret1_lemmas %>%
  mutate(level = level_clean_factor)%>%
  mutate(
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)

#number of verbs produced by course level in PRET
combined_pret4_lemmas <- combined_pret3_lemmas %>%
  group_by(stage)%>%
  summarize(num_pret_tokens = n())

#orient dfs by stage (level and essay genre) for IMP
combined_imp3_lemmas <- combined_imp1_lemmas %>%
  mutate(level = level_clean_factor)%>%
  mutate(
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)

#number of verbs produced by course level in IMP
combined_imp4_lemmas <- combined_imp3_lemmas %>%
  group_by(stage)%>%
  summarize(num_imp_tokens = n())

#bring these two dfs together
token_count_df_python_lemmas <- combined_pret4_lemmas %>%
  left_join(combined_imp4_lemmas)
#same for reanalysis using python

#View(token_count_df_python_lemmas)


## IV. Visualization ####
#x-axis levels: so in correct order (factor data type)
x_axis_levels = c('SPA 1 midpoint', 'SPA 1 end', 'SPA 2 midpoint', 'SPA 2 end',
                  'SPA 3 midpoint', 'SPA 3 end')

#combined table with correct labels etc
df_token_count_df_python_lemmas <- token_count_df_python_lemmas%>%
  mutate(
    x_axis = factor(case_when(
      stage == '1_description' ~ 'SPA 1 midpoint',
      stage == '1_narration' ~ 'SPA 1 end',
      stage == '2_description' ~ 'SPA 2 midpoint',
      stage == '2_narration' ~ 'SPA 2 end',
      stage == '3_description' ~ 'SPA 3 midpoint',
      stage == '3_narration' ~ 'SPA 3 end'
    ), levels = x_axis_levels))%>%
  rename(
    `preterit` = num_pret_tokens,
    `imperfect` = num_imp_tokens
  )%>%
  pivot_longer(c(`preterit`, `imperfect`), names_to = "tense_aspect", values_to = "count")

##now graph this
ggplot(df_token_count_df_python_lemmas, aes(x = x_axis, y = count, fill = tense_aspect))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  labs(x = "Data Collection Time", y = "Total Use of Past Form", fill = "Tense-Aspect")+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))


## V. At which levels start producing forms? ####
### 4.1 Preterit ####
#going to combined_pret and combine with the full participant info
combined_pret12 <- combined_pret %>% #max is 30 columns here
  pivot_longer('0':'30', names_to = "number", values_to = "verb")%>%
  select(-number)
  
  
#changing all NAs to 0s
combined_pret12$verb[is.na(combined_pret12$verb)] <- 0
#turning verbs produced to 1s
combined_pret12 <- combined_pret12 %>%
  mutate(pret_count =
    case_when(
      verb == 0 ~ 0,
      TRUE ~ 1
    ))
#sum number of tokens per essay
combined_pret13 <- combined_pret12 %>%
  group_by(ID, level_clean_factor, quarter, prompt) %>%
  summarize(
    num_pret_tokens = sum(pret_count)
  )%>%
  mutate(level = level_clean_factor)%>%
  mutate(
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)
#I checked this with the case study group and it's correct

#let's look only at Spanish 1 description (midpoint)
combined_pret13_SPA1_desc <- combined_pret13 %>%
  filter(stage == '1_description')%>%
  arrange(desc(num_pret_tokens))
#very few

#let's look only at Spanish 1 narration (end)
combined_pret13_SPA1_nar <- combined_pret13 %>%
  filter(stage == '1_narration')%>%
  arrange(desc(num_pret_tokens))

#let's visualize variance and median with a boxplot
ggplot(combined_pret13, aes(x = stage, y = num_pret_tokens))+
  geom_boxplot()+
  labs(x = "Level and Prompt", y = "Preterit Tokens")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))+
  ylim(0, 35)

### 4.2 Imperfect ####
#going to combined_imp and combine with the full participant info
combined_imp12 <- combined_imp %>% #max is 24 columns here
  pivot_longer('0':'24', names_to = "number", values_to = "verb")%>%
  select(-number)

#changing all NAs to 0s
combined_imp12$verb[is.na(combined_imp12$verb)] <- 0
#turning verbs produced to 1s
combined_imp12 <- combined_imp12 %>%
  mutate(imp_count =
           case_when(
             verb == 0 ~ 0,
             TRUE ~ 1
           ))
#sum number of tokens per essay
combined_imp13 <- combined_imp12 %>%
  group_by(ID, level_clean_factor, quarter, prompt) %>%
  summarize(
    num_imp_tokens = sum(imp_count)
  )%>%
  mutate(level = level_clean_factor)%>%
  mutate(
    genre = case_when(
      prompt == 'famous' | prompt == 'special' ~ 'description',
      prompt == 'vacation' | prompt == 'terrible' ~ 'narration'
    )
  )%>%
  unite(stage, level, genre)
#I checked this with the case study group and it's correct

#let's look only at Spanish 1 description (midpoint)
combined_imp13_SPA1_desc <- combined_imp13 %>%
  filter(stage == '1_description')%>%
  arrange(desc(num_imp_tokens))
#very few

# View(combined_imp13_SPA1_desc)

#let's look only at Spanish 1 narration (end)
combined_imp13_SPA1_nar <- combined_imp13 %>%
  filter(stage == '1_narration')%>%
  arrange(desc(num_imp_tokens))

#View(combined_imp13_SPA1_nar)

#let's visualize variance and midpoint with a boxplot
ggplot(combined_imp13, aes(x = stage, y = num_imp_tokens))+
  geom_boxplot()+
  labs(x = "Level and Prompt", y = "Imperfect Tokens")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))+
  ylim(0,35)

## VI. True Beginners Only ####
#12-7-21: based on reviews
#combined_pret3 and combined_imp3
#demographics: dem_long
#participants who had prior experience with Spanish:
  #154983
  #172111
  
#subset into prior Spanish experience vs. not
combined_pret3<- combined_pret3%>%
  mutate(tense = 'Preterit')

combined_imp3<- combined_imp3%>%
  mutate(tense = 'Imperfect')

#combine the two
combined_3 <- rbind(combined_pret3, combined_imp3)%>%
  mutate(learner_status =
           case_when(
             ID == 172111 |ID == 154983 ~ 'false beginner',
             TRUE ~ 'beginner'
           )) %>%#need to account for the length of learners' essays
  left_join(dem_long[,c(1:4,48)], by = c('quarter', 'prompt', 'ID', 'level_clean_factor'))
#now have wordlength info

#calculate number of tokens produced by each participant at each stage
ROC <- combined_3%>%
  group_by(learner_status, quarter, prompt, ID, stage, tense)%>%
  summarize(num_tokens = n())
#View(ROC)

#number of tokens by essay
dem_long_wc <- dem_long[,c(1:4,30,48)]
#View(dem_long_wc)

#join with ROC to get proportions and account for essays where didn't produce anything
ROC_wc <- dem_long_wc %>%
  left_join(ROC)%>% #need to recode learner status
  mutate(learner_status =
           case_when(
             ID == 172111 |ID == 154983 ~ 'false beginner',
             TRUE ~ 'beginner'
           ))
#View(ROC_wc)
#replace count NAs with 0
ROC_wc$num_tokens[is.na(ROC_wc$num_tokens)] <- 0

#pivot wider to account for people who produced neither
#have to set those equal to 0
ROC_wc <- ROC_wc%>%
  pivot_wider(names_from = tense, values_from = num_tokens)

#replace NAs again
ROC_wc$Preterit[is.na(ROC_wc$Preterit)] <- 0 #pret
ROC_wc$Imperfect[is.na(ROC_wc$Imperfect)] <- 0 #imperfect

#now let's find use of pret or imp per 100 words
ROC_wc1 <- ROC_wc %>%
  #rowwise()%>%
  mutate(
    # preterit = Preterit / NumberW * 100,
    # imperfect = Imperfect / NumberW * 100
    preterit = Preterit,
    imperfect = Imperfect
  )%>%
  pivot_longer(c(`preterit`, `imperfect`), 
               names_to = "tense_aspect",
               values_to = "count")
#View(ROC_wc)

#working with individual counts
ROC_wc_indiv <- ROC_wc %>%
  filter(learner_status == 'beginner')%>%
  select(-`NA`)

#now let's filter for who has 0 in SPA 1
ROC_wc_indiv_SPA1 <- ROC_wc_indiv %>%
  filter(stage == '1_description' | stage == '1_narration' )%>%
  filter(Preterit == 0 & Imperfect == 0) %>%
  group_by(ID) %>%
  summarize(count = n())%>%
  filter(count == 2)

#8 participants produced none in all of SPA 1
print(ROC_wc_indiv_SPA1$ID)
#120 127219 127891 128151 128952 143816 169768 171906

#INDIV: histograms
#preterit
ggplot(ROC_wc_indiv, 
       aes(x = Preterit, color = stage))+
  geom_histogram()+
  facet_wrap(~stage)

#imperfect
ggplot(ROC_wc_indiv, 
       aes(x = Imperfect, color = stage))+
  geom_histogram()+
  facet_wrap(~stage)

#do it only at certain stage
ROC_wc_indiv_3N <- ROC_wc_indiv %>%
  filter(stage == '3_narration')
#preterit
ggplot(ROC_wc_indiv_3N, 
       aes(x = Preterit))+
  geom_histogram()+
  facet_wrap(~ID)

#imperfect
ggplot(ROC_wc_indiv_3N, 
       aes(x = Imperfect))+
  geom_histogram()+
  facet_wrap(~ID)


#x-axis levels: so in correct order (factor data type)
x_axis_levels = c('SPA 1 midpoint', 'SPA 1 end', 'SPA 2 midpoint', 'SPA 2 end',
                  'SPA 3 midpoint', 'SPA 3 end')

#average by level: filtered to only include true beginners
#12-12-21
ROC_wc_level_avg <- ROC_wc1 %>%
  filter(learner_status == 'beginner')%>% #true beginner
  mutate(
    x_axis = factor(case_when(
      stage == '1_description' ~ 'SPA 1 midpoint',
      stage == '1_narration' ~ 'SPA 1 end',
      stage == '2_description' ~ 'SPA 2 midpoint',
      stage == '2_narration' ~ 'SPA 2 end',
      stage == '3_description' ~ 'SPA 3 midpoint',
      stage == '3_narration' ~ 'SPA 3 end'
    ), levels = x_axis_levels))%>%
    group_by(x_axis, tense_aspect)%>%
    summarize(mean_count = mean(count),
              total_count = sum(count))
#View(ROC_wc_level_avg)
  
##now bargraph this
ggplot(ROC_wc_level_avg, 
       aes(x = x_axis, y = total_count, fill = tense_aspect))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  # labs(x = "Data Collection Time", y = "Mean Frequency per 100 Tokens",
  #      fill = "Tense-Aspect")+
  labs(x = "Data Collection Time", y = "Total Use of Past Form",
       fill = "Tense-Aspect")+
  scale_fill_grey()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))

# #by group actually
# ROC1 <- combined_3%>%
#   group_by(learner_status,stage, tense)%>%
#   summarize(num_tokens = n())

#let's plot this now
ggplot(data = ROC_wc1,
       aes(x = stage,
           y = count,
           #group = learner_status,
           color = tense_aspect))+ 
  geom_boxplot()+
  facet_grid(tense_aspect~learner_status)+
  labs(x = "Data Collection Time", y = "Past form frequency per 100 Tokens", color = "Tense-Aspect")+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))

#now, let's see what the averages are and plot those as line graphs
ROC_wc_mean <- ROC_wc1 %>%
  group_by(learner_status, stage, tense_aspect)%>%
  summarize(mean_count = mean(count),
            sd_count = sd(count))
#View(ROC_wc_mean)

#let's graph this now!
ggplot(data = ROC_wc_mean,
       aes(x = stage,
           y = mean_count,
           group = learner_status, #need this for line
           color = learner_status))+ 
  geom_point()+  
  geom_line()+
  facet_grid(tense_aspect~learner_status)+
  labs(x = "Data Collection Time", y = "Mean past form frequency per 100 Tokens", color = "Learner Group")+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))

#let's actually calculate the rate of change
#weird df operations
ROC_wc_mean_rates <- ROC_wc_mean %>%
  select(-sd_count)%>%
  pivot_wider(names_from = stage, values_from = mean_count)%>%
  rowwise()%>%
  mutate(
    `1st` = `1_narration` - `1_description`,
    `2nd` = `2_description` - `1_narration`,
    `3rd` = `2_narration` - `2_description`,
    `4th` = `3_description` - `2_narration`,
    `5th` = `3_narration` - `3_description` 
  )%>%
  #cut out extra columns
  select(-c(3:8))%>%
  #pivot back
  pivot_longer(c(`1st`, `2nd`, `3rd`, `4th`, `5th`),
               names_to = 'Transition',
               values_to = 'count')
#View(ROC_wc_mean_rates)
#plot this now
ggplot(data = ROC_wc_mean_rates,
       aes(x = Transition,
           y = count,
           group = learner_status, #need this for line
           color = learner_status))+ 
  geom_point()+  
  geom_line()+
  facet_wrap(~tense_aspect)+
  labs(x = "Data Collection Time", y = "Rate of change in past form frequency (per 100 Tokens)", color = "Learner Group")+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))

#mean essay length for true beginners
dem_long_wc_true_beg <- dem_long_wc %>%
  mutate(learner_status =
           case_when(
             ID == 172111 |ID == 154983 ~ 'false beginner',
             TRUE ~ 'beginner'
           )) %>%
  filter(learner_status == 'beginner')

mean(dem_long_wc_true_beg$NumberW)
#227.86  

## VII. New Sample Texts ####

#12-22-21
#for graphs later on
x_axis_levels = c('SPA 1 midpoint', 'SPA 1 end', 'SPA 2 midpoint', 'SPA 2 end',
                  'SPA 3 midpoint', 'SPA 3 end')

#8 participants produced none in all of SPA 1
#120 127219 127891 128151 128952 143816 169768 171906
#turning into a list
true_true_beg <- as.list(ROC_wc_indiv_SPA1$ID)

#now filter for only their trajectories based on ID list
#folks who didn't start off using the preterit-imperfect
ROC_wc_indiv_0 <- ROC_wc_indiv %>%
  filter(ID %in% true_true_beg)%>% #now pivot longer
  pivot_longer(cols = c(Preterit, Imperfect), 
               names_to = 'TenseAspect',
               values_to = 'count')%>%
  rowwise()%>%
  mutate(tokens_per_100_w = count / NumberW * 100)

#view(ROC_wc_indiv_0)
#mean word length of essays
ROC_wc_indiv_0_length <- ROC_wc_indiv %>%
  filter(ID %in% true_true_beg)
# view(ROC_wc_indiv_0_length)
mean(ROC_wc_indiv_0_length$NumberW)
#218.625

#let's try to get within-subjects error bars
ROC_wc_indiv_0c <- summarySEwithin(ROC_wc_indiv_0,
                         measurevar="count",
                         withinvars=c("stage","TenseAspect"),
                         idvar="ID")%>%
  mutate(
    x_axis = factor(case_when(
      stage == '1_description' ~ 'SPA 1 midpoint',
      stage == '1_narration' ~ 'SPA 1 end',
      stage == '2_description' ~ 'SPA 2 midpoint',
      stage == '2_narration' ~ 'SPA 2 end',
      stage == '3_description' ~ 'SPA 3 midpoint',
      stage == '3_narration' ~ 'SPA 3 end'
    ), levels = x_axis_levels))
#View(ROC_wc_indiv_0c)

#same but per 100 words
ROC_wc_indiv_0c_100 <- summarySEwithin(ROC_wc_indiv_0,
                                   measurevar="tokens_per_100_w",
                                   withinvars=c("stage","TenseAspect"),
                                   idvar="ID")%>%
  mutate(
    x_axis = factor(case_when(
      stage == '1_description' ~ 'SPA 1 midpoint',
      stage == '1_narration' ~ 'SPA 1 end',
      stage == '2_description' ~ 'SPA 2 midpoint',
      stage == '2_narration' ~ 'SPA 2 end',
      stage == '3_description' ~ 'SPA 3 midpoint',
      stage == '3_narration' ~ 'SPA 3 end'
    ), levels = x_axis_levels))
view(ROC_wc_indiv_0c_100)


#folks who did start off using the preterit-imperfect
ROC_wc_indiv_1 <- ROC_wc_indiv %>%
  filter(!ID %in% true_true_beg)%>% #now pivot longer
  pivot_longer(cols = c(Preterit, Imperfect), 
               names_to = 'TenseAspect',
               values_to = 'count')


#now graph their trajectories
#folks who started at 0
ggplot(data = ROC_wc_indiv_0,
       aes(x = stage,
           y = count,
           #group = `TenseAspect`, #need this for line
           color = `TenseAspect`))+ 
  #geom_boxplot()+  
  geom_line()+
  #facet_wrap(~tense_aspect)+
  labs(x = "Data Collection Time", 
       y = "Use of past form per essay",
       color = "Tense-Aspect")+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))


#line plots
#mean and CI based on summarySEwithin (within-subjects variance)
#start at 0
ggplot(ROC_wc_indiv_0c, aes(x=x_axis, y=count, 
                            group = `TenseAspect`,
                            color = `TenseAspect`)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=count-ci, ymax=count+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(-5,20)+
  labs(x = "Data Collection Time", y = "Mean use of past form", color = "Tense-Aspect")+
  #scale_fill_grey()+
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))+
  scale_color_manual(values=c("grey0", "grey50"))

#per 100 words, started at 0
#write_csv(ROC_wc_indiv_0c_100, "Figure1_RiCL_042822.csv")

##### GRAPH IN PAPER ####
ggplot(ROC_wc_indiv_0c_100, aes(x=x_axis, y=tokens_per_100_w, 
                            group = `TenseAspect`,
                            color = `TenseAspect`)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=tokens_per_100_w-ci, ymax=tokens_per_100_w+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  #ylim(-5,20)+
  labs(x = "Data Collection Time", y = "Mean Use of Past Form per 100 Words", color = "Tense-Aspect")+
  #scale_fill_grey()+
  theme(text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))+
  scale_color_manual(values=c("grey0", "grey50"))


#let's pull up the actual essays of students who started at 0
all_essays <- read_csv('all_present_data_COWS_062521.csv')%>%
  filter(id %in% true_true_beg)%>%
  write_csv('essays_SPA1_0past_122321.csv')

## VIII. Essay Length Adjustment ####
#dem_long_wc
#combined_pret3
#combined_imp3

#combine with essay length info
#pret
combined_pret3_wc <- combined_pret3 %>%
  left_join(dem_long_wc)
#view(combined_pret3_wc)
#imp
combined_imp3_wc <- combined_imp3 %>%
  left_join(dem_long_wc)
#view(combined_imp3_wc)
#this worked

#combine
combined_wc <- rbind(combined_pret3_wc, combined_imp3_wc)
view(combined_wc)

#try to summarize
combined_wc_sum <- combined_wc%>%
  group_by(ID, stage, tense)%>%
  dplyr::summarize(
    tokens_per_100_w = n() / NumberW * 100)%>%
  unique()%>%
  pivot_wider(names_from = tense, values_from = tokens_per_100_w)
#nas to 0s
combined_wc_sum[is.na(combined_wc_sum)] <- 0

#now combine with dem_long_wc
combined_wc_sum1 <- dem_long_wc %>%
  left_join(combined_wc_sum)
#view(combined_wc_sum1)

#nas to 0s
combined_wc_sum1[is.na(combined_wc_sum1)] <- 0

#need to filter out ID == 172111 |ID == 154983: false beginners
combined_wc_sum2 <- combined_wc_sum1 %>%
  filter(ID != 172111 & ID !=154983) %>%
  pivot_longer(c(`Preterit`, `Imperfect`), 
               names_to = "tense_aspect",
               values_to = "count")
#view(combined_wc_sum2) #now at 144

#double checking that including everyone
combined_wc_sum2_check <- combined_wc_sum2 %>%
  group_by(ID)%>%
  dplyr::summarise(count = n())
#yes, 12 essays from each of the 24 participants

#summary SE
#let's try to get within-subjects error bars
#summarySEwithin adjusts the error bars so that inter-subject variability is removed
combined_wc_sum1_SE <- summarySEwithin(combined_wc_sum2,
                                   measurevar="count",
                                   withinvars=c("stage","tense_aspect"),
                                   idvar="ID")%>%
  mutate(
    x_axis = factor(case_when(
      stage == '1_description' ~ 'SPA 1 midpoint',
      stage == '1_narration' ~ 'SPA 1 end',
      stage == '2_description' ~ 'SPA 2 midpoint',
      stage == '2_narration' ~ 'SPA 2 end',
      stage == '3_description' ~ 'SPA 3 midpoint',
      stage == '3_narration' ~ 'SPA 3 end'
    ), levels = x_axis_levels))

#view(combined_wc_sum1_SE)

#double checking that this is right
combined_wc_sum1_SE_check <- combined_wc_sum2%>%
  group_by(stage, tense_aspect)%>%
  dplyr::summarise(mean_use = mean(count))
#view(combined_wc_sum1_SE_check)
#yes, the numbers match

#let's plot the barplots now
ggplot(combined_wc_sum1_SE, 
       aes(x=x_axis, y=count, fill=tense_aspect)) +
  geom_bar(position=position_dodge(.9), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=count-ci, ymax=count+ci)) +
  labs(x = "Data Collection Time", 
       y = "Mean Use of Past Form per 100 Words",
       fill = "Tense-Aspect")+
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.75, vjust = 0.9))
