#1-14-21 Sophia Minnillo

#In this script, I analyze the cross-sectional CEDEL2
#data in order to test the following hypotheses:
#1) Default Past Tense Hypothesis 
#     (also tested with COWS in '1-13-21 DPTH L1 EN.R')
#2) Distributional Bias Hypothesis
#3) Lexical Aspect Hypothesis

#There are 2 participant groups:
#L1 Spanish: N = 820 
#L1 English, L2 Spanish: N = 611

#See the table of contents on the right

#set up
library(tidyverse)
library(Rmisc) #for SummarySE
#library(ggthemes) # themes for plots

## 1. Metadata ####
### L2 ####
#metadata
CEDEL2_metadata <- read_csv("CEDEL2_metadata2.csv")
view(CEDEL2_metadata)
#metadata with essay length calculated
final_essays1 <- read_csv("CEDEL2_meta_essay_length.csv") %>%
  select(-...1)

#make Proficiency a factor variable
course_levels = c('Lower beginner', 'Upper beginner', 'Lower intermediate', 'Upper intermediate')

#now grouping with full metadata & reclassifying the genres of the essays
metadata1 <- inner_join(CEDEL2_metadata, final_essays1, by = 'Filename')%>%
  distinct() %>% #723 total
  filter(NumberW >= 50)%>% #number of words > 50
  filter(NumberW <= 500)%>% #number of words < 500
  mutate( #genre info added
    genre = 
      case_when(
        `Task number` == 3 |`Task number` == 4 |`Task number` == 5 |`Task number` == 6 |`Task number` == 7 ~ "narrative",
        `Task number` == 13 |`Task number` == 14 ~ "narrative_story_retell", #change 2-15-21
        `Task number` == 1 | `Task number` == 2  ~ "descriptive",
        `Task number` == 9 | `Task number` == 10 | `Task number` == 11 | `Task number` == 12 | `Task number` == 8 ~ "argumentative_analytical" #change here 2-2-21
      )
  )%>%
  mutate(Proficiency = factor(Proficiency, levels = course_levels))%>%
  filter(genre !="argumentative_analytical")
#only including narrative and descriptive essays

#View(metadata1)

#export this to csv for use with python
# write_csv(metadata1, 'CEDEL2_L2_metadata_042221.csv')

#simplifying down to Filename and Proficiency
metadata11 <- metadata1 %>%
  select(Filename, Proficiency)

### NS ####
#already filtered for genre when downloaded data from CEDEL2 website
#metadata for all descriptive & narrative essays but Frog & Chaplin
CEDEL2_meta_NS1 <- read_csv('NS_no_frog_chaplin_metadata.csv')

#metadata including Frog & Chaplin (narrative story retell)
CEDEL2_meta_NS2 <- read_csv('NS_frog_chaplin_metadata.csv')

#combine
CEDEL2_meta_NS <- rbind(CEDEL2_meta_NS1, CEDEL2_meta_NS2)
#884 total

#upload the length of their essays so can filter by essay length
CEDEL2_NS_meta_essay_length <- read_csv('CEDEL2_NS_meta_essay_length.csv')%>%
  select(-...1)

#filter so that essay length is right (>=50, <= 500)
CEDEL2_NS_meta_filtered <- CEDEL2_NS_meta_essay_length %>%
  filter(NumberW >= 50 & NumberW <= 500)
#N = 820 once filtered for essay length

#combine everything together
CEDEL2_NS_meta_filtered1 <- CEDEL2_NS_meta_filtered %>%
  left_join(CEDEL2_meta_NS)
  
#export this to csv for use with python
# write_csv(CEDEL2_NS_meta_filtered1, 'CEDEL2_NS_metadata_042221.csv')

#just Filename
CEDEL2_NS_meta_filtered2 <- CEDEL2_NS_meta_filtered1%>%
  select(Filename)

## 1.1 Demographics ####
### L2 ####

#mean essay length = 206
mean(metadata1$NumberW)
#number of participants by proficiency level
metadata1N <- metadata1 %>%
  group_by(Proficiency) %>%
  dplyr::summarize(num_part = n())

### NS ####
#mean essay length = 231
mean(CEDEL2_NS_meta_filtered1$NumberW)

## 2. Python Data Collection ####

## 2.1 Tokens ####
### L2 ####
#pret
L2_pret_tokens <- read_csv('CEDEL2_pret_tokens_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_pret <- metadata11 %>%
  left_join(L2_pret_tokens)
  
#do pivot longer
combined_pret1 <- combined_pret %>% #max is 45 columns here
  pivot_longer('0':'45', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#3337 total tokens produced

#View(combined_pret1)

#imp
L2_imp_tokens <- read_csv('CEDEL2_imp_tokens_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_imp <- metadata11 %>%
  left_join(L2_imp_tokens)

#do pivot longer
combined_imp1 <- combined_imp %>% #max is 38 columns here
  pivot_longer('0':'38', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#1531 total tokens produced
#View(combined_imp1)


### NS ####
#pret
NS_pret_tokens <- read_csv('CEDEL2_NS_pret_tokens_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_pret_NS <- CEDEL2_NS_meta_filtered2 %>%
  left_join(NS_pret_tokens)

#do pivot longer
combined_pret1_NS <- combined_pret_NS %>% #max is 31 columns here
  pivot_longer('0':'31', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#4327 total tokens produced

#view(combined_pret1_NS)

#imp
NS_imp_tokens <- read_csv('CEDEL2_NS_imp_tokens_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_imp_NS <- CEDEL2_NS_meta_filtered2 %>%
  left_join(NS_imp_tokens)

#do pivot longer
combined_imp1_NS <- combined_imp_NS %>% #max is 25 columns here
  pivot_longer('0':'25', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#2668 total tokens produced

#view(combined_imp1_NS)

## 2.2 Lemmas ####
### L2 ####
#pret
L2_pret_lemmas <- read_csv('CEDEL2_pret_lemmas_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_pret_lemmas <- metadata11 %>%
  left_join(L2_pret_lemmas)

#do pivot longer
combined_pret1_lemmas <- combined_pret_lemmas %>% #max is 45 columns here
  pivot_longer('0':'45', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#3337 total lemmas produced

#imp
L2_imp_lemmas <- read_csv('CEDEL2_imp_lemmas_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_imp_lemmas <- metadata11 %>%
  left_join(L2_imp_lemmas)

#do pivot longer
combined_imp1_lemmas <- combined_imp_lemmas %>% #max is 38 columns here
  pivot_longer('0':'38', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#1531 total tokens produced

### NS ####
#pret
NS_pret_lemmas <- read_csv('CEDEL2_NS_pret_lemmas_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_pret_NS_lemmas <- CEDEL2_NS_meta_filtered2 %>%
  left_join(NS_pret_lemmas)

#do pivot longer
combined_pret1_NS_lemmas <- combined_pret_NS_lemmas %>% #max is 31 columns here
  pivot_longer('0':'31', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#4327 total lemmas produced

#imp
NS_imp_lemmas <- read_csv('CEDEL2_NS_imp_lemmas_042221.csv')%>%
  select(-...1)
##combine the two: left join
#so that excess data points weren't included
combined_imp_NS_lemmas <- CEDEL2_NS_meta_filtered2 %>%
  left_join(NS_imp_lemmas)

#do pivot longer
combined_imp1_NS_lemmas <- combined_imp_NS_lemmas %>% #max is 25 columns here
  pivot_longer('0':'25', names_to = "number", values_to = "verb")%>%
  drop_na() %>% #this excludes participants who didn't produce any verbs
  select(-number)
#2668 total tokens produced

## 3. DPTH ####

### L2 per 100 words, 1-18-22 ####
#number of verbs produced by proficiency level in PRET
combined_pret4_w <- combined_pret1 %>%
  group_by(Filename)%>%
  dplyr::summarize(num_pret_tokens = n())%>%
  left_join(metadata1)%>% #join with essay length info
  mutate(
    pret_per_100_w = num_pret_tokens / NumberW * 100
  )%>%
  select(Filename, Proficiency, pret_per_100_w)%>%
  right_join(metadata11) #binding with main info df to account for all participants
#include num_pret_tokens, NumberW,  to double check the math
#view(combined_pret4_w)
#nb that this does not include participants who did not produce any (about 200)

#number of verbs produced by proficiency level in IMP
combined_imp4_w <- combined_imp1 %>%
  group_by(Filename)%>%
  dplyr::summarize(num_imp_tokens = n())%>%
  left_join(metadata1)%>% #join with essay length info
  mutate(
    imp_per_100_w = num_imp_tokens / NumberW * 100
  )%>%
  select(Filename, Proficiency, imp_per_100_w)%>%
  right_join(metadata11)  #binding with main info df to account for all participants
#include , num_imp_tokens, NumberW  to double check the math
#view(combined_imp4_w)

#bring these two dfs together
token_count_df_python_w <- left_join(combined_pret4_w, combined_imp4_w)
#view(token_count_df_python_w)

#set NA equivalent to 0
token_count_df_python_w[is.na(token_count_df_python_w)] <- 0

#now add in proficiency info
token_count_df_python_w_meta <- left_join(token_count_df_python_w, metadata11)%>%
  dplyr::rename(
    Preterit = pret_per_100_w,
    Imperfect = imp_per_100_w
  )%>%
  pivot_longer(cols = c(Preterit, Imperfect), names_to = 'tense_aspect',
               values_to = "count")%>%
  mutate(
    Proficiency1 =
      case_when(
        Proficiency == 'Lower beginner' ~ 'A1',
        Proficiency == 'Upper beginner' ~ 'A2',
        Proficiency == 'Lower intermediate' ~ 'B1',
        Proficiency == 'Upper intermediate' ~ 'B2'
      )
  )

#view(token_count_df_python_w_meta)

#let's double check that SummarySE is working
tcdfpwm_check <- token_count_df_python_w_meta %>%
  group_by(tense_aspect, Proficiency1)%>%
  dplyr::summarise(
    mean = mean(count),
    sd = sd(count)
  )
view(token_count_df_python_w_meta)
#they are the same :)

#now summarize by proficiency level & tense: summarySE
token_count_df_python_w_prof <- summarySE(token_count_df_python_w_meta,
  measurevar = 'count', groupvars = c('tense_aspect', 'Proficiency1'))

#view(token_count_df_python_w_prof)
#write_csv(token_count_df_python_w_prof, 'Figure2_RiCL_042822.csv')

#let's plot the barplots now
#####IN PAPER#####
ggplot(token_count_df_python_w_prof, 
       aes(x=Proficiency1, y=count, fill=tense_aspect)) +
  geom_bar(position=position_dodge(.9), stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=count-ci, ymax=count+ci)) +
  labs(x = "Proficiency Level", 
       y = "Mean Use of Past Form per 100 Words",
       fill = "Tense-Aspect")+
  scale_fill_grey()+
  ylim(-.25, 6)

### L2 ####
#number of verbs produced by proficiency level in PRET
combined_pret4 <- combined_pret1 %>%
  group_by(Proficiency)%>%
  dplyr::summarize(num_pret_tokens = n())

#number of verbs produced by proficiency level in IMP
combined_imp4 <- combined_imp1 %>%
  group_by(Proficiency)%>%
  dplyr::summarize(num_imp_tokens = n())

#bring these two dfs together
token_count_df_python <- combined_pret4 %>%
  left_join(combined_imp4)

#need to adjust based on number of essays
#at each level

#L1 English num participants by level
essential_info_num_part_level <- metadata11%>%
  group_by(Proficiency)%>%
  dplyr::summarize(num_part = n())

#adjusting counts
token_count_df_python <- token_count_df_python %>%
  left_join(essential_info_num_part_level)

#mean number of forms per participant
token_count_df_python <- token_count_df_python %>%
  rowwise() %>%
  mutate(
    prop_pret = num_pret_tokens / num_part,
    prop_imp = num_imp_tokens / num_part
  )

View(token_count_df_python)
#want to account for text length here

#x-axis levels: so in correct order (factor data type)
x_axis_levels = c('A1', 'A2', 'B1', 'B2')

#combined table with correct labels etc
df_token_count_df_python <- token_count_df_python%>%
  mutate(
    x_axis = factor(case_when(
      Proficiency == 'Lower beginner' ~ 'A1',
      Proficiency == 'Upper beginner' ~ 'A2',
      Proficiency == 'Lower intermediate' ~ 'B1',
      Proficiency == 'Upper intermediate' ~ 'B2',
    ), levels = x_axis_levels))%>%
  dplyr::rename(
    `preterit` = prop_pret,
    `imperfect` = prop_imp
  )%>%
  pivot_longer(c(`preterit`, `imperfect`), names_to = "tense_aspect", values_to = "count")
#view(df_token_count_df_python)

#### Graph ####
##now graph this
ggplot(df_token_count_df_python, aes(x = x_axis, y = count, fill = tense_aspect))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  labs(x = "Proficiency Level", y = "Mean Use of Past Form", fill = "Tense-Aspect")+
  scale_fill_grey()


## 4. Zipfian Distribution ####
### L2 ####

#start with preterit lemmas
L2_lemma_pret <- combined_pret1_lemmas %>%
  group_by(verb) %>%
  dplyr::summarize(freq_lemma_pret = n()) #freq of lemma in pret

#same but for imperfect
L2_lemma_imp <- combined_imp1_lemmas %>%
  group_by(verb) %>%
  dplyr::summarize(freq_lemma_imp = n()) #freq of lemma in imp

#combine
L2_lemma_both <- full_join(L2_lemma_pret, L2_lemma_imp,
                           by = 'verb') %>%
  filter(verb != 'aqui') #excluding what isn't a verb

#need to set things equal to 0
L2_lemma_both$freq_lemma_pret[is.na(L2_lemma_both$freq_lemma_pret)] <- 0
L2_lemma_both$freq_lemma_imp[is.na(L2_lemma_both$freq_lemma_imp)] <- 0

#now do proportions (of total past verbs)
L2_lemma_both <- L2_lemma_both%>%
  rowwise()%>%
  mutate(total_past_by_verb = sum(freq_lemma_pret, freq_lemma_imp))%>%
  mutate(
    prop_pret = freq_lemma_pret / total_past_by_verb,
    prop_imp = freq_lemma_imp / total_past_by_verb
  )
#ok now sum of each
total_pret3 = sum(L2_lemma_both$freq_lemma_pret)
total_imp3 = sum(L2_lemma_both$freq_lemma_imp)

#calculating percentage frequency
L2L_past_list1a <- L2_lemma_both %>%
  cbind(total_pret3, total_imp3)%>%
  mutate(
    per_of_pret = freq_lemma_pret / total_pret3 * 100,
    per_of_imp = freq_lemma_imp / total_imp3 * 100
  )
#10 most frequent verbs and their percentage frequency
L2L_past_list1a_pret <- L2L_past_list1a %>%
  select(verb, per_of_pret, freq_lemma_pret)%>%
  arrange(desc(per_of_pret))
#imp
L2L_past_list1a_imp <- L2L_past_list1a %>%
  select(verb, per_of_imp, freq_lemma_imp)%>%
  arrange(desc(per_of_imp))

#27 most frequent verbs; factor variable (for graphing)
L2L_past_list1a_pret_order <-L2L_past_list1a_pret %>%
  slice_head(n=27)%>% #27 most freq
  mutate(verbs = factor(verb, levels = L2L_past_list1a_pret$verb))

#write_csv(L2L_past_list1a_pret_order, "Figure3_L2pret_RiCL_042822.csv")

#### Graph ####
#####IN PAPER#####
#graph: width 400
ggplot(L2L_past_list1a_pret_order, 
       aes(x = verbs, y = freq_lemma_pret))+
  geom_bar(stat = "identity", position = "dodge", fill="steelblue")+
  labs(x = 'Verb', y= 'Frequency of Verb in Preterit')+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,600)

#imp
#arrange by frequency in imp
L2L_past_list1a_imp_order <- L2L_past_list1a_imp%>%
  arrange(desc(freq_lemma_imp))

#27 most frequent verbs; factor variable
L2L_past_list1a_imp_order <-L2L_past_list1a_imp_order %>%
  slice_head(n=27)%>% #27 most freq
  mutate(verbs = factor(verb, levels = L2L_past_list1a_imp$verb))

#write_csv(L2L_past_list1a_imp_order, "Figure3_L2imp_RiCL_042822.csv")

#graph
ggplot(L2L_past_list1a_imp_order, 
       aes(x = verbs, y = freq_lemma_imp))+
  geom_bar(stat = "identity", position = "dodge", fill="steelblue")+
  labs(x = 'Verb', y= 'Frequency of Verb in Imperfect')+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,600)

### NS ####
#start with preterit lemmas
NS_lemma_pret <- combined_pret1_NS_lemmas %>%
  group_by(verb) %>%
  dplyr::summarize(freq_lemma_pret = n()) #freq of lemma in pret

#same but for imperfect
NS_lemma_imp <- combined_imp1_NS_lemmas %>%
  group_by(verb) %>%
  dplyr::summarize(freq_lemma_imp = n()) #freq of lemma in imp

#combine
NS_lemma_both <- full_join(NS_lemma_pret, NS_lemma_imp,
                           by = 'verb') %>%
  filter(verb != 'aqui')

#need to set things equal to 0
NS_lemma_both$freq_lemma_pret[is.na(NS_lemma_both$freq_lemma_pret)] <- 0
NS_lemma_both$freq_lemma_imp[is.na(NS_lemma_both$freq_lemma_imp)] <- 0

#now do proportions of total past verbs
NS_lemma_both <- NS_lemma_both%>%
  rowwise()%>%
  mutate(total_past_by_verb = sum(freq_lemma_pret, freq_lemma_imp))%>%
  mutate(
    prop_pret = freq_lemma_pret / total_past_by_verb,
    prop_imp = freq_lemma_imp / total_past_by_verb
  )

#now sum of each
NS_total_pret3 = sum(NS_lemma_both$freq_lemma_pret)
NS_total_imp3 = sum(NS_lemma_both$freq_lemma_imp)

#calculating percentage frequency
NS_past_list1a <- NS_lemma_both %>%
  cbind(NS_total_pret3, NS_total_imp3)%>%
  mutate(
    per_of_pret = freq_lemma_pret / NS_total_pret3 * 100,
    per_of_imp = freq_lemma_imp / NS_total_imp3 * 100
  )

#most frequent verbs and their percentage frequency
NS_past_list1a_pret <- NS_past_list1a %>%
  select(verb, per_of_pret, freq_lemma_pret)%>%
  arrange(desc(per_of_pret))
#imp
NS_past_list1a_imp <- NS_past_list1a %>%
  select(verb, per_of_imp, freq_lemma_imp)%>%
  arrange(desc(per_of_imp))

#have to split up so factor works (for graphing)
NS_past_list1a_pret_order <-NS_past_list1a_pret %>%
  slice_head(n=27)%>% #27 most freq
  mutate(verbs = factor(verb, levels = NS_past_list1a_pret$verb))

#write_csv(NS_past_list1a_pret_order, "Figure3_NSpret_RiCL_042822.csv")


#### Graph ####
#####IN PAPER#####
#graph: width 400
ggplot(NS_past_list1a_pret_order, 
       aes(x = verbs, y = freq_lemma_pret))+
  geom_bar(stat = "identity", position = "dodge", fill="steelblue")+
  labs(x = 'Verb', y= 'Frequency of Verb in Preterit')+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,600)

#imp
#arrange by frequency in imp
NS_past_list1a_imp_order <- NS_past_list1a_imp%>%
  arrange(desc(freq_lemma_imp))
#have to split up so factor works
NS_past_list1a_imp_order <-NS_past_list1a_imp_order %>%
  slice_head(n=27)%>% #27 most freq
  mutate(verbs = factor(verb, levels = NS_past_list1a_imp$verb))

#write_csv(NS_past_list1a_imp_order, "Figure3_NSimp_RiCL_042822.csv")

#graph
ggplot(NS_past_list1a_imp_order, 
       aes(x = verbs, y = freq_lemma_imp))+
  geom_bar(stat = "identity", position = "dodge", fill="steelblue")+
  labs(x = 'Verb', y= 'Frequency of Verb in Imperfect')+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,600)

## 5. Token Frequency ####
#Issue with prior analysis for preterit
#it had been duplicating data for fui, fuimos, etc
#because corresponds to ser & ir as lemmas
#only had done for NS, not L2 group
#results less informative bc of this
#fixing now, 9-7-21
#will need to update paper with this change


### L2 ####

#this is the basis for analysis for preterit
##View(combined_pret1)

#this is the basis for analysis for imperfect
##View(combined_imp1)

#count aggregate frequency: preterit
combined_pret2 <- combined_pret1 %>%
  group_by(verb)%>%
  dplyr::summarize(L2_freq = n()) #freq of pret by token in L2 production

#count aggregate frequency: imperfect
combined_imp2 <- combined_imp1 %>%
  group_by(verb)%>%
  dplyr::summarize(L2_freq = n()) #freq of imp by token in L2 production

##View(combined_imp2)

### NS ####

#this is the basis for analysis for preterit
##View(combined_pret1_NS)

#this is the basis for analysis for imperfect
##View(combined_imp1_NS)

#count aggregate frequency: preterit
combined_pret2_NS <- combined_pret1_NS %>%
  group_by(verb)%>%
  dplyr::summarize(NS_freq = n()) #freq of pret by token in NS production

##View(combined_pret2_NS)

#count aggregate frequency: imperfect
combined_imp2_NS <- combined_imp1_NS %>%
  group_by(verb)%>%
  dplyr::summarize(NS_freq = n()) #freq of imp by token in NS production

##View(combined_imp2_NS)

### *combined ####
#preterit
pret_token_freq <- inner_join(
  combined_pret2, combined_pret2_NS, by = 'verb'
)

#View(pret_token_freq)

#imperfect
imp_token_freq <- inner_join(
  combined_imp2, combined_imp2_NS, by = 'verb'
)

# View(imp_token_freq)

### *log-transformed ####
#preterit
pret_token_freq_log <- pret_token_freq %>%
  mutate(
    L2_freq_log = log10(L2_freq+1),
    NS_freq_log = log10(NS_freq+1)
  )
#view(pret_token_freq_log)

#imperfect
imp_token_freq_log <- imp_token_freq %>%
  mutate(
    L2_freq_log = log10(L2_freq+1),
    NS_freq_log = log10(NS_freq+1)
  )
#view(imp_token_freq_log)

#### Linear Regression ####
#new linear regression: this is on a *type* basis, not lemma
#based on total frequencies

#preterit
basic_lm_pret_NS_all_new <- lm(L2_freq ~ NS_freq,
                               data = pret_token_freq)
summary(basic_lm_pret_NS_all_new)
# Multiple R-squared:  0.7785,	Adjusted R-squared:  0.7777 
# F-statistic:  1047 on 1 and 298 DF,  p-value: < 2.2e-16
plot(basic_lm_pret_NS_all_new)

# Pearson's r = 0.88
cor(pret_token_freq$L2_freq, pret_token_freq$NS_freq,
         method = 'pearson')

##linear regression: imperfect
basic_lm_imp_NS_all_new <- lm(L2_freq ~ NS_freq,
                              data = imp_token_freq) 
#same as with data = LR_imp_all_new, because no change
summary(basic_lm_imp_NS_all_new)
#Multiple R-squared:  0.6981,	Adjusted R-squared:  0.6957 
#F-statistic: 293.6 on 1 and 127 DF,  p-value: < 2.2e-16
###View residuals
plot(basic_lm_imp_NS_all_new)

##### *log-transformed ####
#preterit
lm_pret_log <- lm(L2_freq_log ~ NS_freq_log,
                               data = pret_token_freq_log)
summary(lm_pret_log)

#imperfect
lm_imp_log <- lm(L2_freq_log ~ NS_freq_log,
                  data = imp_token_freq_log)
summary(lm_imp_log)

#write_csv(pret_token_freq, "Figure5_pret_RiCL_042822.csv")

#### Graph ####
#graphs
LR_pret_all_new <- ggplot(pret_token_freq, aes(x =NS_freq, y = L2_freq))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, color = 'gray36')+
  labs(x = "Frequency in L1 Corpus", y = "Frequency in L2 Corpus")+
  ylim(0,500)+
  xlim(0, 500)
LR_pret_all_new
#aggregated for all proficiency levels: much clearer trend

#write_csv(imp_token_freq, "Figure5_imp_RiCL_042822.csv")

#imperfect
LR_imp_all_new <- ggplot(imp_token_freq, aes(x =NS_freq, y = L2_freq))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, color = 'gray36')+
  labs(x = "Frequency in L1 Corpus", y = "Frequency in L2 Corpus")+
  ylim(0,500)+
  xlim(0,500)
LR_imp_all_new

##### *log-transformed ####
#preterit
ggplot(pret_token_freq_log, aes(x =NS_freq_log, y = L2_freq_log))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, color = 'gray36')+
  labs(x = "Frequency in L1 Corpus", y = "Frequency in L2 Corpus")
# +
#   ylim(0,500)+
#   xlim(0, 500)

#imperfect
ggplot(imp_token_freq_log, aes(x =NS_freq_log, y = L2_freq_log))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, color = 'gray36')+
  labs(x = "Frequency in L1 Corpus", y = "Frequency in L2 Corpus")

## 6. Contingent Frequency ####
### L2 ####

#this is the basis for analysis for preterit
##View(combined_pret1_lemmas)

#this is the basis for analysis for imperfect
##View(combined_imp1_NS_lemmas)

#I need to create a table with 1) pret/imp 2) lemma
#preterit
cf_df1 <- combined_pret1_lemmas %>%
  select(verb)%>%
  mutate(TA = 'preterit')
##View(cf_df1)

#imperfect
cf_df2 <- combined_imp1_lemmas %>%
  select(verb)%>%
  mutate(TA = 'imperfect')
##View(cf_df2)

#combine
cf_df3 <- rbind(cf_df1, cf_df2)%>%
  select(TA, verb)%>%
  filter(verb != 'aqui' & verb != 'estabamos')%>%
  write_delim('cf_df7.tsv', delim = "\t")
##View(cf_df3)
#number of verbs: 4818

#write to txt to get over weird processing of UTF8
# write_delim(data1, 'cf_df8_L2.txt')

#### DCA ####
##run the program: Coll.analysis
#input: cf_df7.tsv
#output:CEDEL2_DCA_cleaned.txt
#source("http://www.stgries.info/teaching/groningen/coll.analysis.r")

#checked 9-7-21, this worked appropriately
#used -log10 (Fisher-Yates exact, one-tailed) (= default) option

#### Table ####
#results: L2
DCA_CEDEL2 <- read_csv('DCA_CEDEL2_updated.csv')%>%
  select(words, obs.freq.1, obs.freq.2, pref.occur, coll.strength)%>%
  dplyr::rename(
    freq_imp = obs.freq.1,
    freq_pret = obs.freq.2
  )

#view(DCA_CEDEL2)

#preterit top 10
Col_CEDEL2_pret <- DCA_CEDEL2 %>%
  filter(pref.occur == 'preterit')%>%
  arrange(desc(coll.strength))

#imperfect top 10
Col_CEDEL2_imp <- DCA_CEDEL2 %>%
  filter(pref.occur == 'imperfect')%>%
  arrange(desc(coll.strength))


### NS ####

#this is the basis for analysis for preterit
##View(combined_pret1_NS_lemmas)

#this is the basis for analysis for imperfect
##View(combined_imp1_NS_lemmas)

#I need to create a table with 1) pret/imp 2) lemma
#preterit
cf_df1_NS <- combined_pret1_NS_lemmas %>%
  select(verb)%>%
  mutate(TA = 'preterit')
#View(cf_df1_NS)
#4327 tokens

#imperfect
cf_df2_NS <- combined_imp1_NS_lemmas %>%
  select(verb)%>%
  mutate(TA = 'imperfect')
#View(cf_df2_NS)
#2668 tokens

#combine
cf_df3_NS <- rbind(cf_df1_NS, cf_df2_NS)%>%
  select(TA, verb)%>% #need to reverse order: TA first, then verb
  filter(verb != 'aqui' & verb != 'estabamos')%>%
  write_delim('cf_df3_NS.tsv', delim = "\t")

##View(cf_df3_NS)
#6977 without aqui and estabamos
  
#this will let me copy and paste into excel
#write_delim(data1, 'cf_df8_NS.txt')

#### DCA ####
##run the program: Coll.analysis
#input:cf_df3_NS.tsv
#output: CEDEL2_NS_DCA_cleaned.txt
#source("http://www.stgries.info/teaching/groningen/coll.analysis.r")

#### Table ####
#results: NS
DCA_CEDEL2_NS <- read_csv('DCA_CEDEL2_NS_updated.csv')%>%
  select(words, obs.freq.1, obs.freq.2, pref.occur, coll.strength)%>%
  dplyr::rename(
    freq_imp = obs.freq.1,
    freq_pret = obs.freq.2
  )

#view(DCA_CEDEL2_NS)

#preterit top 10
Col_CEDEL2_pret_NS <- DCA_CEDEL2_NS %>%
  filter(pref.occur == 'preterit')%>%
  arrange(desc(coll.strength))
###View(Col_CEDEL2_pret_NS)
#imperfect top 10
Col_CEDEL2_imp_NS <- DCA_CEDEL2_NS %>%
  filter(pref.occur == 'imperfect')%>%
  arrange(desc(coll.strength))

### L2~NS Correlation ####
#create one large DF
#NS
DCA_CEDEL2_NS_1 <- DCA_CEDEL2_NS %>%
  select(words, pref.occur, coll.strength)%>%
  dplyr::rename(
    pref_occur_NS = pref.occur,
    coll_strength_NS = coll.strength
  )
#view(DCA_CEDEL2_NS_1)

#same for L2
DCA_CEDEL2_1 <- DCA_CEDEL2 %>%
  select(words, pref.occur, coll.strength)%>%
  dplyr::rename(
    pref_occur_L2 = pref.occur,
    coll_strength_L2 = coll.strength
  )

#combine the two via left join
DCA_CEDEL2_all <- DCA_CEDEL2_1%>%
  left_join(DCA_CEDEL2_NS_1, by = 'words')%>%
  drop_na()%>% #included 230 verb types that were the same
  mutate(
    coll_strength_NS = as.double(coll_strength_NS),
    coll_strength_L2 = as.double(coll_strength_L2)
    )
  
view(DCA_CEDEL2_all)

# #export as csv, then read back in
# write_csv(DCA_CEDEL2_all, 'DCA_CEDEL2_all_031622.csv')
# 
# #read in
# DCA_CEDEL2_all1 <- read_csv('DCA_CEDEL2_all_031622.csv')
# view(DCA_CEDEL2_all1)

#linear regression
b <- lm(coll_strength_L2 ~ coll_strength_NS, DCA_CEDEL2_all)
summary(b)

#plot this
DCA_CEDEL2_all_graph <- ggplot(DCA_CEDEL2_all, 
                               aes(x =coll_strength_NS,
                                   y = coll_strength_L2))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE, color = 'gray36')+
  labs(x = "Collostructional Strength in L1 Corpus", y = "Collostructional Strength in L2 Corpus")
# +
  # ylim(0,500)+
  # xlim(0, 500)
DCA_CEDEL2_all_graph

## 7. Prototypicality ####
#27 verbs will use
#espal: frequency of 27 most frequent verbs of each LAH group
ESPAL_27_lemmas_freq <- read_csv('ESPAL_27_lemmas_freq.csv')%>%
  dplyr::rename(lexical_aspect = `lexical aspect`)

#just verbs & LA
ESPAL_27_lemmas_verbs <- ESPAL_27_lemmas_freq %>%
  select(verb, lexical_aspect)

### L2 ####

#get counts by verb and by tense-aspect (TA) in L2 corpus
prot_df_L2 <- cf_df3 %>%
  group_by(verb, TA)%>%
  dplyr::summarize(num_times_produced = n())%>%
  pivot_wider(names_from = TA, values_from = num_times_produced)

#View(prot_df_L2)

#turn NAs to 0s
prot_df_L2[is.na(prot_df_L2)] <- 0

#now get summary counts & filter so it's only the verbs in question
prot_df_L2 <- prot_df_L2 %>%
  rowwise()%>%
  mutate(
    total_past = sum(imperfect, preterit),
    prop_imp = imperfect / total_past,
    prop_pret = preterit / total_past
  )%>%
  arrange(desc(total_past))

#filter for only 27 verb subset 
prot_df_L2_27 <- prot_df_L2 %>%
  left_join(ESPAL_27_lemmas_verbs, by = 'verb')%>%
  arrange(desc(total_past))%>%
  drop_na()

#View(prot_df_L2_27)

#### Graph ####
#graphs for L2
#pret
ggplot(prot_df_L2_27, aes(x = lexical_aspect, y = prop_pret))+
  geom_boxplot()+
  labs(x = "Lexical Aspect", y = "Proportion of Past Forms in Preterit")

#imp
ggplot(prot_df_L2_27, aes(x = lexical_aspect, y = prop_imp))+
  geom_boxplot()+
  labs(x = "Lexical Aspect", y = "Proportion of Past Forms in Imperfect")


### NS ####

#get counts by verb and by TA in L2 corpus
prot_df_NS <- cf_df3_NS %>%
  group_by(verb, TA)%>%
  dplyr::summarize(num_times_produced = n())%>%
  pivot_wider(names_from = TA, values_from = num_times_produced)

#turn NAs to 0s
prot_df_NS[is.na(prot_df_NS)] <- 0

#now get summary counts & filter so it's only the verbs in question
prot_df_NS <- prot_df_NS %>%
  rowwise()%>%
  mutate(
    total_past = sum(imperfect, preterit),
    prop_imp = imperfect / total_past,
    prop_pret = preterit / total_past
  )%>%
  arrange(desc(total_past))

#only subset of 27 verbs
prot_df_NS_27 <- prot_df_NS %>%
  left_join(ESPAL_27_lemmas_verbs, by = 'verb')%>%
  arrange(desc(total_past))%>%
  drop_na()

View(prot_df_NS_27)

#### Graph ####
#graphs for L1 Spanish
#pret
ggplot(prot_df_NS_27, aes(x = lexical_aspect, y = prop_pret))+
  geom_boxplot()+
  labs(x = "Lexical Aspect", y = "Proportion of Past Forms in Preterit")

#imp
ggplot(prot_df_NS_27, aes(x = lexical_aspect, y = prop_imp))+
  geom_boxplot()+
  labs(x = "Lexical Aspect", y = "Proportion of Past Forms in Imperfect")

#thanks for reading!
