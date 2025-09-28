# ======================
# Section: 패키지 로드
# ======================
library(pacman)
pacman::p_load(
  tidyverse, haven, readxl, psych, car, gt,
  interactions, jtools, modelsummary, gtsummary, emmeans, broom, webshot2, Hmisc
)


# ======================
# Section: 데이터 불러오기
# ======================
file_path <- "~/2025_1학기/SNA/SNA_KSHAP/DATA"
setwd(file_path)


df1 <- read_excel("KSHAP_W1_KOSSDA.xlsx")



# ======================
# Section: Wave 1 데이터 처리
# ======================
mmse_vars <- c(
  "a8501", "a8502", "a8503", "a8504", "a8505", "a8506", "a8507", "a8508", "a8509", "a8510",
  "a861", "a862", "a863", "a871", "a872", "a873", "a874", "a875", "a881", "a882",
  "a883", "a891", "a892", "a90", "a911", "a912", "a913", "a92", "a93", "a94"
)

wave1 <- df1 %>%
  mutate(
    #Num of family/relatives - 중앙값으로 계산
    fam_rel_num = case_when(
      a29 == 1 ~ 0,
      a29 == 2 ~ 1,
      a29 == 3 ~ 2.5,
      a29 == 4 ~ 6.5,
      a29 == 5 ~ 15,
      a29 == 6 ~ 21,
      a29 == 9 ~ NA_real_
    ),
    # Num of friends
    friend_num = case_when(
      a34 == 1 ~ 0,
      a34 == 2 ~ 1,
      a34 == 3 ~ 2.5,
      a34 == 4 ~ 6.5,
      a34 == 5 ~ 15,
      a34 == 6 ~ 21,
      a34 == 9 ~ NA_real_
    ),
    
    spouse_num = ifelse(a062 %in% c(1, 2), 1, 0),
    sum_social_ties = friend_num + fam_rel_num + spouse_num,
    spouse_label = factor(spouse_num, levels = c(0, 1), labels = c("No Spouse", "Has Spouse")),
    
    # NA 처리
    across(c(a21, a22, a23, a24, a25, a26, a27, a28, a30, a31, a32, a33), ~na_if(., 9)),
    
    # 정서적 지지 및 부담 점수 계산
    spouse_support = rowMeans(cbind(a21, a22), na.rm = FALSE),
    family_support = rowMeans(cbind(a25, a26), na.rm = FALSE),
    friend_support = rowMeans(cbind(a30, a31), na.rm = FALSE),
    emotional_support = rowMeans(cbind(spouse_support, friend_support, family_support), na.rm = TRUE),
    
    spouse_burden = rowMeans(cbind(a23, a24), na.rm = FALSE),
    family_burden = rowMeans(cbind(a27, a28), na.rm = FALSE),
    friend_burden = rowMeans(cbind(a32, a33), na.rm = FALSE),
    emotional_burden = rowMeans(cbind(spouse_burden, friend_burden, family_burden), na.rm = TRUE),
    
    # 통제 변수 처리
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    edu = case_when(
      a02 == 1 ~ "No formal education",
      a02 %in% c(2, 3) ~ "Elementary",
      a02 == 4 ~ "Middle School",
      a02 == 5 ~ "High School",
      a02 == 6 ~ "College or higher",
      a02 >= 7 ~ NA_character_
    ) %>% factor(levels = c("No formal education", "Elementary", "Middle School", "High School", "College or higher")),
    income = case_when(
      b1091 == 1 ~ "Low",
      b1091 == 2 ~ "Mid-low",
      b1091 == 3 ~ "Mid",
      b1091 %in% c(4, 5) ~ "High"
    ) %>% factor(levels = c("Low", "Mid-low", "Mid", "High")),
    
    married = ifelse(a062 %in% c(1, 2), 1, 0),
    smoker = factor(ifelse(a47 %in% c(1, 4), 1, 0), levels = c(0, 1), labels = c("Non-smoker", "Smoker")),
    self_health = 6 - b100,
    diabetes = ifelse(dm_status == 2, 1, 0),
    iadl_imp = factor(ifelse(b104 >= 3, 1, 0), levels = c(0, 1), labels = c("No Impairment", "Impairment")),
    
    # CES-D (우울증 점수)
    across(c(a8405, a8410, a8415), ~5 - .),
    across(a8401:a8420, ~. - 1),
    cesd = rowSums(across(a8401:a8420), na.rm = TRUE),
    
    # MMSE (인지기능 점수)
    across(all_of(mmse_vars), ~ifelse(. == 9, 0, .)),
    mmse = rowSums(across(all_of(mmse_vars)), na.rm = TRUE),
    mmse_NA_count = rowSums(is.na(across(all_of(mmse_vars)))),
    
    mmse_group = case_when(
      mmse >= 21 ~ "Normal Cognition",
      mmse < 21 ~ "Suspected Cognitive Impairment"
    )
  ) %>%
  mutate(
    depressed = factor(ifelse(cesd >= 16, 1, 0), levels = c(0, 1), labels = c("non-depressed", "depressed"))
  )

summary(wave1$mmse)

# ======================
# Section: Descriptive Statistics
# ======================

desc_vars <- wave1 %>%
  select(mmse, cesd, sum_social_ties,fam_rel_num,friend_num, emotional_support, emotional_burden,
         spouse_support, spouse_burden, friend_support, friend_burden, family_support,family_burden,
         sex, edu, income, smoker,  iadl_imp,mmse_group)


# 변수 구분
continuous_vars <- c("mmse", "cesd",  "sum_social_ties", "friend_num" , "fam_rel_num" ,"emotional_support", "emotional_burden",
                     "spouse_support", "spouse_burden", "friend_support", "friend_burden", "family_support","family_burden")

categorical_vars <- c("sex", "edu", "income", "smoker", "iadl_imp")


# Table 생성
desc_table_by_group <- desc_vars %>% 
  tbl_summary(
    by = mmse_group,
    type = list(all_of(continuous_vars) ~ "continuous",
                all_of(categorical_vars) ~ "categorical"),
    statistic = list(all_continuous() ~ "{mean} ± {sd}",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no",
    label = list(
      mmse ~ "MMSE (Cognitive Function)",
      cesd ~ "CES-D (Depressive Symptoms)",
      sum_social_ties ~ "Sum of Social Ties",
      friend_num ~ "No. of Friends",
      fam_rel_num ~ "No. of Family & Relatives",
      emotional_support ~ "Emotional Support",
      emotional_burden ~ "Emotional Burden",
      spouse_support ~ "Spouse Support",
      spouse_burden ~ "Spouse Burden",
      friend_support ~ "Friend Support",
      friend_burden ~ "Friend Burden",
      family_support~ "Family/Relative Support",
      family_burden ~ "Family/Relative Support",
      sex ~ "Gender",
      edu ~ "Education",
      income ~ "Income",
      smoker ~ "Smoking",
      iadl_imp ~ "IADL Impairment"
    )
  ) %>%
  add_p(
   test.args = all_categorical() ~ list(simulate.p.value = TRUE)
  ) %>%
  add_overall() %>%
  bold_labels()

# gt 테이블 변환 후 제목 넣기
desc_table_by_group_gt <- desc_table_by_group %>%
  as_gt() %>%
  tab_header(title = md("**Table 1. Characteristics of Study Participants by MMSE Group (with Overall)**"))

# 저장
gtsave(desc_table_by_group_gt, "desc_table_by_group.png", vwidth = 2400, vheight = 3200)




# ======================
# Section: Correlation
# ======================
# 패키지
library(Hmisc)
library(gt)
library(tibble)
wave1$sum
# 변수 라벨
var_labels <- c(
  mmse = "MMSE",
  cesd = "CES-D",
  emotional_support = "Emotional Support",
  emotional_burden = "Emotional Burden",
  spouse_support = "Spouse Support",
  spouse_burden = "Spouse Burden",
  friend_support = "Friend Support",
  friend_burden = "Friend Burden",
  family_support = "Family Support",
  family_burden = "Family Burden",
  fam_rel_num =  "No.Family & Relatives",
  friend_num = "No. Friends"
)

cor_vars <- wave1 %>%
  select(mmse, cesd, 
         emotional_support, emotional_burden,
         spouse_support, spouse_burden,
         friend_support, friend_burden,
         family_support,family_burden,
         friend_num,fam_rel_num) %>%
  drop_na()

# Correlation + p-value 계산
cor_results <- rcorr(as.matrix(cor_vars))

# r 값과 p 값 추출
cor_r <- cor_results$r
cor_p <- cor_results$P

# 별표 표시 포함한 matrix 만들기
sig_r_stars <- ifelse(cor_p < 0.05, paste0(round(cor_r, 2), " *"), round(cor_r, 2))
diag(sig_r_stars) <- ""  # 자기자신 공백

# row, col 이름 변경
rownames(sig_r_stars) <- var_labels[rownames(sig_r_stars)]
colnames(sig_r_stars) <- var_labels[colnames(sig_r_stars)]

# 데이터 프레임 변환
sig_r_stars_df <- as.data.frame(sig_r_stars)
sig_r_stars_df <- tibble::rownames_to_column(sig_r_stars_df, "Variable")

# gt 테이블 변환
cor_gt <- sig_r_stars_df %>%
  gt() %>%
  tab_header(title = "Correlation Matrix ( * p < 0.05 )") %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(4)
  )

# 저장
gtsave(cor_gt, "correlation_with_stars.png", vwidth = 2400, vheight =1600)



# ======================
# Section: Regression
# ======================

#model 1: Only covariates
#model 2: added X variables
#model 3: added interaction terms

# 기본 모델용 (Emotional 포함)
complete_w1 <- wave1 %>%
  select(mmse, spouse_label, sex, edu, income, smoker, iadl_imp,
         emotional_support, emotional_burden, cesd) %>%
  drop_na()

# 친구 관계
complete_w1_fri <- wave1 %>%
  select(mmse, spouse_label, sex, edu, income, smoker, iadl_imp,
         friend_num, friend_support, friend_burden, cesd) %>%
  drop_na()

# 배우자 관계
complete_w1_spo <- wave1 %>%
  select(mmse, sex, edu, income, smoker,  iadl_imp,
         spouse_support, spouse_burden, cesd) %>%
  drop_na()

# 가족 관계
complete_w1_fam <- wave1 %>%
  select(mmse, sex, edu, income, smoker,  iadl_imp,
         fam_rel_num, family_support, family_burden, cesd, spouse_label) %>%
  drop_na()

# 정서적 관계 전체
complete_w1_emo <- wave1 %>%
  select(mmse, sex, edu, income, smoker,  iadl_imp, spouse_label,
         emotional_support, emotional_burden, sum_social_ties, cesd) %>%
  drop_na()


# M1: Control only
m1_control <- lm(mmse ~ spouse_label + sex + edu + income + smoker + iadl_imp,
                 data = complete_w1)
summary(m1_control)
vif(m1_control)

# M2-1: Spousal support / burden
m2_1_spouse <- lm(mmse ~ sex + edu + income + smoker + iadl_imp +
                    spouse_support + spouse_burden + cesd,
                  data = complete_w1_spo)
summary(m2_1_spouse)
vif(m2_1_spouse)

# M2-2: Friend support / burden + friend number
m2_w1_friend <- lm(mmse ~ spouse_label + sex + edu + income + smoker + iadl_imp +
                     friend_num + friend_support + friend_burden + cesd,
                   data = complete_w1_fri)
summary(m2_w1_friend)
vif(m2_w1_friend)

# M2-3: Family support / burden + family number
m2_w1_family <- lm(mmse ~ spouse_label + sex + edu + income + smoker + iadl_imp +
                     fam_rel_num + family_support + family_burden + cesd,
                   data = complete_w1_fam)
summary(m2_w1_family)
vif(m2_w1_family)

# M2-4: Emotional support / burden
m2_4_emotional <- lm(mmse ~ spouse_label + sex + edu + income + smoker + iadl_imp +
                       emotional_support + emotional_burden + cesd,
                     data = complete_w1_emo)
summary(m2_4_emotional)
vif(m2_4_emotional)

# ------------------------------
# M3-1 (Friend Num × Friend Support)는 VIF 문제로 제외하기로 결정됨
# ------------------------------

# Centering for Friend Support × CES-D
complete_w1_fri <- complete_w1_fri %>%
  mutate(
    friend_support_c = friend_support - mean(friend_support, na.rm = TRUE),
    cesd_c = cesd - mean(cesd, na.rm = TRUE)
  )

# M3-3: Friend Support × CES-D
m3_3_support_cesd_c <- lm(mmse ~ sex + edu + income + smoker + iadl_imp +
                            spouse_label + friend_support_c + friend_support_c:cesd_c ,
                          data = complete_w1_fri)
summary(m3_3_support_cesd_c)
vif(m3_3_support_cesd_c)


m3_3_support_cesd <- lm(mmse ~ sex + edu + income + smoker + iadl_imp +
                            spouse_label + friend_support + friend_support:cesd,
                          data = complete_w1_fri)
summary(m3_3_support_cesd)
vif(m3_3_support_cesd)

# Centering for Family Num × Family Support
complete_w1_fam <- complete_w1_fam %>%
  mutate(
    fam_rel_num_c = fam_rel_num - mean(fam_rel_num, na.rm = TRUE),
    family_support_c = family_support - mean(family_support, na.rm = TRUE)
  )

# M3-2: Family Num × Family Support (centered)
m3_2_fam_num_support_c <- lm(mmse ~ spouse_label + sex + edu + income + smoker + iadl_imp +
                               fam_rel_num_c + family_support_c + family_burden + cesd +
                               fam_rel_num_c * family_support_c,
                             data = complete_w1_fam)
summary(m3_2_fam_num_support_c)
vif(m3_2_fam_num_support_c)






# Section: 회귀분석 이미지 출력
library(broom)
library(dplyr)
library(tidyr)
library(purrr)
library(gt)
library(webshot2)
# 변수 이름 매핑
name_map <- c(
  "(Intercept)" = "Intercept",
  
  # Demographics / Control
  "spouse_labelHas Spouse" = "Has Spouse",
  "sexFemale" = "Gender: Female",
  
  "eduElementary" = "Education: Elementary School",
  "eduMiddle School" = "Education: Middle School",
  "eduHigh School" = "Education: High School",
  "eduCollege or higher" = "Education: College or Higher",
  
  "incomeMid-low" = "Income: Mid-Low",
  "incomeMid" = "Income: Middle",
  "incomeHigh" = "Income: High",
  
  "smokerSmoker" = "Smoker",
  "iadl_impImpairment" = "IADL Impairment",
  
  # Social support (main variables)
  "spouse_support" = "Spousal Support",
  "spouse_burden" = "Spousal Burden",
  
  "friend_support" = "Friend Support",
  "friend_burden" = "Friend Burden",
  
  "family_support" = "Family Support",
  "family_burden" = "Family Burden",
  
  "emotional_support" = "Emotional Support",
  "emotional_burden" = "Emotional Burden",
  

  
  # Number of ties
  "friend_num" = "Number of Friends",
  "fam_rel_num" = "Number of Family/Relatives",
  
  "sum_social_ties" = "Sum of Social Ties",
  
  # centered terms!
  "fam_rel_num_c" = "Number of Family/Relatives (centered)",
  "family_support_c" = "Family Support (centered)",
  "fam_rel_num_c:family_support_c" = "Family Num × Family Support",
  
  "friend_support_c" = "Friend Support (centered)",
  "friend_support_c:cesd_c" = "Friend Support × CES-D",
  
  # Depression
  "cesd" = "Depressive Symptoms (CES-D)",
  
  # Interaction terms
  "fam_rel_num:family_support" = "Family Num × Family Support",
  "friend_support:cesd" = "Friend Support × CES-D"
)

# ★ 별표 함수

add_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ ""
  )
}

# ★ extract_summary 함수
extract_summary <- function(model, model_name) {
  tidy(model) %>%
    mutate(
      Variable = name_map[term],
      `Coefficient (SE)` = paste0(
        sprintf("%.3f", estimate), add_stars(p.value),
        " (", sprintf("%.3f", std.error), ")"
      ),
      `p-value` = sprintf("%.3f", p.value)
    ) %>%
    filter(!is.na(Variable)) %>%    # name_map 에 없는 항목 제거 (깨짐 방지)
    select(Variable, `Coefficient (SE)`, `p-value`) %>%
    rename_with(~ paste(model_name, c("Coef. (SE)", "p-value")), -Variable)
}



models_table1 <- list(
  "Model 1 (Control only)" = m1_control,
  "Model 2-1 (Spouse)" = m2_1_spouse,
  "Model 2-2 (Friend)" = m2_w1_friend,
  "Model 2-3 (Family)" = m2_w1_family
)

summary_list1 <- map2(models_table1, names(models_table1), extract_summary)
final_table1 <- reduce(summary_list1, full_join, by = "Variable") %>%
  arrange(match(Variable, name_map))

gt_table1 <- final_table1 %>%
  gt() %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  tab_header(title = md("**Table 1. Impact of Spousal, Friend, and Family Support on Cognitive Function**")) %>%
  tab_options(table.font.size = px(14), data_row.padding = px(4))

gtsave(gt_table1, "table1_spouse_friend_family.png", vwidth = 1300, vheight = 1600)





models_table2 <- list(
  "Model 2-4 (Emotional Support / Burden)" = m2_4_emotional
)

summary_list2 <- map2(models_table2, names(models_table2), extract_summary)
final_table2 <- reduce(summary_list2, full_join, by = "Variable") %>%
  arrange(match(Variable, name_map))


gt_table2 <- final_table2 %>%
  gt() %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  tab_header(title = md("**Table 2. Emotional Support and Burden on Cognitive Function**")) %>%
  tab_options(table.font.size = px(14), data_row.padding = px(4))

gtsave(gt_table2, "table2_emotional_support_burden.png", vwidth = 1000, vheight = 1400)




models_table3 <- list(
  "Model 3-1 (Family Num × Family Support)" = m3_2_fam_num_support_c,
  "Model 3-2 (Friend Support × CES-D)" = m3_3_support_cesd_c
)

summary_list3 <- map2(models_table3, names(models_table3), extract_summary)
final_table3 <- reduce(summary_list3, full_join, by = "Variable") %>%
  arrange(match(Variable, name_map))

gt_table3 <- final_table3 %>%
  gt() %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  tab_header(title = md("**Table 3. Interaction Effects on Cognitive Function**")) %>%
  tab_options(table.font.size = px(14), data_row.padding = px(4))

gtsave(gt_table3, "table3_interaction_effects.png", vwidth = 1400, vheight = 1800)





# Section: Prediction - Graph for Spouse Support
# 평균값 계산 (연속형 변수)
means <- complete_w1_spo %>%
  summarise(across(c(  cesd), mean, na.rm = TRUE))

# baseline category 설정
ref_grid_spouse <- ref_grid(m2_1_spouse, at = list(
  spouse_support = c(1,2.0,3.0 ,4.0), 
  spouse_burden = 2,
  sex = "Female",
  edu = "High School",
  income = "Mid",
  smoker = "Non-smoker",
  iadl_imp = "No Impairment",
  cesd = means$cesd
))


preds_support <- emmeans(ref_grid_spouse, ~ spouse_support|cesd, type = "response")
# Plot 저장용 객체로 할당
p_spouse_support <- preds_support %>% 
  as_tibble() %>% 
  ggplot(aes(x = spouse_support, y = emmean)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "lightblue", alpha = 0.3) +
  labs(x = "Spouse Support", 
       y = "Predicted MMSE Score", 
       title = "Effect of Spouse Support on Cognitive Function (MMSE)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# 파일로 저장
ggsave("figure_spouse_support_mmse.png", plot = p_spouse_support, width = 8, height = 6, dpi = 300)




# baseline category 설정
ref_grid_spouse_burden <- ref_grid(m2_1_spouse, at = list(
  spouse_burden = c(1, 2.0, 3.0, 4.0), 
  spouse_support = 2,
  sex = "Female",
  edu = "High School",
  income = "Mid",
  smoker = "Non-smoker",
  iadl_imp = "No Impairment",
  cesd = means$cesd
))

# 예측 및 시각화
preds_burden <- emmeans(ref_grid_spouse_burden, ~ spouse_burden | cesd, type = "response")

p_spouse_burden <- preds_burden %>%
  as_tibble() %>%
  ggplot(aes(x = spouse_burden, y = emmean)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "lightblue", alpha = 0.3) +
  labs(x = "Spouse Burden", 
       y = "Predicted MMSE Score", 
       title = "Effect of Spouse Burden on Cognitive Function (MMSE)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
ggsave("figure_spouse_burden_mmse.png", plot = p_spouse_burden, width = 8, height = 6, dpi = 300)



# Section: Graph for Effect of Friend Support on Cognitive Function (MMSE)
summary(m2_w1_friend)

# 평균값 계산 (연속형 변수)
means <- complete_w1_fri %>%
  summarise(across(c( cesd), mean, na.rm = TRUE))

# baseline category 설정
ref_grid_friend <- ref_grid(m2_w1_friend, at = list(
  friend_support = seq(1, 4, by = 0.1),  # 1~5점 스케일로 가정
  friend_burden = mean(complete_w1_fri$friend_burden, na.rm = TRUE),
  sex = "Female",
  edu = "Elementary",
  income = "Low",
  smoker = "Non-smoker",
  iadl_imp = "No Impairment",
  cesd = means$cesd
))


preds_support <- emmeans(ref_grid_friend, ~ friend_support, type = "response")

preds_support %>%
  as_tibble() %>%
  ggplot(aes(x = friend_support, y = emmean)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "lightblue", alpha = 0.3) +
  labs(x = "Friend Support",
       y = "Predicted MMSE Score",
       title = "Effect of Friend Support on Cognitive Function (MMSE)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )



# Section: Graph for Effect of Friend Burden on Cognitive Function (MMSE)
means <- complete_w1_fri %>%
  summarise(across(c( , cesd), mean, na.rm = TRUE))

# baseline category 설정
ref_grid_friend <- ref_grid(m2_w1_friend, at = list(
  friend_support =mean(complete_w1_fri$friend_burden, na.rm = TRUE) , 
  friend_burden = seq(1, 4, by = 0.1),
  sex = "Female",
  edu = "Elementary",
  income = "Low",
  smoker = "Non-smoker",
  iadl_imp = "No Impairment",
  cesd = means$cesd
))


preds_burden <- emmeans(ref_grid_friend, ~ friend_burden, type = "response")

preds_burden %>%
  as_tibble() %>%
  ggplot(aes(x = friend_burden, y = emmean)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "lightblue", alpha = 0.3) +
  labs(x = "Friend Burden",
       y = "Predicted MMSE Score",
       title = "Effect of Friend Burden on Cognitive Function (MMSE)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )



# ======================
# Section: Figure 4 - Family Num × Family Support Interaction
# ======================
# centering 된 값 계산
mean_fam_rel_num <- mean(complete_w1_fam$fam_rel_num, na.rm = TRUE)
mean_family_support <- mean(complete_w1_fam$family_support, na.rm = TRUE)
mean_family_burden <- mean(complete_w1_fam$family_burden, na.rm = TRUE)
mean_cesd <- mean(complete_w1_fam$cesd, na.rm = TRUE)

# ref_grid 정확히!
ref_grid_family <- ref_grid(m3_2_fam_num_support_c, at = list(
  fam_rel_num_c = c(0, 1, 2.5, 6.5, 15, 21) - mean_fam_rel_num,
  family_support_c = seq(1, 4, by = 0.5) - mean_family_support,
  family_burden = mean_family_burden,
  sex = "Female",
  edu = "Elementary",
  income = "Low",
  smoker = "Non-smoker",
  iadl_imp = "No Impairment",
  cesd = mean_cesd
))

# emmeans 
emmeans(ref_grid_family, ~ family_support_c | fam_rel_num_c, type = "response") %>%
  as_tibble() %>%
  mutate(
    fam_rel_num = fam_rel_num_c + mean_fam_rel_num,
    family_support = family_support_c + mean_family_support,
    fam_rel_num = factor(fam_rel_num)
  ) %>%
  ggplot(aes(x = family_support, y = emmean, color = fam_rel_num, fill = fam_rel_num, group = fam_rel_num)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  labs(x = "Family Support", 
       y = "Predicted MMSE Score",
       color = "Number of Family/Relatives",
       fill = "Number of Family/Relatives",
       title = "Figure 4. Interaction of Family Num × Family Support on MMSE") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

ggsave("figure4_familynum_familysupport.png", width = 7, height = 5)




# Section: Friend support*cesd

m3_support_cesd <- lm(mmse ~ sex + edu + income + smoker +  iadl_imp +
                        spouse_label+
                        friend_support + friend_support*cesd ,
                      data = complete_w1_fri)

# 최빈값(Mode) 계산 함수
get_mode <- function(x) {
  ux <- na.omit(x)                     # NA 제거
  ux[which.max(tabulate(match(ux, ux)))]  # 최빈값 반환
}


# 최빈값 계산
mode_sex    <- get_mode(complete_w1_fri$sex)
mode_edu    <- get_mode(complete_w1_fri$edu)
mode_income <- get_mode(complete_w1_fri$income)
mode_smoker <- get_mode(complete_w1_fri$smoker)
mode_drink  <- get_mode(complete_w1_fri$drinking)
mode_iadl   <- get_mode(complete_w1_fri$iadl_imp)


newdata <- expand.grid(
  cesd = 0:56,
  friend_support = 1:4
) %>%
  mutate(
    sex = mode_sex, #female
    edu = mode_edu,#elementary
    income = mode_income, #low
    smoker = mode_smoker, # non-smoker
    drinking = mode_drink, #driking
    iadl_imp = mode_iadl, # 일상생활 지장 x 

    spouse_label = "Has Spouse"
  )

newdata <- newdata |> bind_cols(predict(m3_support_cesd ,newdata,interval='confidence'))
newdata |> 
  mutate(
    friend_support=factor(friend_support)
  ) |>
  ggplot(aes(x=cesd, y=fit,fill=friend_support,color=friend_support))+
  geom_line()+
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3)+
  theme_classic()+
  labs(
    x = "CES-D (Depressive symptoms)",
    y = "MMSE"
  )
