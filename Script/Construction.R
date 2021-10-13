
# Setting -----------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(data.table)
library(lubridate)
library(gt)
library(broom)
library(modelsummary)
library(ggplot2)
library(tictoc)
library(systemfonts)
par(family = "Yu Gothic")

# 参考文献情報

# メモ
# 16期間をjoinしてローリング回帰
# 企業・四半期ごとの係数を付したパネル・データが得られる
# 企業・四半期データごとにnestして、業種・四半期で他企業の係数情報をjoin（全数マッチング）
# fillで株価データとオリジナルの予測値を補完し、他企業の係数でも予測値を計算して列同士で比較可能性を演算
# 中央値や上位4社の平均値などの統計量を得る


# Data -------------------------------------------------


# 時価総額データ
DataMV <- fread("../../../Database/FQ/AccountingExp.csv", encoding = "UTF-8") %>% 
  filter(FiscalEnd >= "2008-03-31") %>% 
  filter(FiscalEnd <= "2019-03-31") %>% 
  filter(MM == 3) %>% 
  rename(MV0=MarketValueFE, MV3=MarketValueFE3, MV6=MarketValueFE6, MV9=MarketValueFE9) %>%
  select(NKCODE,FiscalEnd,starts_with("MV")) %>% 
  drop_na %>% 
  mutate(
    Q1 = ceiling_date(FiscalEnd %m+% months(3), "month") - 1,
    Q2 = ceiling_date(FiscalEnd %m+% months(6), "month") - 1,
    Q3 = ceiling_date(FiscalEnd %m+% months(9), "month") - 1,
    Q4 = ceiling_date(FiscalEnd %m+% months(12), "month") - 1) %>% 
  select(NKCODE, starts_with("MV"), starts_with("Q")) %>% 
  pivot_longer(c(-NKCODE,-Q1:-Q4), names_to = "Quarter", values_to = "MV") %>% 
  mutate(
    FiscalEnd = case_when(Quarter == "MV0" ~ Q1, Quarter == "MV3" ~ Q2,
                          Quarter == "MV6" ~ Q3, Quarter == "MV9" ~ Q4)
  ) %>% 
  select(NKCODE,FiscalEnd,MV)

# Annual Data
DataAnnual <- fread("../../../Database/FQ/AccountingExp.csv", encoding = "UTF-8") %>% 
  filter(FiscalEnd >= "2008-03-31") %>% 
  filter(FiscalEnd <= "2019-03-31") %>% 
  filter(MM == 3) %>% 
  rename(P0=XBCLOSEFE, P3=XBCLOSEFE3, P6=XBCLOSEFE6, P9=XBCLOSEFE9, P12=XBCLOSEFE12) %>% 
  mutate(
    FiscalAnnual=FiscalEnd,
    RQ1 = (P3 - P0) / P0,
    RQ2 = (P6 - P3) / P3,
    RQ3 = (P9 - P6) / P6,
    RQ4 = (P12 - P9) / P9
    ) %>% 
  group_by(NKCODE) %>% 
  mutate(
    Lag1 = lag(MACC,1, default = 12),
    Lag2 = lag(MACC,2, default = 12),
    Lag3 = lag(MACC,3, default = 12),
    Lag4 = lag(MACC,4, default = 12)) %>% 
  ungroup %>% 
  # 16期間の間に決算期を変更していない
  filter(Lag1 == 12 & Lag2 == 12 & Lag3 == 12 & Lag4 == 12) %>%
  drop_na(starts_with("RQ")) %>%
  mutate(
    Q1 = ceiling_date(FiscalEnd %m+% months(3), "month") - 1,
    Q2 = ceiling_date(FiscalEnd %m+% months(6), "month") - 1,
    Q3 = ceiling_date(FiscalEnd %m+% months(9), "month") - 1,
    Q4 = ceiling_date(FiscalEnd %m+% months(12), "month") - 1) %>% 
  select(NKCODE, starts_with("RQ"), starts_with("Q")) %>% 
  pivot_longer(c(-NKCODE,-Q1:-Q4), names_to = "Quarter", values_to = "Return") %>% 
  mutate(
    FiscalEnd = case_when(Quarter == "RQ1" ~ Q1, Quarter == "RQ2" ~ Q2,
                          Quarter == "RQ3" ~ Q3, Quarter == "RQ4" ~ Q4)
  ) %>% 
  select(NKCODE,FiscalEnd,Return) %>% 
  distinct_all 

# Quarterly Data
DataQuart <- fread("../../../Database/FQ/QuarterlyExp.csv", encoding = "UTF-8") %>% 
  filter(FiscalEnd >= "2008-06-30") %>% 
  filter(FiscalEnd <= "2019-03-31") %>% 
  arrange(NKCODE, FiscalEnd) %>% 
  group_by(NKCODE) %>% 
  mutate(FiscalEnd = as_date(FiscalEnd),
         LagEarningsOya = lag(EarningsOya),
         EarningsOya = case_when(A01_HACC != "21" ~ (EarningsOya - LagEarningsOya),
                                  A01_HACC == "21" ~ EarningsOya)
         ) %>%
  ungroup %>% 
  # drop_na(EarningsOya) %>%
  select(NKCODE, FiscalEnd, EarningsOya) %>% 
  inner_join(DataAnnual, by=c("FiscalEnd", "NKCODE")) %>% 
  inner_join(DataMV, by=c("FiscalEnd", "NKCODE")) %>% 
  mutate(EarningsOya = EarningsOya / MV)

DataPack <- DataQuart
# 列名を動的に作成
for (n in 1:15) {
  column_name = str_c("Q",n)
  DataPack <- DataPack %>% 
    mutate(!!column_name := ceiling_date(FiscalEnd %m+% months(n*3), "month") - 1)
}
DataPack <- DataPack %>% 
  mutate(SEQ = str_c(row_number(), NKCODE, sep = "_")) %>% 
  select(NKCODE, SEQ, Q0=FiscalEnd, Q1:Q15) %>% 
  pivot_longer(c(-NKCODE,-SEQ), names_to = "Quarter", values_to = "FiscalEnd") %>% 
  inner_join(DataQuart, by=c("NKCODE","FiscalEnd")) %>% 
  group_by(SEQ) %>% 
  filter(n() == 16) %>% 
  nest %>% 
  ungroup


# Regression --------------------------------------------------------------

# 3分間
tic()
DataReg <- DataPack %>% 
  mutate(
    Model = map(data, ~ lm(EarningsOya ~ Return, data = .)),
    Results = map(Model, ~ tidy(.)),
    Fitted = map2(Model, data, ~ augment(.x, newdata = .y))
    )
toc()

# 業種情報
I <- fread("../../../Database/FQ/AccountingExp.csv", encoding = "UTF-8") %>% 
  select(NKCODE,CodeIndustry) %>% 
  distinct_all

# 決算期情報
FE <- DataPack %>% 
  unnest(data) %>% 
  select(SEQ,FiscalEnd)

# 係数情報
DataCoef <- DataReg %>% 
  select(-data, -Model) %>%
  unnest(Results) %>%
  select(SEQ,term,estimate) %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>% 
  rename(Intercept = 2, Beta = 3) %>% 
  inner_join(FE, by="SEQ") %>% 
  separate(SEQ, into = c("SEQ","NKCODE"), sep = "_") %>% 
  mutate(SEQ = as.numeric(SEQ), NKCODE = as.numeric(NKCODE)) %>% 
  inner_join(I, by="NKCODE") %>% 
  drop_na(Beta) %>% # 係数の推定失敗
  group_by(SEQ) %>% 
  mutate(When = str_c(min(FiscalEnd),"/",max(FiscalEnd))) %>% 
  ungroup

# Fitted Value
DataFitted <- DataReg %>% 
  select(SEQ,Fitted) %>% 
  unnest(cols=c(Fitted)) %>% 
  rename(EarningsFitted = .fitted) %>% 
  select(SEQ,NKCODE,FiscalEnd,Return,EarningsFitted) %>% 
  separate(SEQ, into = c("SEQ","NKCODE"), sep = "_") %>% 
  mutate(SEQ = as.numeric(SEQ), NKCODE = as.numeric(NKCODE)) %>% 
  inner_join(I, by=c("NKCODE")) %>% 
  group_by(CodeIndustry, FiscalEnd) %>% 
  filter(n() >= 48) %>% # 3企業以上
  ungroup %>% 
  group_by(SEQ) %>% 
  mutate(When = str_c(min(FiscalEnd),"/",max(FiscalEnd))) %>% 
  ungroup

# Comparebility -----------------------------------------------------------

# 業種名情報
I2 <- fread("../../../Database/FQ/AccountingExp.csv", encoding = "UTF-8") %>% 
  select(CodeIndustry,Industry) %>% 
  distinct_all

Database <- DataFitted %>% 
  # 日経業種中分類と四半期が一致する企業の係数を複数マッチング
  inner_join(DataCoef %>% select(SEQ2=SEQ,Intercept:When), 
             by=c("CodeIndustry", "FiscalEnd", "When")) %>%
  filter(SEQ != SEQ2) %>% # 企業i同士のマッチングを排除
  mutate(
    EarningsPred = Intercept + (Beta*Return),
    EarningsDiff = abs(EarningsFitted - EarningsPred)
    ) %>% 
  select(CodeIndustry,SEQ,SEQ2,NKCODE,EarningsDiff) %>% 
  group_by(SEQ,SEQ2) %>% 
  mutate(Comparebility = -1 * mean(EarningsDiff)) %>% 
  ungroup %>% 
  distinct(SEQ,SEQ2,Comparebility,.keep_all = TRUE) %>% 
  group_by(SEQ) %>% 
  mutate(MedComp = median(Comparebility),
         RankComp = dense_rank(Comparebility)) %>% 
  filter(RankComp <= 3) %>% 
  mutate(MeanComp = mean(Comparebility)) %>% 
  ungroup %>% 
  distinct(SEQ,MedComp,MeanComp,.keep_all = TRUE) %>% 
  inner_join(I2, by="CodeIndustry") %>% 
  select(SEQ, Industry, Comparebility, MedComp, MeanComp)

Data <- Database %>% 
  group_by(Industry) %>% 
  summarise(Mean = mean(MedComp)) %>% 
  arrange(Mean)

# Plot
Data %>% 
  ggplot(aes(x=reorder(Industry, Mean), y=Mean)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "産業別の比較可能性") +
  theme(plot.title = element_text(hjust = 0.5,size=rel(1.6),face="bold")) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave("Output/Plot/Comparability.png", device = cairo_pdf)

