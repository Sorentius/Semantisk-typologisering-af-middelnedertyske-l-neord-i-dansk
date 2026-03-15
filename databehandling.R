##vi indlæser de relevante pakker
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

## importere samlede liste
mergedlists <- read_csv2("data/data_input/mergedlists.csv")

## gøre alle forekomster til små bogstaver
mergedlists$lemma <- tolower(mergedlists$lemma)
view(mergedlists)

## rense listen for doppelte/tredoppelte forekomster
mergedlists_cleaned <- mergedlists %>%
  distinct(lemma, .keep_all = TRUE)

## eksport af mergedlists_cleaned
write_csv2(mergedlists_cleaned, file = "data/data_output/mergedlists_cleaned.csv")

## import af den manuelt med ordklasser annoterede fil
mergedlists_annotated <- read_csv2("data/data_input/mergedlists_annotated.csv")

## lad os for sjov lave et plot over fordelingen af ordklasser
wordclasses <- mergedlists_annotated %>% 
  count(ordklasse)
view(wordclasses)
## hovsa - der var et par slåfejl i datasættet - f.eks. var et adjektiv fejlagtigt angivet som "ADJ" i stedet for "A". 
## Derudover var et substantiv et par gange fejlagtigt tagget med "N" i stedet for "S", samt en interjektion "I" angivet med "U".
## finde ordet med den manglende ordklasseangivelse
mergedlists_nofpcolumn %>%
  filter(is.na(ordklasse) | trimws(ordklasse) == "")
## fejlene rettes i excelarket, og gemmes som "table_all.csv" i data/data_input"

## "table_all" indlæses
table_all <- read_csv2("data/data_input/table_all.csv")

## vi tjekker igen ordklasserne
wordclasses <- table_all %>% 
  count(ordklasse)
view(wordclasses)
## det passer nu

##udtræk af substantiver
nouns_raw <- table_all %>% 
  filter(ordklasse == "S")
view(nouns_raw)
write.csv2(nouns, file = "Data/data_output/nouns_raw.csv")

## Substantiverne tagges og gennemlæses i excel i data_input/excelfiles, og gemmes som nouns_tagged.csv i data_input.

## vi indlæser de taggede substantiver
nouns <- read_csv2(
  "data/data_input/nouns_tagged.csv")
view(nouns)
## tjek ok

## fjernelse af false positives, inklusive søjlen
nouns_cleaned <- nouns[is.na(nouns$falsepositive)|nouns$falsepositive !=1, ] %>% select(-falsepositive)
view(nouns_cleaned)
##tjek ok

## fjernelse af kalkeringer og nydannelser
nouns_cleaned_only_loanwords <- filter(nouns_cleaned, transfertype == "L") %>% select(-c(transfertype, ordklasse))
view(nouns_cleaned_only_loanwords)
write_csv2(nouns_cleaned_only_loanwords, file = "data/data_output/nouns_cleaned_only_loanwords.csv")

## tælle antal låneord, kalkeringer og nydannelser
transfertype_counts <- nouns_cleaned %>% 
  count(transfertype) %>% 
  arrange(desc(n))
view(transfertype_counts)

## beregne transfertype i procenter
transfertype_percentages <- transfertype_counts %>% 
  mutate(Percentage = n / nrow(nouns_cleaned) * 100) %>% 
  arrange(desc(Percentage))
view(transfertype_percentages)

## lister med låneord, kalkeringer og nydannelser
låneord <- nouns_cleaned %>% 
  filter(if_any(transfertype, ~ str_detect(.x, "L")))
view(låneord)
write_csv2(låneord, file = "data/data_output/låneord.csv")

kalkeringer <- nouns_cleaned %>% 
  filter(if_any(transfertype, ~ str_detect(.x, "K")))
view(kalkeringer)
write_csv2(kalkeringer, file = "data/data_output/kalkeringer.csv")

nydannelser <- nouns_cleaned %>% 
  filter(if_any(transfertype, ~ str_detect(.x, "N")))
view(nydannelser)
write_csv2(nydannelser, file = "data/data_output/nydannelser.csv")

## splitte rækkerne ad, således at hver enkelt semantisk tag står for sig
tagset_nouns <- nouns_cleaned_only_loanwords %>% 
  separate_rows(tags, sep = ";")
view(tagset_nouns)

## tælle tagsene
tag_counts <- tagset_nouns %>% 
  count(tags) %>% 
  arrange(desc(n))
view(tag_counts)
## tjek ok

## nu i procenter
tag_percentages <- tag_counts %>% 
  mutate(Percentage = n / nrow(tagset_nouns) * 100) %>% 
  arrange(desc(Percentage))
view(tag_percentages)
## tjek ok, udskrives med det samme
write_csv2(tag_percentages, file = "data/data_output/tag_percentages.csv")

## diagram over tagsene
tag_counts %>% 
  ggplot(aes(x = reorder(tags, -n), y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Fordeling af semantiske områder",
    x = "Semantiske område",
    y = "Antal"
  ) +
  theme_minimal()

## udtræk af de forskellige semantiske områder
håndværk <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "HÅN")))
view(håndværk)
write_csv2(håndværk, file = "data/data_output/semantic_areas/håndværk.csv")

samfund <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "SAM")))
view(samfund)
write_csv2(samfund, file = "data/data_output/semantic_areas/samfund.csv")

handling <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "HAN")))
view(handling)
write_csv2(handling, file = "data/data_output/semantic_areas/handling.csv")

fauna <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "FAU")))
view(fauna)
write_csv2(fauna, file = "data/data_output/semantic_areas/fauna.csv")

militær <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "MIL")))
view(militær)
write_csv2(militær, file = "data/data_output/semantic_areas/militær.csv")

søfart <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "SØF")))
view(søfart)
write_csv2(søfart, file = "data/data_output/semantic_areas/søfart.csv")

husholdning <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "HUS")))
view(husholdning)
write_csv2(husholdning, file = "data/data_output/semantic_areas/husholdning.csv")

flora <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "FLO")))
view(flora)
write_csv2(flora, file = "data/data_output/semantic_areas/flora.csv")

verden <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "VER")))
view(verden)
write_csv2(verden, file = "data/data_output/semantic_areas/verden.csv")

gastronomi <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "GAS")))
view(gastronomi)
write_csv2(gastronomi, file = "data/data_output/semantic_areas/gastronomi.csv")

følelsesliv <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "FØL")))
view(følelsesliv)
write_csv2(følelsesliv, file = "data/data_output/semantic_areas/følelsesliv.csv")

besiddelse <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "BES")))
view(besiddelse)
write_csv2(besiddelse, file = "data/data_output/semantic_areas/besiddelse.csv")

kropslighed <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "KRO")))
view(kropslighed)
write_csv2(kropslighed, file = "data/data_output/semantic_areas/kropslighed.csv")

beklædning <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "BEK")))
view(beklædning)
write_csv2(beklædning, file = "data/data_output/semantic_areas/beklædning.csv")

jura <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "JUR")))
view(jura)
write_csv2(jura, file = "data/data_output/semantic_areas/jura.csv")

religiøsitet <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "REL")))
view(religiøsitet)
write_csv2(religiøsitet, file = "data/data_output/semantic_areas/religiøsitet.csv")

kvantitet <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "KVA")))
view(kvantitet)
write_csv2(kvantitet, file = "data/data_output/semantic_areas/kvantitet.csv")

kognition <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "KOG")))
view(kognition)
write_csv2(kognition, file = "data/data_output/semantic_areas/kognition.csv")

bevægelse <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "BEV")))
view(bevægelse)
write_csv2(bevægelse, file = "data/data_output/semantic_areas/bevægelse.csv")

rumlighed <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "RUM")))
view(rumlighed)
write_csv2(rumlighed, file = "data/data_output/semantic_areas/rumlighed.csv")

musik <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "MUS")))
view(musik)
write_csv2(musik, file = "data/data_output/semantic_areas/musik.csv")

sprog <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "SPR")))
view(sprog)
write_csv2(sprog, file = "data/data_output/semantic_areas/sprog.csv")

slægtskab <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "SLÆ")))
view(slægtskab)
write_csv2(slægtskab, file = "data/data_output/semantic_areas/slægtskab.csv")

tid <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "TID")))
view(tid)
write_csv2(tid, file = "data/data_output/semantic_areas/tid.csv")

sansning <- nouns_cleaned_only_loanwords %>% 
  filter(if_any(tags, ~ str_detect(.x, "SAN")))
view(sansning)
write_csv2(sansning, file = "data/data_output/semantic_areas/sansning.csv")
## færdig
