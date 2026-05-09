## translate table 
## dhemerson.costa@ipam.org.br
library(dplyr)
library(ggplot2)

options(scipen=9e6)

## read table
data <- read.csv('./table/collection101-biomes-state-prodesMask-v3.csv') %>%
  select(-system.index, -.geo)

## translate ids
data$uf <- data$class_id %% 100
data$biome <- floor(data$class_id /100)


## translate
data <- data %>%
  mutate(biome = gsub(1, 'Amazônia', biome),
         biome = gsub(4, 'Cerrado', biome))

## get onjly cerrado and amazon
data <- subset(data, biome == 'Amazônia' | biome == 'Cerrado')

## translate
data <- data %>%
  mutate(uf = gsub(13, 'Amazonas', uf),
         uf = gsub(51, 'Mato Grosso', uf),
         uf = gsub(11, 'Rondônia', uf),
         uf = gsub(15, 'Pará', uf),
         uf = gsub(14, 'Roraima', uf),
         uf = gsub(17, 'Tocantins', uf),
         uf = gsub(21, 'Maranhão', uf),
         uf = gsub(16, 'Amapá', uf),
         uf = gsub(35, 'São Paulo', uf),
         uf = gsub(41, 'Paraná',uf),
         uf = gsub(31, 'Minas Gerais', uf),
         uf = gsub(50, 'Mato Grosso do Sul', uf),
         uf = gsub(29, 'Bahia', uf),
         uf = gsub(22, 'Piauí', uf),
         uf = gsub(12, 'Acre', uf))

## rename column
colnames(data)[3] <- 'PRODES_mask'

## get only soybeAN
#data <- subset(data, class == 39)

# Assuming your data frame is named df
df_summary <- data %>%
  group_by(year, biome) %>%
  summarise(total_area = sum(area, na.rm = TRUE)) %>%
  ungroup()

# 
# ggplot(data= data, mapping= aes(x= year, y= area/1e6, fill= class_id)) +
#   geom_bar(stat= 'identity') +
#   scale_fill_manual(values=c('#1f8d49', '#d6bc74', '#d4271e', '#ffefc3', '#edde8e', '#7dc975', '#C27BA0', '#2532e4', '#519799'))+ 
#   facet_wrap(~PRODES_mask) +
#   theme_bw() +
#   ylab('Area (Mha)')

write.table(data, './collection101-biomes-state-prodesMask-v3.csv', sep=';')

