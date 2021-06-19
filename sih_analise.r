library(microdatasus); library(tidyverse); library(readxl); library(lubridate)
library(readr); library(forcats); library(grafify); library(ggridges);
library(patchwork)

setwd("~/Documents/R/analises_junho")

# Baixando bases ----------------------------------------------------------

sih <- fetch_datasus(year_start = 2010, month_start = 1, year_end = 2019, 
                        month_end = 12, uf = "DF", information_system = "SIH-RD") %>% 
                        janitor::clean_names() 


estados <- read_excel("Lista-de-Municípios-com-IBGE-Brasil.xlsx", 
                         sheet = "Lista de Estados IBGE", 
                         col_types = c("text","text", "text", "text"))

cid_10 <- read_excel("cid_10.xlsx") %>% 
                        janitor::clean_names()

sigtap <- read_excel("sigtap.xlsx") %>% 
                        janitor::clean_names()


estabelecimentos <- read_delim("estabelecimentos-530000-202012.csv", 
                                             ";", escape_double = FALSE, 
                               col_types = cols(COMPETENCIA = col_character(), 
                                                IBGE = col_character()), trim_ws = TRUE) %>% 
                    janitor::clean_names() %>% 
                    select(cnes,nome_fantasia,razao_social)

hospitais <- c("0010456","0010464","5717515","0010502","0010480","0010499","0010472","2645157","0010510","3276678","2673916")


# Enriquecimento de bases -------------------------------------------------

sih_df_rd <- sih %>% 
                  select(cnes, ano_cmpt, mes_cmpt, espec, 
                         cep, munic_res, nasc, sexo, marca_uti, 
                         qt_diarias, proc_rea, val_sh, val_sp, dt_inter, dt_saida, proc_solic,
                         diag_princ, cobranca, dias_perm, morte, car_int, cbor, cid_morte,
                         complex, raca_cor, nacional, diagsec1) %>% 
                  filter(marca_uti == "74" | marca_uti == "75" | marca_uti == "76") %>% 
                  mutate(codigo_uf = str_sub(munic_res, end = 2)) %>% 
                  filter(cnes %in% hospitais)  


sih_enriquecido <- sih_df_rd %>% 
                      left_join(estabelecimentos, by = c("cnes"="cnes")) %>% 
                      left_join(cid_10, by = c ("diag_princ" = "cod_subcat")) %>% 
                      left_join(sigtap, by = c ("proc_rea" = "codigo")) %>% 
                      left_join(estados, by = c("codigo_uf"="IBGE"))
                      
#writexl::write_xlsx(sih_enriquecido, 'sih_uti.xlsx')
                      
# Selecionando o que importa ----------------------------------------------

rd_selec <- sih_enriquecido %>%
              mutate(uf = str_sub(munic_res,end = 2)) %>% 
              mutate(origem = if_else(uf == "53","DF","fora do DF")) %>% 
              mutate(sexo = case_when(sexo == "1" ~ "Masculino",
                                                sexo == "2" | sexo == "3" ~ "Feminino")) %>% 
                     mutate(dt_inter = ymd(dt_inter), dt_saida = ymd(dt_saida),
                     competen = paste(ano_cmpt,mes_cmpt,sep=""),
                     mes_ano = zoo::as.yearmon(competen, "%Y %m"), 
                     raca = case_when(raca_cor == "01" ~ "branco",
                                      raca_cor == "02" ~ "preta",
                                      raca_cor == "03" ~ "parda", 
                                      raca_cor == "04" ~ "amarela",
                                      raca_cor == "05" ~ "indigena"),
                     valor_por_dia_sh = val_sh/dias_perm,
                     valor_por_dia_sp = val_sp/dias_perm,
                     nasc = ymd(nasc), 
                     idade = round((dt_inter - nasc)/365.25))

#rd_selec[,c(1,2,4)] <- lapply(rd_selec[,c(1,2,4)], as.character)
#rd_selec[,c(1,2,4)] <- lapply(rd_selec[,c(1,2,4)], as.numeric)

# Agrupando por dia e hospital -------------------------------------------

internacoes_dia <- rd_selec %>% 
                        group_by(ano_cmpt,nome_fantasia) %>% 
                        summarise(total = n()) 

internacoes_dia %>% 
  ggplot(aes(ano_cmpt,total)) + geom_line() + theme_bw() + 
  facet_wrap(~nome_fantasia, scales = "free_y")



# Análise do hospital de base ---------------------------------------------

# análise de tipos de causas de internação 
# hospital_base <- rd_selec %>% 
#                     filter(cnes == "0010456") %>% 
#                     filter(ano_cmpt == 2010 | ano_cmpt == 2019) %>% 
#                     group_by(ano_cmpt,categoria) %>% 
#                     count() %>% 
#                     group_by(ano_cmpt) %>% 
#                     slice_max(order_by = n, n = 6) %>% 
#                     ungroup()


hospital_base_2010 <- rd_selec %>% 
  filter(cnes == "0010456") %>% 
  filter(ano_cmpt == 2010) %>% 
  group_by(ano_cmpt,capitulo) %>% 
  count() %>% 
  mutate(prop = n/sum(hospital_base_2010$n))

hospital_base_2019 <- rd_selec %>% 
  filter(cnes == "0010456") %>% 
  filter(ano_cmpt == 2019) %>% 
  group_by(ano_cmpt,capitulo) %>% 
  count() %>% 
  mutate(prop = n/sum(hospital_base_2019$n))

hospital_base <- rbind(hospital_base_2010,hospital_base_2019) %>% 
                             group_by(ano_cmpt) %>% 
                             slice_max(order_by = prop, n = 6) %>% 
                             ungroup()
  
hospital_base %>% 
  plot_befafter_colors(ano_cmpt,prop,capitulo) + 
  ggtitle("Hospital de Base") + ylab("total") + xlab("ano") + 
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 14)) +
  theme(plot.title = element_text(size=16))

# Sexo de pessoas internadas

rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt,sexo) %>% 
  count() %>% 
  ggplot(aes(x=ano_cmpt,y=n,fill=sexo)) + 
  geom_bar(position = "fill",stat = "identity") + 
  ggtitle("Hospital de Base","Percentual de sexo em relação ao internados") +
  ylab("Percentual(%)") + xlab("Ano") + scale_y_continuous(breaks = seq(from=0,to=1,by=0.10)) +
  geom_hline(yintercept = 0.60, linetype = "dashed") +
  theme_minimal()

# Idade dos internados por sexo 

rd_selec %>% 
  filter(cnes == "0010456") %>% 
  ggplot(aes(x=ano_cmpt,y=idade,fill=sexo)) + 
  geom_boxplot() + scale_y_continuous(breaks = seq(from=0,to=100,by=10)) +
  ggtitle("Hospital de Base","Boxplot por idade e sexo") +
  ylab("Idade") + xlab("Ano") +
  theme_minimal() 

rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt, sexo) %>% 
  summarise(idade_mediana = median(idade), idade_min = min(idade),
            idade_max = max(idade))

# Quantidade de dias internados 

# visual - gráfico boxplot
rd_selec %>% 
  filter(cnes == "0010456") %>% 
  ggplot(aes(x=qt_diarias,y=ano_cmpt)) + 
  geom_boxplot() + scale_x_continuous(breaks = seq(from=0,to=250,by=10)) +
  ggtitle("Hospital de Base","Boxplot por mediana de diárias e sexo") +
  ylab("Diárias") + xlab("Ano") +
  theme_minimal() + geom_vline(xintercept = 7, linetype = "dashed", color = "red")

# visual - barras 
rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt,sexo) %>%
  summarise(mediana_diarias = median(qt_diarias)) %>% 
  ggplot(aes(x=ano_cmpt,y=mediana_diarias,fill=sexo)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(breaks = seq(from=0,to=15,by=1)) +
  ggtitle("Hospital de Base","Mediana de diárias por sexo") +
  ylab("Diárias") + xlab("Ano") +
  theme_minimal() 

# tabela
diarias_base <- rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt,sexo) %>% 
  summarise(media = mean(qt_diarias),
            median = median(qt_diarias),
            min = min(qt_diarias),
            max = max(qt_diarias))


# diárias por capítulo 
diarias_base_capitulo <- rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt,capitulo,cod_capitulo) %>% 
  summarise(media = mean(qt_diarias),
            median = median(qt_diarias),
            min = min(qt_diarias),
            max = max(qt_diarias)) %>% 
  filter(cod_capitulo != "HH" & cod_capitulo != "LL" & cod_capitulo != "ZZ")

diarias_base_capitulo %>%
  ungroup() %>% 
  ggplot(aes(ano_cmpt,median)) + geom_col() +   
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 5)) +
  coord_flip() + facet_wrap(~capitulo) + theme_minimal() 
  
# Origem do paciente 

rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt, origem) %>% 
  count() %>% 
  ggplot(aes(x=ano_cmpt, n, fill=origem)) + 
  geom_bar(position = "fill",stat = "identity") + 
  geom_hline(yintercept = 0.22, linetype = "dashed") +
  theme_minimal() + 
  ggtitle("Hospital de Base","Origem dos pacientes") 

# Valor diário médio - serviços hospitalares 

rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt,sexo) %>%
  summarise(mediana_valores = median(valor_por_dia_sh)) %>% 
  ggplot(aes(x=ano_cmpt,y=mediana_valores,fill=sexo)) + 
  geom_col(position = "dodge") +
  ggtitle("Hospital de Base","Mediana de valores de serviços hospitalares") +
  ylab("Valores em R$") + xlab("Ano") +
  theme_minimal() 

# valor
rd_selec %>% 
  filter(cnes == "0010456") %>% 
  group_by(ano_cmpt,sexo) %>%
  summarise(mediana_valores = median(valor_por_dia_sp)) %>% 
  ggplot(aes(x=ano_cmpt,y=mediana_valores,fill=sexo)) + 
  geom_col(position = "dodge") +
  ggtitle("Hospital de Base","Mediana de valores de serviços profissionais") +
  ylab("Valores em R$") + xlab("Ano") +
  theme_minimal() 


# Hospital Regional de Taguating -------------------------------------------------

# Sexo de pessoas internadas

rd_selec %>% 
  filter(cnes == "0010499") %>% 
  group_by(ano_cmpt,sexo) %>% 
  count() %>% 
  ggplot(aes(ano_cmpt,n,fill=sexo)) + 
  geom_bar(position = "fill",stat = "identity") + 
  ggtitle("Hospital Regional de Taguatinga","Percentual de sexo em relação ao internados") +
  ylab("Percentual(%)") + xlab("Ano") + geom_hline(yintercept = 0.60, linetype = "dashed") +
  theme_minimal()
