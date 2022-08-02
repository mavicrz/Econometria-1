# Fecundidade SP - Formas funcionais - Parte 2----------------------------------
# Aluna: Maria Cruz
# Descrição: Este código realiza as atividades da segunda parte do roteiro
# para o trabalho de Econometria 1

# 0. Configuração---------------------------------------------------------------
rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse', 'purrr', 'fixest', 'readxl'), install = T)

# Input
file_fec <- 'Trabalho/input/Tema 1 - Fecundidade em SP.xlsx'

# 1. Tratar base ---------------------------------------------------------
painel_fec <- readxl::read_xlsx(file_fec) %>%
  dplyr::rename(race= 2) %>% 
  dplyr::mutate(salariom_2 = salariom +1,
                race = as.double(case_when(
                  race == 'indigena' ~ '0',
                  T ~ race)),
                educ_cat = case_when(
                  educ < 4 ~ 0, # Fundamental 1 incompleto
                  educ %in% 5:8 ~ 1, # Fundamental 2 incompleto
                  educ %in% 9:11 ~ 2, # Ensino médio incompleto
                  educ  == 12 ~ 3, #Ensino médio completo
                  educ > 12 ~4 #Ensino superior
                ),
                salario_hora =  salariom/horasm + 0.1,
                idade_cat = case_when(
                  idade < 20 ~ 1, 
                  idade %in% 20:30 ~ 2,
                  idade >30 ~ 3
                ),
                riqueza = case_when(
                  salario_hora > 5.747126 ~ 1,
                  T ~ 0
                ),
                renda_cat = ntile(salario_hora, n = 5)
  )

painel_fec_rep <- painel_fec %>% 
  dplyr::filter(idade %in% 15:49) %>% 
  dplyr::mutate(renda_cat = factor(ntile(salario_hora, n = 5)))

# 2. Descritivas -----------------------------------------------------

# Descritivas gerais
painel_fec_rep %>% 
  dplyr::select(salariom, educ, idade, filhos) %>%
  as.data.frame() %>% 
  stargazer(type = "text", title = "Análise das variáveis",
            summary.stat = c("mean", "sd", 'median'))

# Distribuição de salários - quantidade
painel_fec_rep %>%
  ggplot(., aes(x=(salariom))) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(salariom, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ylab('Quantidade de mulheres') + xlab('Salário Mensal') +
  theme_minimal()

# Distribuição de salários - densidade
painel_fec_rep %>%
  ggplot(., aes(x=salariom)) + geom_density() +
  ylab('Densidade') + xlab('Salário Mensal') +
  theme_minimal()
  
# Distribuição de escolaridade
painel_fec_rep %>%
  ggplot(., aes(x=(educ))) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(educ, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ylab('Quantidade de mulheres') + xlab('Anos de educação') +
  theme_minimal()

# Distribuição de escolaridade - densidade
painel_fec_rep %>%
  ggplot(., aes(x=educ)) + geom_density() +
  ylab('Densidade') + xlab('Anos de educação') +
  theme_minimal()

# Média de filhos por quantis de escolaridade
painel_fec_rep %>% 
  group_by(educ_cat) %>% 
  summarise(filhos = mean(filhos, na.rm = TRUE), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot() +
  geom_bar(mapping = aes(x = educ_cat, y = filhos), stat = 'identity')

# Média de filhos por quantis de renda
painel_fec_rep %>% 
  group_by(renda_cat) %>% 
  summarise(filhos = mean(filhos, na.rm = TRUE), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot() +
  geom_bar(mapping = aes(x = renda_cat, y = filhos), stat = 'identity')

# Média de filhos por quantis de renda separado em grupos de escolaridade alto e baixo
painel_fec_rep %>% 
  mutate(Educação = factor(case_when(educ_cat >= 3 ~ 'Ensino Básico Incompleto', educ_cat < 3 ~ 'Ensino Básico Completo'))) %>%
  group_by(renda_cat, Educação) %>% 
  summarise(filhos = mean(filhos, na.rm = TRUE), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot() +
  geom_bar(mapping = aes(x = renda_cat, y = filhos, fill = Educação), stat = 'identity', position = 'dodge')+
  ylab('Média de Filhos') + xlab('Distribuição de renda') + 
  scale_fill_brewer(type = 'qual')+
  theme_minimal()


# 3. Modelo 1-----------------------------------------------------
reg_0 <- painel_fec_rep %>%
    fixest::feols(filhos ~ i(educ_cat, ref = 0),  vcov = 'hetero')
  
table_0 <-  etable(reg_0) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_0, path = 'Trabalho/output/table_0.xlsx')
  
# 4. Modelo 2.1 -----------------------------------------------------
reg_1_baixa_educ <- painel_fec_rep %>%
    dplyr::filter(educ_cat <3) %>% 
    fixest::feols(filhos ~ log(salario_hora),  vcov = 'hetero')

etable(reg_1_baixa_educ)

# 5. Modelo 2.2 -----------------------------------------------------
reg_1_alta_educ <- painel_fec_rep %>%
  dplyr::filter(educ_cat >=3) %>% 
  fixest::feols(filhos ~ log(salario_hora),  vcov = 'hetero')

etable(reg_1_alta_educ)

table_1 <- etable(reg_1_alta_educ,reg_1_baixa_educ) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_1, path = 'Trabalho/output/table_1.xlsx')

# 5. Modelo 2.3 -----------------------------------------------------
reg_1 <- painel_fec_rep %>%
  dplyr::mutate(high_educ = case_when(educ_cat >=3 ~ 1,
                                      T ~ 0))%>% 
  fixest::feols(filhos ~ log(salario_hora) + high_educ,  vcov = 'hetero')

etable(reg_1)

table_1_2 <- etable(reg_1) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_1_2, path = 'Trabalho/output/table_1_2.xlsx')

# 6. Modelo 3.1 -----------------------------------------------------
reg_2_baixa_educ <- painel_fec_rep %>%
  dplyr::mutate(salario_hora_2 = log(salario_hora)^2) %>%
  dplyr::filter(educ_cat <3) %>% 
  fixest::feols(filhos ~ log(salario_hora) + salario_hora_2,  vcov = 'hetero')

etable(reg_2_baixa_educ)

# 7. Modelo 3.2 -----------------------------------------------------
reg_2_alta_educ <- painel_fec_rep %>%
  dplyr::mutate(salario_hora_2 = log(salario_hora)^2) %>%
  dplyr::filter(educ_cat >=3) %>% 
  fixest::feols(filhos ~ log(salario_hora) + salario_hora_2, vcov = 'hetero')

etable(reg_2_alta_educ)

table_2 <- etable(reg_2_alta_educ,reg_2_baixa_educ) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_2, path = 'Trabalho/output/table_2.xlsx')

# 8. Modelo 3.3 -----------------------------------------------------

reg_2 <- painel_fec_rep %>%
  dplyr::mutate(high_educ = case_when(educ_cat >=3 ~ 1,
                                      T ~ 0))%>% 
  fixest::feols(filhos ~  log(salario_hora) + salario_hora_2 + high_educ,  vcov = 'hetero')

etable(reg_2)

table_2_2 <- etable(reg_2) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_2_2, path = 'Trabalho/output/table_2_2.xlsx')


# 9. Teste Modelo 1-----------------------------------------------------
reg_0_1 <- painel_fec_rep %>% 
  fixest::feols(filhos ~ i(educ_cat, ref = 3),  vcov = 'hetero')

table_0_1 <-  etable(reg_0_1) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_0_1, path = 'Trabalho/output/table_0_1.xlsx')

# 9.1 Teste 2 Modelo 1-----------------------------------------------------
reg_0_2 <- painel_fec_rep %>% 
  fixest::feols(filhos ~ i(educ_cat, ref = 0) + domestico + race + riqueza,  vcov = 'hetero')

table_0_2 <-  etable(reg_0_2) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_0_2, path = 'Trabalho/output/table_0_2.xlsx')


# 10. Teste Modelo 2-----------------------------------------------------
## 10.1. Modelo 1-----------------------------------------------------
reg_1_1_baixa_educ <- painel_fec_rep %>%
  dplyr::filter(educ_cat <3) %>% 
  fixest::feols(filhos ~ log(salario_hora)+ domestico + race + riqueza,  vcov = 'hetero')

etable(reg_1_1_baixa_educ)

## 10.2. Modelo 2.1 -----------------------------------------------------
reg_1_1_alta_educ <- painel_fec_rep %>%
  dplyr::filter(educ_cat >=3) %>% 
  fixest::feols(filhos ~ log(salario_hora) + domestico + race  + riqueza,  vcov = 'hetero')

etable(reg_1_1_alta_educ)

table_1_1 <- etable(reg_1_1_alta_educ,reg_1_1_baixa_educ) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_1_1, path = 'Trabalho/output/table_1_1.xlsx')


# 11. Teste Modelo 2-----------------------------------------------------
## 11.1. Modelo 3.1 -----------------------------------------------------
reg_2_1_baixa_educ <- painel_fec_rep %>%
  dplyr::mutate(salario_hora_2 = log(salario_hora)^2) %>%
  dplyr::filter(educ_cat <3) %>% 
  fixest::feols(filhos ~ log(salario_hora) + salario_hora_2  + domestico + race  + riqueza,  vcov = 'hetero')

etable(reg_2_1_baixa_educ)

## 11.2. Modelo 3.2 -----------------------------------------------------
reg_2_1_alta_educ <- painel_fec_rep %>%
  dplyr::mutate(salario_hora_2 = log(salario_hora)^2) %>%
  dplyr::filter(educ_cat >=3) %>% 
  fixest::feols(filhos ~ log(salario_hora) + salario_hora_2  + domestico + race  + riqueza, vcov = 'hetero')

etable(reg_2_1_alta_educ)

table_2_1 <- etable(reg_2_1_baixa_educ,reg_2_1_alta_educ) %>% 
  tibble::as_tibble(rownames = NA) %>% 
  dplyr::mutate(names = row.names(.))

writexl::write_xlsx(table_2_1, path = 'Trabalho/output/table_2_1.xlsx')

# 12. Predict -----------------------------------------------------
## 12.1. Modelo 1 -----------------------------------------------------

pred_1 <- painel_fec_rep %>% 
  dplyr::select(educ_cat) %>%
  dplyr::filter(is.na(.)==F) %>% 
  arrange(educ_cat) %>% 
  distinct(.) %>% 
  predict(object = reg_0, newdata = .)


## 12.2. Modelo 2 -----------------------------------------------------

pred_2_alta <- painel_fec_rep %>% 
  dplyr::filter(is.na(salario_hora)==F & educ_cat >=3) %>% 
  dplyr::select(salario_hora) %>%
  dplyr::mutate(salario_hora = mean(salario_hora)) %>% 
  dplyr::distinct() %>% 
  predict(object = reg_1_alta_educ, newdata = .)


pred_2_baixa <- painel_fec_rep %>% 
  dplyr::filter(is.na(salario_hora)==F & educ_cat <3) %>% 
  dplyr::select(salario_hora) %>%
  dplyr::mutate(salario_hora = mean(salario_hora)) %>% 
  dplyr::distinct() %>% 
  predict(object = reg_1_baixa_educ, newdata = .)

## 12.2. Modelo 3 -----------------------------------------------------

pred_3_alta <- painel_fec_rep %>% 
  dplyr::filter(is.na(salario_hora)==F & educ_cat >=3) %>% 
  dplyr::select(salario_hora) %>%
  dplyr::mutate(salario_hora = mean(salario_hora),
                salario_hora_2 = mean(salario_hora*salario_hora)) %>% 
  dplyr::distinct() %>% 
  predict(object = reg_2_alta_educ, newdata = .)


pred_3_baixa <- painel_fec_rep %>% 
  dplyr::filter(is.na(salario_hora)==F & educ_cat <3) %>% 
  dplyr::select(salario_hora) %>%
  dplyr::mutate(salario_hora = mean(salario_hora),
                salario_hora_2 = mean(salario_hora*salario_hora))%>% 
  dplyr::distinct() %>% 
  predict(object = reg_2_baixa_educ, newdata = .)



