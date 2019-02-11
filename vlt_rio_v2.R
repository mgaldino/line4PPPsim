## Rio de janeiro VLT
library(dplyr)
library(rstanarm)
library(ggplot2)
# instalar com devtools
#library(devtools); devtools::intall_github("mgaldino/line4PPPsim")
library(line4PPPsim)
library(tidyr)
library(readxl)
library(rbcb)
library(timeSeries)
library(forecast)
library(xts)

############################
### estima inflação futura
############################

# fonte dos dados do INPC
# https://www.ibge.gov.br/estatisticas-novoportal/economicas/precos-e-custos/9258-indice-nacional-de-precos-ao-consumidor.html?=&t=series-historicas

# importa inpc
setwd("C:/Users/mczfe/Pessoal/Freela/BID PPP")
inpc1 <- read_excel("inpc1.xlsx",
                    col_names = FALSE)
names(inpc1) <- c("inpc", "ano")

# limpa inpc
inpc1 <- inpc1 %>%
  mutate(inpc = as.numeric(gsub(",", ".", inpc))/100) %>%
  dplyr::filter( ano > 1998)
glimpse(inpc1)


# inflação de energia, coletada manualmente para alguns anos
# acumulado, 2013 ano base
energia_base <- c(1, 1.0299, 1.6516, 1.992, 1.8041, 1.9846)
# transformar em variação anual
en_obs <- lead(energia_base) / energia_base - 1
en_obs <- head(en_obs, 5) # removendo NA

df_en <- data.frame(ano = 2014:2018, infla_energia = en_obs)

# inflação do IPCA - dados da API do BC
ipca <- rbcb::get_series(c(IPCA = 433), last = 229, as = "tibble")
head(ipca)

# limpa base ipca
ipca1 <- ipca %>%
  mutate(ano = format(date, "%Y")) %>%
  group_by(ano) %>%
  mutate(indice = cumprod(1 + IPCA/100),
         max_data = max(date)) %>%
  dplyr::filter(max_data == date) %>%
  mutate(ipca_anual = indice - 1) %>%
  dplyr::filter(date < "2019-01-01")

# junta tudo (ipca, inpc e energia)
inflacao <- ipca1 %>%
  dplyr::select(ano, ipca_anual) %>%
  ungroup() %>%
  mutate(ano = as.numeric(ano)) %>%
  inner_join(inpc1, by = "ano") %>%
  left_join(df_en, by = "ano")

glimpse(inflacao)

# correlação entre inpc e energia é .926
# vamos usar como preditor


## gráfico das enegias (para ver a correlação)
p_infla <- ggplot(inflacao, aes(x=ano, y=inpc)) + geom_line(aes(colour="INPC")) +
  geom_line(aes(y=infla_energia, color="Energia")) + geom_line(aes(y=ipca_anual, color="IPCA")) +
  theme_bid(tam_fonte=16) + ylab("Índices de inflação e energia") + scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(name="",
                      values=c(INPC="red", Energia="blue", IPCA="purple"))

p_infla

ggsave(p_infla, file="inflacao.png", height = 8, width = 12, dpi = 300, type = "cairo", scale=.5)

# ---------------------------- #
## estimando inflação futura
# ---------------------------- #


## inflacao
inflacao2 <- data.frame(ano = 2019:2024,
                        ipca_anual = NA, inpc = NA, infla_energia = NA)
inflacao3 <- rbind(inflacao, inflacao2)

# usando arima para prever inflacao ipca, inpc e energia

## IPCA
ar1 <- inflacao3$ipca_anual[1:19]
ar1_fit <- arima(ar1, order = c(3, 0, 0), method = "ML")

ipca_pred <- ar1[17:19]
for ( i in 1:6) {
  ipca_pred[i+3] <- coef(ar1_fit)[4] + coef(ar1_fit)[1]*ipca_pred[i+2] + coef(ar1_fit)[2]*ipca_pred[i+1] + coef(ar1_fit)[3]*ipca_pred[i]
}

# inpc
ar2 <- inflacao3$inpc[1:19]
x <- ar1 # vamos usar ipca previsto como preditor, para ajudar a regularizar estimativas
ar2_fit <- arima(ar2, order = c(3, 0, 0), xreg = x, method = "ML")

inpc_pred <- ar2[17:19]
for ( i in 1:6) {
  inpc_pred[i+3] <- coef(ar2_fit)[4] + coef(ar2_fit)[1]*inpc_pred[i+2] + coef(ar2_fit)[2]*inpc_pred[i+1] +
    coef(ar2_fit)[3]*inpc_pred[i] + coef(ar2_fit)[4]*ipca_pred[i+3]
}

#energia

## energia
ar3 <- inflacao3$infla_energia[15:19]

ar3_fit <- arima(ar3, order = c(1, 0, 0), xreg = x[15:19], method = "ML")

en_pred <- ar3[5]
for ( i in 1:6) {
  en_pred[i+1] <- coef(ar3_fit)[2] + coef(ar3_fit)[1]*en_pred[i] + coef(ar3_fit)[3]*ipca_pred[i+3]
}

## Gerando distribuição para energia com multiavariada normal, anos independentes
dp <- sqrt(0.004815)
en_pred1 <- en_pred[-1] # exclui 2018
mat_id <- diag(length(en_pred1), length(en_pred1))/length(en_pred1)
mat_sigma <- mat_id*dp
n_sim <- 1000
sim_en <- mvtnorm::rmvnorm(n_sim, en_pred1, mat_sigma)



df_sim1 <- as.data.frame(sim_en) %>%
  gather(ano, en_pred) %>%
  mutate(ano = rep(2019:2024, each=n_sim),
         sim = rep(1:n_sim, 6)) %>%
  arrange(sim)

# gráfico da simulação da energia
df_prev <- data.frame(ano = 2019:2024, energia = en_pred1)


p_unif_energia <- ggplot() + geom_line(data=df_sim1, aes(ano, en_pred, group = sim), alpha= .1) +
  theme_bid(tam_fonte = 16) + ylab("Reajuste da energia") +
  geom_line(data = df_prev, aes(y=energia, x=ano, color = "média"), size=1) +
  scale_y_continuous(labels = scales::percent, limits = c(-1, 1.4)) +
  scale_colour_manual(name="", values=c(média="red"))

p_unif_energia <- p_unif_energia + ggtitle("Simulação da inflação da energia")

p_unif_energia

ggsave(p_unif_energia, file="inflacao_energia_sim.png", height = 8, width = 12,
       dpi = 300, type = "cairo", scale=.5)

## Simulando tarifa (reajuste)
ano <- 2014:2024

# reajuste previsto
infla_rev <- data.frame(ano = 2020:2024,
                        ipca_e = ipca_pred[-c(1:3, 9)],
                        inpc = inpc_pred[-c(1:3, 9)],
                        en = en_pred1[-6])

df_bind <- data.frame(ano = 2014, ipca_e= 0, inpc= 0, en= 0)

# reajuste observado
df_infla_aux <- inflacao3 %>%
  dplyr::filter(!is.na(infla_energia)) %>%
  mutate(ano = 2015:2019)  %>%
  rename(ipca_e = ipca_anual,
         en = infla_energia)

#junta tudo
df_infla <- df_bind %>%
  bind_rows(df_infla_aux) %>%
  bind_rows(infla_rev)

## funções que calculam reajuste da tarifa com energia e sem energia (Caso base)
tr <- function(tr0 = 1.98, inpc0 = 1, ipca_e0 = 1, en0 = 1, en ) {

  inpc = 1+ df_infla$inpc
  ipca_e = 1 + df_infla$ipca_e

  tr0*(.5*cumprod(inpc/inpc0) + .4*cumprod(ipca_e/ipca_e0) + .1*cumprod(en/en0))

}

tr1 <- function(tr0 = 1.98, inpc0 = 1, ipca_e0 = 1 ) {

  inpc = 1+ df_infla$inpc
  ipca_e = 1 + df_infla$ipca_e

  tr0*((5/9)*cumprod(inpc/inpc0) + (4/9)*cumprod(ipca_e/ipca_e0))


}

## base tr (sem energia)
base <- tr1()
df_base <- data.frame(base = base, ano = 2014:2024)


# ## sim reajuste tarifa
# gera data frame com dados observados
df_join <- data.frame(ano = rep(2014:2019, n_sim),
                      en_pred = rep(df_infla$en[1:6],n_sim),
                      sim = rep(1:n_sim, each=6))
#
# junta com dados simulados

df_sim2 <- df_sim1 %>%
  mutate( ano = ano + 1) %>%
  dplyr::filter(ano != 2025)

df_sim_tr <- df_sim2 %>%
  bind_rows(df_join) %>%
  arrange(sim, ano) %>%
  group_by(sim) %>%
  mutate(sim_tr = tr(en = 1 + en_pred))

energia_media <- tr(en = 1 + c(df_infla$en[1:6],en_pred[-c(1,7)]))
df_en_media <- data.frame(ano = 2014:2024, en_media = energia_media)

# gráfico do reajuste da tarifa
p_en <- ggplot() + geom_line(data=df_sim_tr, aes(ano, sim_tr, group = sim), alpha= .1) +
geom_line(data = df_base, aes(y=base,x=ano, color = "caso_base"), size=1) +
  geom_line(data = df_en_media, aes(y=en_media,x=ano, color = "caso_medio"), size=1) +
theme_bid(tam_fonte=16) + ylab("Tarifa reajustada R$") + ylim(0, 7) +
scale_colour_manual(name="", values=c(caso_base="red", caso_medio = "blue")) +
  scale_x_continuous(breaks = c(2014, 2017, 2020, 2023))

p_en

ggsave(p_en, file="comp_energia.png", height = 8, width = 12, dpi = 300, type = "cairo", scale=.5)


# % do tempo que fica abaixo
df_sim_tr %>%
  group_by(sim) %>%
  mutate(y = sim_tr - df_base$base) %>%
  ungroup() %>%
  dplyr::filter(ano > 2019) %>%
  summarise(sum(y> 0)/n())

# diferença média de tarifa por ano
dif_tarif_ano <- df_sim_tr %>%
  group_by(sim) %>%
  mutate(y = sim_tr - df_base$base) %>%
  ungroup() %>%
  group_by(ano) %>%
  summarise(round(mean(y),4),
            round(max(y),4),
            round(min(y), 4))

# % dos anos que fica abaixo
df_sim_tr %>%
  group_by(sim) %>%
  mutate(y = sim_tr - df_base$base) %>%
  ungroup() %>%
  group_by(ano) %>%
  summarise(sum(y> 0)/n())

##########################
### Receita Tarifária
##########################
pax_tra <- 1000*c(0, 42558.608,	68940.169,	69629.572,	70325.868,	71029.127,
                  71739.416,	72456.812,	73181.38,	73913.192,	74652.325)

rt <- function(tr,  fpq = 1, igv) {
  # dados do excel Modelo PC Plano de Negócios VLT Carioca_v06
  pax_tra <- 1000*c(0, 42558.608,	68940.169,	69629.572,	70325.868,	71029.127,
               71739.416,	72456.812,	73181.38,	73913.192,	74652.325)
  tr*fpq*(1 + .3 - igv)*pax_tra
}

## calcular energia, igv cte.

df_sim1 <- df_sim_tr %>%
  group_by(sim) %>%
  mutate(rt_energia = rt(tr = sim_tr, igv=.3))

rt_tr_base <- rt(tr = df_base$base, igv=.3)

# analytics
# impacto receita tarifaria com caso base (com e sem energia no contrato)
impacto_receita <- df_sim1 %>%
  group_by(sim) %>%
  mutate(y = rt_energia - rt_tr_base) %>%
  ungroup() %>%
  group_by(ano) %>%
  summarise(mean(y),
            mediana = median(y),
            perc_25 = quantile(y, .025),
            perc_975 = quantile(y, .0975))


# valor presente
# selic over
# https://www3.bcb.gov.br/expectativas/publico/?wicket:interface=:1::::
selic <- c(1/1.11, 1/1.1363, 1/1.1416, 1/1.0984, 1, 1.0661, 1.08, 1.08, 1.08, 1.08, 1.08)

# seleci real
ipca_14_24 <- inflacao$ipca_anual[16:20]
ipca_14_24[6:10] <- df_infla$ipca_e[7:11]
ipca_14_24[11] <- ipca_14_24[10] # 2024 repeti 2023, pra facilitar.
ipca_14_24 <- 1 + ipca_14_24
selec_sem_infla <- c(1.11, 1.1363, 1.1416, 1.0984, 1, 1.0661, 1.08, 1.08, 1.08, 1.08, 1.08)/ipca_14_24
selec_sem_infla[1:4] <- 1/selec_sem_infla[1:4]
selec_sem_infla[5] <- 1

impacto_receita %>%
  ungroup() %>%
  mutate(selic = selic,
         mediana_vp = mediana/selic,
         perc_25_vp = perc_25/selic,
         perc_975_vp = perc_975/selic) %>%
  summarise(sum(mediana_vp),
            sum(perc_25_vp),
            sum(perc_975_vp))


# quantos porcento é positivo
df_sim1 %>%
  group_by(sim) %>%
  mutate(y = rt_energia - rt_tr_base) %>%
  mutate(y_cte = y/selic) %>%
  group_by(sim) %>%
  summarise(total_vp = sum(y_cte)) %>%
  ungroup() %>%
  summarise(mediana = median(total_vp),
            perc_025 = quantile(total_vp, .025),
            perc_975 = quantile(total_vp, .975),
            perc_pos = sum(total_vp>0)/n())


### Simulação receita tarifária
lista_rt <- list()
lista_tr <- list()
lista_df_sim <- list()

## amostra um vetor de tr no j
  for ( j in 1:100) {
    teste_tr <- df_sim1 %>%
      dplyr::filter(sim == j)
    lista_rt[[j]] <- rt(tr = teste_tr$sim_tr, igv=.3)
    lista_tr[[j]] <- teste_tr$sim_tr
  }
df_sim_rt <- data.frame(sim_rt = unlist(lista_rt),
                                  tr_input = unlist(lista_tr),
                                  ano = rep(ano,100))

glimpse(df_sim_rt)

# gráfico da simulação de receita tarifária
p_rt_complex <- df_sim_rt %>%
  ggplot(aes(tr_input , sim_rt)) + geom_point(alpha=.3) +
  facet_wrap (~ ano ) +
  theme_bid() + ylab("Receita Tarifária") +
  scale_y_continuous(labels = scales::comma_format(
    prefix = "R$ ",big.mark = ".", decimal.mark = ","))

p_rt_complex <- p_rt_complex + ggtitle("Simulação de receita tarifária por ano  e tr")

p_rt_complex

setwd("C:/Users/mczfe/Pessoal/Freela/BID PPP")
ggsave(p_rt_complex, file="p_rt_complex_v2.png", height = 8, width = 12)



######################################
## Contraprestação Pecuniária
##################
# caso 0, sem reajuste de tarifa (a 1.98)
# caso 1, com reajuste apenas de ipca e inpc (energia cte)
# caso 2, com reajuste médio projetado
# caso 3, com distribuição do reajuste da tarifa

# caso 0
n_sim <- 1000
sensibilidade <- runif(n_sim, .5, .9)
demanda_real <- matrix(0, nrow=n_sim, ncol=11)
result <- as.data.frame(matrix(0, nrow=n_sim, ncol=11))
names(result) <- paste("ano", 2014:2024, sep="_")

for ( i in 1:n_sim) {
  demanda_real[i,] <- pax_tra*sensibilidade[i]
  result[i,] <- unlist(1.98*(pax_tra - demanda_real[i,])*.5)
}


result_final <- result %>%
  mutate(sim = 1:n_sim) %>%
  gather(key="ano", value="sim_contra", -sim) %>%
  group_by(sim) %>%
  mutate(y_cte = sim_contra/selec_sem_infla) %>%
  ungroup()

result_final %>%
  group_by(ano) %>%
  summarise(media_vp = mean(y_cte),
            perc_025 = quantile(y_cte, .025),
            perc_975 = quantile(y_cte, .975))

# caso 1

n_sim <- 1000
sensibilidade <- runif(n_sim, .5, .9)
demanda_real <- matrix(0, nrow=n_sim, ncol=11)
result <- as.data.frame(matrix(0, nrow=n_sim, ncol=11))
names(result) <- paste("ano", 2014:2024, sep="_")

for ( i in 1:n_sim) {
  demanda_real[i,] <- pax_tra*sensibilidade[i]
  result[i,] <- unlist(df_base$base*(pax_tra - demanda_real[i,])*.5)
}


result_final <- result %>%
  mutate(sim = 1:n_sim) %>%
  gather(key="ano", value="sim_contra", -sim) %>%
  group_by(sim) %>%
  mutate(y_cte = sim_contra/selic) %>%
  ungroup()

result_final %>%
  group_by(ano) %>%
  summarise(media_vp = mean(y_cte),
            perc_025 = quantile(y_cte, .025),
            perc_975 = quantile(y_cte, .975))
# caso 2
##

n_sim <- 1000
rt_sim <- df_sim_rt %>%
  mutate(sim = rep(1:100, each=11)) %>%
  dplyr::select(-sim_rt)%>%
  # dplyr::filter(ano > 2019) %>%
  spread(key=ano, value=tr_input)


sensibilidade <- runif(n_sim, .5, .9)
demanda_real <- matrix(0, nrow=n_sim, ncol=11)
result <- matrix(0, nrow=n_sim, ncol=11)
for ( i in 1:n_sim) {
  demanda_real[i,] <- pax_tra*sensibilidade[i]
  result[i,] <- unlist(df_base$base*(pax_tra - demanda_real[i,])*.5)
}

result <- as.data.frame(result)
names(result) <- paste("ano", 2014:2024, sep="_")
result_final <- result %>%
  mutate(sim = 1:n_sim) %>%
  gather(key="ano", value="sim_contra", -sim) %>%
  group_by(sim) %>%
  mutate(y_cte = sim_contra/selic) %>%
  ungroup()

result_final %>%
  group_by(ano) %>%
  summarise(media_vp = mean(y_cte),
            perc_025 = quantile(y_cte, .025),
            perc_975 = quantile(y_cte, .975))

# caso 3
## Com reajuste variando

n_sim <- 1000
rt_sim <- df_sim_rt %>%
  mutate(sim = rep(1:n_sim, each=11)) %>%
  dplyr::select(-sim_rt)%>%
  # dplyr::filter(ano > 2019) %>%
  spread(key=ano, value=tr_input)


sensibilidade <- runif(n_sim, .5, .9)
demanda_real <- matrix(0, nrow=n_sim, ncol=11)
result <- as.data.frame(matrix(0, nrow=n_sim, ncol=11))
names(result) <- paste("ano", 2014:2024, sep="_")
lista_df <- list()

for ( j in 1:100) {
  for ( i in 1:n_sim) {
    demanda_real[i,] <- pax_tra*sensibilidade[i]
    result[i,] <- unlist(rt_sim[j,-1]*(pax_tra - demanda_real[i,])*.5)
  }
  lista_df[[j]] <- result
  lista_df[[j]]$sim_tarifa <- j
  print(j)
}

lista_final <- list()

for ( i in 1:100) {
  lista_final[[i]] <- lista_df[[i]] %>%
    mutate(sim_cb = 1:n_sim) %>%
    dplyr::select(-sim_tarifa) %>%
    gather(key="ano", value="sim_contra", -sim_cb) %>%
    group_by(sim_cb) %>%
    mutate(y_cte = sim_contra/selic) %>%
    ungroup() %>%
    mutate(sim_tarifa = i)
  print(i)
}

# juntando tudo
result_final <- bind_rows(lista_final)

result_final %>%
  group_by(ano) %>%
  summarise(media_vp = mean(y_cte),
            perc_025 = quantile(y_cte, .025),
            perc_975 = quantile(y_cte, .975))


# dados de tarifa para conclusão
rt_sim %>%
  summarise(quantile(`2024`, .975),
            mean(`2024`),
            quantile(`2024`, .025))


