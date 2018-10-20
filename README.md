# line4PPPsim
Este pacote contém funções para simular o impacto fiscal da linha 4 do metrô em São Paulo.

## Instalação
Para instalar, rode:
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::intall_github("mgaldino/line4PPPsim")

## Uso

library(line4PPPsim)

library(ggplot2)

resultado <- sim_sensibilidade_line4(n_sim = 100, num_years = 33,
                                    start_seed = 1, mu = 0, sd= .05,
                                    ajuste_inflacao = F,
                                    type="random_walk")
                                    
result_sim <- resultado[[1]]

result_sim %>%
  ggplot(aes(x=ano, y=diferenca, group = sim)) + geom_line() +
  ylab("diferen?a entre realizado e previsto") +
  scale_y_continuous(labels = scales::comma_format(prefix = "R$ ",big.mark = ".", decimal.mark = ","))


