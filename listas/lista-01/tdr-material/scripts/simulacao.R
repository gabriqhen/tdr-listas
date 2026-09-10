#!/usr/bin/env Rscript

## Reamostragem bootstrap da média de uma coluna numérica de um CSV.
##
## Uso: Rscript simulacao.R <arquivo.csv> <coluna> [repeticoes]
##
## O andamento vai no stderr e o resultado no stdout. O código de saída
## é 1 quando os argumentos não servem e 0 quando a simulação termina.
##
## https://fernandomayer.github.io/tdr/02-cli-unix.html
## Distribuído sob a GPL-3.

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2 || length(args) > 3) {
    message("uso: Rscript simulacao.R <arquivo.csv> <coluna> [repeticoes]")
    quit(status = 1)
}

arquivo <- args[1]
coluna <- args[2]
repeticoes <- if (length(args) == 3) as.integer(args[3]) else 2000

## Verificar a dependência em vez de confiar nela, e dizer na mensagem o
## que instalar, é a recomendação do próprio capítulo
if (!requireNamespace("readr", quietly = TRUE)) {
    message("erro: o pacote readr não está instalado")
    message('instale com: install.packages("readr")')
    quit(status = 1)
}

if (!file.exists(arquivo)) {
    message("erro: arquivo '", arquivo, "' não encontrado")
    quit(status = 1)
}

message("Lendo ", arquivo)
dados <- readr::read_csv(arquivo, show_col_types = FALSE)

if (!coluna %in% names(dados)) {
    message("erro: a coluna '", coluna, "' não existe em ", arquivo)
    message("colunas disponíveis: ", paste(names(dados), collapse = ", "))
    quit(status = 1)
}

valores <- dados[[coluna]]
valores <- valores[!is.na(valores)]

if (!is.numeric(valores) || length(valores) < 2) {
    message("erro: '", coluna, "' não tem valores numéricos suficientes")
    quit(status = 1)
}

message(length(valores), " valores não faltantes em ", nrow(dados),
        " linhas")
message("Reamostrando ", repeticoes, " vezes")

## A semente fixa faz duas execuções darem o mesmo resultado. O laço
## explícito existe para que o andamento apareça enquanto a simulação
## roda, que é o que o exemplo de tee do capítulo mostra
set.seed(2026)
medias <- numeric(repeticoes)
for (i in seq_len(repeticoes)) {
    medias[i] <- mean(sample(valores, replace = TRUE))
    if (i %% 500 == 0) {
        message("  ", i, " de ", repeticoes)
    }
}

resultado <- c(media_obs = mean(valores),
               media_boot = mean(medias),
               erro_padrao = sd(medias),
               ic_inf = unname(quantile(medias, 0.025)),
               ic_sup = unname(quantile(medias, 0.975)))

message("Concluído")
print(round(resultado, 3))
