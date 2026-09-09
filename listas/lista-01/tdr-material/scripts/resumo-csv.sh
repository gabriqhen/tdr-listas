#!/usr/bin/env bash
set -euo pipefail

## Resume os arquivos CSV de um diretório: nome, linhas e tamanho.
##
## Uso: ./resumo-csv.sh [diretório]
## Sem argumento, usa o diretório atual.
##
## https://fernandomayer.github.io/tdr/02-cli-unix.html
## Distribuído sob a GPL-3.

dir=${1:-.}

if [[ ! -d $dir ]]; then
    echo "erro: '$dir' não é um diretório" >&2
    exit 1
fi

## O laço não roda quando nenhum arquivo casa: sem o nullglob, o shell
## entregaria o próprio padrão "*.csv" como se fosse um nome de arquivo
shopt -s nullglob
arquivos=("$dir"/*.csv)

if [[ ${#arquivos[@]} -eq 0 ]]; then
    echo "nenhum .csv em '$dir'" >&2
    exit 1
fi

for arquivo in "${arquivos[@]}"; do
    linhas=$(wc -l < "$arquivo")
    tamanho=$(du -h "$arquivo" | cut -f1)
    printf "%-20s %5s linhas %8s\n" \
           "$(basename "$arquivo")" "$linhas" "$tamanho"
done
