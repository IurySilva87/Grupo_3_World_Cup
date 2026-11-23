# Estatisticas basicas e analise exploratoria

source("code/01_read_data.R")

compute_basic_stats <- function(wc_data = load_world_cup_data("data/raw"),
                                save_results = TRUE,
                                out_dir = "results") {
  wc <- wc_data$worldcups
  m <- wc_data$wcmatches
  
  resumo_edicoes <- data.frame(
    edicoes = nrow(wc),
    ano_inicial = min(wc$year, na.rm = TRUE),
    ano_final = max(wc$year, na.rm = TRUE),
    total_jogos_edicoes = sum(wc$games, na.rm = TRUE),
    total_gols_edicoes = sum(wc$goals_scored, na.rm = TRUE),
    media_gols_edicao = mean(wc$goals_scored, na.rm = TRUE),
    mediana_gols_edicao = median(wc$goals_scored, na.rm = TRUE),
    dp_gols_edicao = sd(wc$goals_scored, na.rm = TRUE),
    total_publico = sum(wc$attendance, na.rm = TRUE),
    media_publico_edicao = mean(wc$attendance, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  m$total_gols_partida <- m$home_score + m$away_score
  resumo_partidas <- data.frame(
    total_partidas = nrow(m),
    media_gols_partida = mean(m$total_gols_partida, na.rm = TRUE),
    mediana_gols_partida = median(m$total_gols_partida, na.rm = TRUE),
    dp_gols_partida = sd(m$total_gols_partida, na.rm = TRUE),
    max_gols_partida = max(m$total_gols_partida, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  titulos_por_selecao <- as.data.frame(table(winner = wc$winner), stringsAsFactors = FALSE)
  titulos_por_selecao <- titulos_por_selecao[order(-titulos_por_selecao$Freq, titulos_por_selecao$winner), ]
  rownames(titulos_por_selecao) <- NULL
  
  desfechos_partidas <- as.data.frame(table(outcome = m$outcome), stringsAsFactors = FALSE)
  desfechos_partidas <- desfechos_partidas[order(-desfechos_partidas$Freq), ]
  rownames(desfechos_partidas) <- NULL
  
  if (isTRUE(save_results)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(resumo_edicoes, file.path(out_dir, "resumo_edicoes.csv"), row.names = FALSE)
    write.csv(resumo_partidas, file.path(out_dir, "resumo_partidas.csv"), row.names = FALSE)
    write.csv(titulos_por_selecao, file.path(out_dir, "titulos_por_selecao.csv"), row.names = FALSE)
    write.csv(desfechos_partidas, file.path(out_dir, "desfechos_partidas.csv"), row.names = FALSE)
  }
  
  list(
    resumo_edicoes = resumo_edicoes,
    resumo_partidas = resumo_partidas,
    titulos_por_selecao = titulos_por_selecao,
    desfechos_partidas = desfechos_partidas
  )
}