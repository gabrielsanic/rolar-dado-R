# Números aleatórios de 1 a 6
numero_dado <- sample(1:6, 1)

# Posição dos pontos do dados
get_ponto_dados <- function(number) {
  center <- (1 + sqrt(pi)) / 2
  offset <- 0.2  
  switch(
    as.character(number),
    "1" = data.frame(x = center, y = center),
    "2" = data.frame(x = c(center - offset, center + offset), 
                     y = c(center - offset, center + offset)),
    "3" = data.frame(x = c(center - offset, center, center + offset), 
                     y = c(center - offset, center, center + offset)),
    "4" = data.frame(x = rep(c(center - offset, center + offset), each = 2), 
                     y = rep(c(center - offset, center + offset), 2)),
    "5" = data.frame(x = c(center - offset, center, center + offset, 
                           center - offset, center + offset), 
                     y = c(center - offset, center, center + offset, 
                           center + offset, center - offset)),
    "6" = data.frame(x = rep(c(center - offset, center + offset), each = 3), 
                     y = c(center - offset, center, center + offset, 
                           center - offset, center, center + offset))
  )
}

# Função para jogar o jogo
jogo_dado <- function() {
  require(ggplot2)
  vitorias <- 0
  derrotas <- 0
  total_jogos <- 0  # Contador de jogos totais
  
  repeat {
    # Ler o número escolhido pelo usuário
    numero_escolhido <- as.numeric(readline(prompt = "Escolha um número entre 1 e 6: "))
    
    # Verificar se o número é válido
    if (is.na(numero_escolhido)) {
      print("Invalido! O valor deve ser um número inteiro.")
      next  # Volta para a próxima iteração do loop
    } else if (numero_escolhido != floor(numero_escolhido)) {
      print("Invalido! O valor deve ser um inteiro, não um decimal.")
      next  # Volta para a próxima iteração do loop
    } else if (numero_escolhido < 1 | numero_escolhido > 6) {
      print("Dado inválido! Escolha um número entre 1 e 6.")
      next  # Volta para a próxima iteração do loop
    } 
    
    # Número aleatório usando sample
    numero_dado <- sample(1:6, 1)
    
    result <- ifelse(numero_escolhido == numero_dado, "Você ganhou!", "Você perdeu!")
    
    # Atualizar contador de vitórias e derrotas
    if (numero_escolhido == numero_dado) {
      vitorias <- vitorias + 1
    } else {
      derrotas <- derrotas + 1
    }
    
    total_jogos <- total_jogos + 1  # Atualizar o número total de jogos
    
    # Calculando o winrate
    winrate <- (vitorias / total_jogos) * 100
    
    # Formatando o winrate colocando apenas duas casas decimais
    winrate_formatado <- sprintf("%.2f", winrate)
    
    title <- paste(result, 
                   "\nNumero escolhido:", numero_escolhido, 
                   "| Numero do dado:", numero_dado,
                   "\nVitórias:", vitorias, "| Derrotas:", derrotas, "| Winrate:", winrate_formatado, "%")
    
    pontos <- get_ponto_dados(numero_dado)
    
    # Plotar o dado
    p <- ggplot() + 
      geom_rect(aes(xmin = 1, xmax = sqrt(pi), ymin = 1, ymax = sqrt(pi)), fill = "white", color = "black") + 
      geom_point(data = pontos, aes(x = x, y = y), size = 20) +  
      coord_equal() +
      ggtitle(title) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))
    
    print(p)
    
    # Perguntar se o jogador quer continuar
    repeat {
      continuar <- readline(prompt = "Deseja continuar jogando? (s/n): ")
      
      if (tolower(continuar) == "s") {
        break  # Continua o jogo
      } else if (tolower(continuar) == "n") {
        print("Jogo encerrado.")
        return()  # Encerra o jogo
      } else {
        print("Entrada inválida! Por favor, digite 's' para continuar ou 'n' para encerrar.")
      }
    }
  }
}

jogo_dado()  # Iniciando jogo
