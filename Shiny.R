library(shiny)
library(httr)
library(jsonlite)
library(formattable)
library(dplyr)
library(rsconnect)
library(shinyjs)
library(DT)

dados <- data.frame(CNPJ = character(0), Nome = character(0), Email = character(0), Termos = logical(0), 
                    Score = numeric(0), Pontualidade = numeric(0), 
                    CCF = character(0), Processo = character(0), Credito_emergencial = numeric(0), inadimplencia = numeric(0))

####################### UI  #######################
ui <- fluidPage(
  tags$div(
    style = "background-color: #007AC3; padding: 18px;",
    tags$img(
      src = "https://antonellieventos.com.br/wp-content/uploads/2016/11/sebrae-logo-2.jpg",
      width = "150",
      height = "auto",
      style = "float: left; margin-right: 50px; margin-top: -20px;"  # Ajuste o valor do margin-right aqui
    ),
    tags$h1(
      style = "color: White; font-weight: bold;font-family: 'Serif', sans-serif;",
      "Orienta Score"
    )
  ),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(textInput("CNPJ", label = "CNPJ", placeholder = "Forneça apenas os números do CNPJ"),
                 useShinyjs(),
                 checkboxInput("Termos", label = HTML("Estou ciente que meus dados serão tratados conforme as diretrizes da <a href='https://minio-cpe.sebrae.com.br/documento/politica-privacidade.pdf' target='_blank' style='margin-left: 5px;'>Política de Privacidade</a> do SEBRAE"), FALSE),
                 conditionalPanel(
                   condition = "input.Termos == true",
                   textInput("Nome", "Nome", placeholder = "Nome completo" ),
                   textInput("Email", "E-mail", placeholder = "E-mail" )
                 ),
                 actionButton("search_btn", "Buscar")
                 
    ),
    mainPanel(
      uiOutput("intro_score"),
      uiOutput("score"),
      uiOutput("resumo_Score"),
      uiOutput("entenda"),
      uiOutput("titulo_PP"),
      uiOutput("intro_pp"),
      uiOutput("Dicas_PP"),
      uiOutput("titulo_Inadimplencia"),
      uiOutput("intro_inadimplencia"),
      uiOutput("Dicas_Inadimplencia"),
      uiOutput("titulo_ccf_Protesto"),
      uiOutput("intro_ccf_Protesto"),
      uiOutput("Dicas_ccf_Protesto"),
      uiOutput("titulo_C_Emergencial"),
      uiOutput("intro_C_emergencial"),
      uiOutput("Dicas_C_Emergencial"),
      downloadButton("download_data_btn", "Desenvolvido por DataLab - SEBRAE PR")
      #tags$a(href = "", id = "downloadLink", download = "data.csv", "Baixar DataFrame"),
      
    )
  )
)

####################### Server  #######################
server <- function(input, output, session) {

  observeEvent(input$search_btn, {
  
    #####Chamada API#####

    CNPJ <- gsub("[[:punct:]]", "", input$CNPJ)
    
    #API
    headers <- c(
      'Content-Type' = 'application/json; charset=UTF-8',
      'Authorization' = 'Basic NzUxMTA1ODUwMDAxMDBAZXNwOkkyTHhsMzJX',
      'Cookie' = '__cf_bm=ZCeA02fIlXUQczLMSV2AK8eWIcEZq78jubCthdmShgQ-1691522280-0-
      ARTESSUKvDE3a8moHgJ1HaBVcM3J475PubkIaTdfEUfNkNx5k6GmRwK64d9PmPrPCrlrtp5fyfiSLdp4QbgaaGc='
    )
    
    Report_a <- "{\n  \"ReportPJRequest\": {\n    \"Options\": {\n      \"IncludeBestInfo\":1,\n      
    \"IncludeCreditRiskIndicators\": 1,\n      \"IncludeCreditRiskData\": 1,\n      \"IncludeQuodScore\": 1\n    
    },\n    \"SearchBy\": {\n      \"CNPJs\": {\n        \"CNPJ\": [\n          \""
    
    Report_b <- "\"\n        ]\n      }\n    }\n  }\n}"
   
    body <- paste0(Report_a, CNPJ,Report_b )
    
    res <- POST(url = "https://api.quod.com.br/WsQuodPJ/ReportPj?ver_=1.4", body = body, add_headers(headers), config(http_version = 1.1) )
    data <- jsonlite::fromJSON(content(res, "text"))
    
    #####Score#####
    
    output$intro_score <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <br>
                                          Ter um bom score de crédito pode facilitar o acesso a crédito e melhores condições, enquanto um score mais 
                                          baixo pode resultar em dificuldades para obter crédito ou levar a condições menos favoráveis. 
                                          É recomendável que os consumidores acompanhem regularmente seu score de crédito e tomem medidas para 
                                          mantê-lo saudável, como pagar as dívidas em dia e manter um bom histórico de crédito.</p>"))

    score <- as.numeric(data$ReportPJResponseEx$Response$Records$ReportPJOutput$ScorePJ$Score)
    
    
    if (length(score) == 0) {
      color_Score <- "#007AC3"
      score = "N/A"
    } else if (score >= 875) {
      color_Score <- "ForestGreen"
    } else if (score >= 725) {
      color_Score <- "DarkOrange"
    }else {
      color_Score <- "Crimson"
    }
    
    output$score <- renderUI(HTML(paste0("<p style='color: ", color_Score, ";text-align: center;font-size: 25px;
                                         font-family: 'Serif', sans-serif;'>Seu Score é: ", score, "</p>")))
    
    
    output$resumo_Score <- renderUI({
      if (score == "N/A") {
        HTML("<p style='font-size: 15px; font-family: \"Serif\", sans-serif; border: 2px solid #007AC3; border-radius: 10px; padding: 10px;'>
        Score não calculado.
             Ainda não temos dados sobre seu negócio, tente novamente em alguns dias </p>")
        
      } else if (score >= 875) {
        HTML("<p style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid ForestGreen; border-radius: 10px; padding: 10px;'> 
        Ter um score de crédito alto abre portas para oportunidades de financiamento favoráveis, mas também fortalece as 
        parcerias comerciais e aumenta a confiança de fornecedores e clientes. </p>")
        
      } else if (score >= 725) {
        HTML("<p style='font-size: 15px; font-family: \"Serif\", sans-serif; border: 2px solid DarkOrange; border-radius: 10px; padding: 10px;'> 
        Gostaríamos de parabenizar sua empresa pelo trabalho e dedicação em busca de um bom desempenho financeiro. 
        <br>
        Score Médio.</p>")
      } else if (score >= 0) {
        HTML("<p style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid Crimson; border-radius: 10px; padding: 10px;'> 
        Gostaríamos de parabenizar sua empresa pelo trabalho e dedicação em busca de um bom desempenho financeiro. 
        <br>
        Score baixo.</p>")
        
      } else {
        HTML("<p style='font-size: 15px; font-family: \"Serif\", sans-serif; border: 2px solid #007AC3; border-radius: 10px; padding: 10px;'>
        Score não calculado.
             Ainda não temos dados sobre seu negócio, tente novamente em alguns dias </p>")
        
      }
    })
    
    
    score_na_base <<- score
    
    output$entenda <- renderUI(HTML("<p style='color:#007AC3 ;font-size: 25px;
                                         font-family: 'Serif', sans-serif;'> O que afeta seu Score? </p>"))
    
    #####Pontualidade de Pagamento#####
    
    output$titulo_PP <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <strong>
                                     Pontualidade de Pagamento </strong> </p>"))
    
    
    output$intro_pp <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <br>
                                     A pontualidade de pagamento é um fator de extrema importância para obter crédito de 
                                     forma favorável. Ela se refere à capacidade de pagar as obrigações financeiras dentro 
                                     dos prazos estabelecidos pelas instituições credoras.</p>"))
    
    index <- which(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Indicators$Indicators$indicator[[1]]$VariableName == "PP_AV_GRV02")
    
    Pontualidade_Pag <- as.numeric(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Indicators$Indicators$indicator[[1]]$Value[index])
    
    if (length(Pontualidade_Pag) == 0) {
      color_PP <- "#007AC3"
      Pontualidade_Pag = "N/A"
    } else if (Pontualidade_Pag <= 73) {
      color_PP <- "Crimson"
    } else if (Pontualidade_Pag <= 88) {
      color_PP <- "DarkOrange"
    } else if (Pontualidade_Pag <= 100) {
      color_PP <- "ForestGreen"
    } else {
      color_PP <- "#007AC3"
    }
    
    output$Dicas_PP <- renderUI({
      if (Pontualidade_Pag <= 73) {
        
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid Crimson; border-radius: 10px; padding: 10px;'> 
        Volume baixo de compromissos financeiros pagos em dia. Para te ajudar, reservamos algumas dicas para você:
        <ul> 
           <br> <li> Organize seu orçamento: Faça um planejamento financeiro detalhado para entender suas despesas, receitas e prazos de pagamento. Certifique-se de reservar fundos suficientes para cobrir todas as contas.</li>
          <li> Priorize os pagamentos: Identifique as contas mais importantes e priorize-as para evitar atrasos e possíveis penalidades. Despesas essenciais, como aluguel, hipoteca, serviços públicos e pagamentos de empréstimos, devem ser priorizadas.</li>
          <li> Crie um cronograma de pagamentos: Faça um registro ou utilize aplicativos e ferramentas de gerenciamento financeiro para acompanhar as datas de vencimento e crie um cronograma de pagamentos. Defina lembretes para garantir que você não esqueça de efetuar os pagamentos.</li>
          <li> Automatize seus pagamentos: Considere configurar pagamentos automáticos para contas recorrentes. Entre em contato com seus credores para estabelecer débito automático, se possível. Isso ajudará a garantir que os pagamentos sejam feitos pontualmente, evitando atrasos por esquecimento.</li> 
        </ul>
      </div>") 
        
      } else if (Pontualidade_Pag <= 88) {
        HTML(" <div style='font-size: 15px; font-family: \"Serif\", sans-serif; border: 2px solid DarkOrange; border-radius: 10px; padding: 10px;'> 
        Volume médio de compromissos financeiros pagos em dia. Para te ajudar reservamos algumas dicas para você:
       <ul> 
         <br>  <li> Organize seu orçamento: Faça um planejamento financeiro detalhado para entender suas despesas, receitas e prazos de pagamento. Certifique-se de reservar fundos suficientes para cobrir todas as contas.</li>
          <li> Priorize os pagamentos: Identifique as contas mais importantes e priorize-as para evitar atrasos e possíveis penalidades. Despesas essenciais, como aluguel, hipoteca, serviços públicos e pagamentos de empréstimos, devem ser priorizadas.</li>
          <li> Crie um cronograma de pagamentos: Faça um registro ou utilize aplicativos e ferramentas de gerenciamento financeiro para acompanhar as datas de vencimento e crie um cronograma de pagamentos. Defina lembretes para garantir que você não esqueça de efetuar os pagamentos.</li>
          <li>Automatize seus pagamentos: Considere configurar pagamentos automáticos para contas recorrentes. Entre em contato com seus credores para estabelecer débito automático, se possível. Isso ajudará a garantir que os pagamentos sejam feitos pontualmente, evitando atrasos por esquecimento.</li> 
        </ul> </div>")
        
      } else if (Pontualidade_Pag <= 100) {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid ForestGreen; border-radius: 10px; padding: 10px;'> 
            Volume alto de compromissos financeiros pagos em dia.
            <br>
             Manter-se em dia com os pagamentos é uma conquista significativa, pois reflete uma gestão financeira sólida e um compromisso com a transparência e a ética nos negócios.</div>")
        
      } else {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid #007AC3; border-radius: 10px; padding: 10px;'>
             Ainda não temos dados sobre seu negócio, tente novamente em alguns dias.</div>")
      }
      
    })
    
    #####Inadimplência Comunicada#####
    
    output$titulo_Inadimplencia <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <strong> <br>
                                    Inadimplência Comunicada </strong> </p>"))
    
    output$intro_inadimplencia <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <br>
                                                A inadimplência comunicada é registrada nos bancos de dados das agências de proteção ao crédito, o que pode 
                                                afetar o score de crédito do indivíduo ou empresa. Um histórico de inadimplência tende a ter um impacto 
                                                negativo no score, dificultando a obtenção de crédito no futuro ou resultando em condições menos favoráveis. </p>"))
    
    
    if ("ApontamentoStatus" %in% colnames(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$Apontamentos$Apontamento[[1]])) {
      apontamentos <- data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$Apontamentos$Apontamento[[1]]
      if ("A" %in% apontamentos$ApontamentoStatus) {
        inadimplencia <- 1
      } else {
        inadimplencia <- 0
      }
    } else {
      inadimplencia <- 0
    }
    
    output$Dicas_Inadimplencia <- renderText(
      if ("ApontamentoStatus" %in% colnames(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$Apontamentos$Apontamento[[1]])) {
        apontamentos <- data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$Apontamentos$Apontamento[[1]]
        if ("A" %in% apontamentos$ApontamentoStatus) {
          HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid Crimson; border-radius: 10px; padding: 10px;'> 
        Há inadimplências.
              Para sair dessa situação, é fundamental procurar as empresas com as quais você possui algum débito para renegociar a dívida e, então, retirar o seu nome das listas de inadimplentes.</div>")
          
        } else {
          HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid ForestGreen; border-radius: 10px; padding: 10px;'> 
        Não há inadimplências comunicadas.
                Ao evitar a inadimplência, vocês fortalecem sua reputação como um parceiro confiável. 
                Continuem a priorizar a saúde financeira da sua empresa e a adotar práticas integras de gestão. Essa abordagem responsável certamente continuará trazendo resultados positivos e fortalecerá ainda mais sua posição no mercado.</div>")
          
        }
      } else {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid #007AC3; border-radius: 10px; padding: 10px;'> 
        Ainda não temos dados sobre seu negócio, tente novamente em alguns dias.</div> ")
        
      })
    
    #####Pendências financeiras (Cheque sem fundo e Protestos)#####
    output$titulo_ccf_Protesto <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <strong> <br>
                                    Pendências Financeiras </strong> </p>"))
    
    output$intro_ccf_Protesto <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <br>
                                                Pendências financeiras referem-se a dívidas ou obrigações não cumpridas por um indivíduo ou uma empresa. 
                                                Essas pendências podem ser de diversos tipos, incluindo empréstimos não pagos, contas atrasadas, faturas vencidas,
                                                cheques sem fundos, cartões de crédito não quitados, entre outros </p>"))
   
     if (!is.null(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$CcfApontamento) ||
        !is.null(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$LawSuitApontamento)) {
       CcfApontamento <- 1
      processo <- 1
    } else {
      CcfApontamento <- 0
      processo <- 0
    }
    
    output$Dicas_ccf_Protesto <- renderText(
      if (!is.null(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$CcfApontamento) ||
          !is.null(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Negative$LawSuitApontamento)) {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid Crimson; border-radius: 10px; padding: 10px;'>
        Há pendências financeiras.
                Faça uma avaliação completa da sua situação financeira. Liste todas as suas dívidas, identifique os valores em aberto, os prazos de pagamento e os juros envolvidos. Ter uma visão clara de todas as suas pendências ajudará a priorizar e planejar a resolução delas.</div>")
           
      } else {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid ForestGreen; border-radius: 10px; padding: 10px;'>
        Não há pendências financeiras.
                Essa conquista demonstra sua capacidade de gerenciar os recursos de forma prudente e tomar decisões financeiras responsáveis.</div>")
      })
    
    #####Contratações crédito emergencial#####
    
    output$titulo_C_Emergencial <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <strong> <br>
                                    Contratações crédito emergencial </strong> </p>"))
    
    output$intro_C_emergencial <- renderUI(HTML("<p style='font-size: 15px;font-family: 'Serif', sans-serif;'> <br>
                                                O uso de crédito emergencial refere-se à obtenção de recursos financeiros de forma rápida e temporária para 
                                                lidar com emergências ou imprevistos financeiros. Esse tipo de crédito é geralmente utilizado quando uma pessoa
                                                ou empresa precisa de dinheiro rapidamente para cobrir despesas urgentes ou inesperadas, como despesas médicas,
                                                reparos emergenciais, pagamentos imediatos de dívidas, entre outros. </p>"))
    
    index <- which(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Indicators$Indicators$indicator[[1]]$VariableName == "PF_EM_ITS02")
    
    C_Emergencial <- as.numeric(data$ReportPJResponseEx$Response$Records$ReportPJOutput$Indicators$Indicators$indicator[[1]]$Value[index])
    
    if (length(C_Emergencial) == 0) {
      color_C_Emergencial <- "#007AC3"
      C_Emergencial = "N/A"
    } else if (C_Emergencial <= 73) {
      color_C_Emergencial <- "ForestGreen"
    } else if (C_Emergencial <= 88) {
      color_C_Emergencial <- "DarkOrange"
    } else if (C_Emergencial <= 100) {
      color_C_Emergencial <- "Crimson"
    } else {
      color_C_Emergencial <- "#007AC3"
    }
    
    
  
    output$Dicas_C_Emergencial <- renderText({
      if (C_Emergencial <= 73) {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid ForestGreen; border-radius: 10px; padding: 10px;'>
        Valor baixo de crédito emergencial.
        <br>
          A falta de uso de crédito emergencial reflete o compromisso e a capacidade da sua empresa em lidar com desafios financeiros de forma prudente e sustentável. Essa conquista demonstra sua habilidade em manter uma posição financeira estável e evitar o endividamento excessivo.</div>")
        
      } else if (C_Emergencial <= 88) {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid DarkOrange; border-radius: 10px; padding: 10px;'>
          Valor médio de crédito emergencial.
          <br>
          Para te ajudar reservamos algumas dicas para você: 
      <ul><br>
          <li>	Construa um fundo de emergência: Economize regularmente e reserve uma parte do seu orçamento para criar uma reserva financeira destinada a cobrir despesas imprevistas. Ter um fundo de emergência pode ajudar a evitar a necessidade de recorrer a créditos em momentos de crise.</li>
          <li>	Planeje seu orçamento: Elabore um plano de gastos realista com base em sua renda e despesas. Acompanhe suas receitas e despesas mensais para garantir que você esteja vivendo dentro de suas possibilidades financeiras.</li>
          <li>	Estabeleça prioridades financeiras: Determine quais são as suas despesas essenciais e priorize-as. Isso inclui itens como moradia, alimentação, serviços públicos e saúde. Ao dar prioridade a esses gastos essenciais, você pode evitar a necessidade de buscar créditos emergenciais para cobri-los.</li>
          <li>	Evite dívidas desnecessárias: Tente evitar o acúmulo de dívidas não essenciais. Avalie cuidadosamente antes de assumir qualquer empréstimo ou financiamento e pergunte-se se é realmente necessário e se você tem a capacidade de pagar.</li>
          <li>	Esteja preparado para imprevistos: Além de um fundo de emergência, tenha um plano para lidar com situações inesperadas. Isso pode incluir ter um seguro adequado para proteger seus bens e sua saúde, bem como manter um bom histórico de crédito para ter acesso a melhores condições caso precise de crédito no futuro.</li>
          <li>	Busque alternativas de renda: Considere maneiras adicionais de gerar renda, como um trabalho paralelo ou freelancer. Ter uma fonte adicional de renda pode ajudar a cobrir despesas extras e reduzir a necessidade de recorrer a créditos emergenciais.</li>
          <li>	Eduque-se sobre finanças pessoais: Invista tempo em aprender sobre educação financeira, incluindo conceitos como orçamento, planejamento financeiro, investimentos e gerenciamento de dívidas. Quanto mais conhecimento você tiver sobre finanças pessoais, melhor preparado estará para tomar decisões financeiras informadas e evitar situações de emergência.</li>
      </ul>
         </div>")
        
      } else if (C_Emergencial <= 100) {
        HTML( "<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid Crimson; border-radius: 10px; padding: 10px;'>
        Valor elevado de crédito emergencial. 
       <br>
          Para te ajudar reservamos algumas dicas para você:
      <ul><br>
          <li>	Construa um fundo de emergência: Economize regularmente e reserve uma parte do seu orçamento para criar uma reserva financeira destinada a cobrir despesas imprevistas. Ter um fundo de emergência pode ajudar a evitar a necessidade de recorrer a créditos em momentos de crise.</li>
          <li>	Planeje seu orçamento: Elabore um plano de gastos realista com base em sua renda e despesas. Acompanhe suas receitas e despesas mensais para garantir que você esteja vivendo dentro de suas possibilidades financeiras.</li>
          <li>	Estabeleça prioridades financeiras: Determine quais são as suas despesas essenciais e priorize-as. Isso inclui itens como moradia, alimentação, serviços públicos e saúde. Ao dar prioridade a esses gastos essenciais, você pode evitar a necessidade de buscar créditos emergenciais para cobri-los.</li>
          <li>	Evite dívidas desnecessárias: Tente evitar o acúmulo de dívidas não essenciais. Avalie cuidadosamente antes de assumir qualquer empréstimo ou financiamento e pergunte-se se é realmente necessário e se você tem a capacidade de pagar.</li>
          <li>	Esteja preparado para imprevistos: Além de um fundo de emergência, tenha um plano para lidar com situações inesperadas. Isso pode incluir ter um seguro adequado para proteger seus bens e sua saúde, bem como manter um bom histórico de crédito para ter acesso a melhores condições caso precise de crédito no futuro.</li>
          <li>	Busque alternativas de renda: Considere maneiras adicionais de gerar renda, como um trabalho paralelo ou freelancer. Ter uma fonte adicional de renda pode ajudar a cobrir despesas extras e reduzir a necessidade de recorrer a créditos emergenciais.</li>
          <li>	Eduque-se sobre finanças pessoais: Invista tempo em aprender sobre educação financeira, incluindo conceitos como orçamento, planejamento financeiro, investimentos e gerenciamento de dívidas. Quanto mais conhecimento você tiver sobre finanças pessoais, melhor preparado estará para tomar decisões financeiras informadas e evitar situações de emergência.</li>
      </ul>
          </div>")
      } else {
        HTML("<div style='font-size: 15px; font-family: \"Serif\", sans-serif;border: 2px solid #007AC3; border-radius: 10px; padding: 10px;'>
        Ainda não temos dados sobre seu negócio, tente novamente em alguns dias.</div>")
      }
    })
    
    
    #####Montagem base de Dados#####
    # Adicione o score ao dataframe
    nova_linha <- data.frame(CNPJ = input$CNPJ, Nome = input$Nome, Email = input$Email, Termos = input$Termos, Score = score, Pontualidade = Pontualidade_Pag, CCF = CcfApontamento, Processo = processo, Credito_emergencial = C_Emergencial, inadimplencia = inadimplencia)
    dados <<- rbind(dados, nova_linha)  # Usamos <<- para modificar a variável global dados
    
    # Atualize a tabela DT com os dados atualizados
    output$score_table <- renderDT({
      datatable(dados)
    })
    
    #####Limpar campos#####
    updateCheckboxInput(session, "Termos", value = "")
    updateTextInput(session, "CNPJ", value = "")
    updateTextInput(session, "Nome", value = "")
    updateTextInput(session, "Email", value = "")
    
  })
  
  #####Botão Dowload#####
  
  output$download_data_btn <- downloadHandler(
  filename = function() {
    paste("dados_exportados.csv", sep = "")
    },
  content = function(file) {
    write.csv(dados, file)
    }
  )

}
####################### App  #######################
shinyApp(ui = ui, server = server)
