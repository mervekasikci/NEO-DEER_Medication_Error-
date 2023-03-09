
set.seed(1234)
class_model <- readRDS("model.rds")


df <- dplyr::tibble(TND=NA, PNA=NA, APGAR=NA, DWWH=NA, NWWH=NA, NMS=NA, JASU='25', NNS='13', AATM = '30', RRS='7')

data <- read.csv("data.csv")

################
# SERVER 
################

shinyServer(function(input, output, session) { #session eklendi
   
  values <- reactiveValues()
  values$df <- data.frame()
  
  mod_df <- shiny::reactiveValues(x = df)
  
  output$table <- DT::renderDT({
    isolate(mod_df$x)
  })
  
  
  shiny::observeEvent(input$Get1, {   
    
    max_length <- max(length(input$JASU), length(input$NNS), length(input$AATM), length(input$RRS))
    
    
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(TND = input$TND, PNA = input$PNA, APGAR=input$APGAR, DWWH=input$DWWH, NWWH=input$NWWH, NMS=input$NMS,
                      JASU = c(input$JASU, rep(input$JASU[length(input$JASU)], times=(max_length-length(input$JASU)))), 
                      NNS = c(input$NNS, rep(input$NNS[length(input$NNS)], times=(max_length-length(input$NNS)))), 
                      AATM = c(input$AATM, rep(input$AATM[length(input$AATM)], times=(max_length-length(input$AATM)))), 
                      RRS = c(input$RRS, rep(input$RRS[length(input$RRS)], times=(max_length-length(input$RRS))))
        )
      )
    da <- data.frame(TND = input$TND, PNA = input$PNA, APGAR=input$APGAR, DWWH=input$DWWH, NWWH=input$NWWH, NMS=input$NMS,
                     JASU = c(input$JASU, rep(input$JASU[length(input$JASU)], times=(max_length-length(input$JASU)))), 
                     NNS = c(input$NNS, rep(input$NNS[length(input$NNS)], times=(max_length-length(input$NNS)))),
                     AATM = c(input$AATM, rep(input$AATM[length(input$AATM)], times=(max_length-length(input$AATM)))),
                     RRS = c(input$RRS, rep(input$RRS[length(input$RRS)], times=(max_length-length(input$RRS)))))
    values$df <- rbind(values$df, da)
  })

  
  proxy <- DT::dataTableProxy('table')
  shiny::observe({
    DT::replaceData(proxy, mod_df$x)
  })
  
 
  predictions_class<-reactive({
    req(input$Get1)
    input_data <- values$df
    
    colnames(input_data) = c("toplamilacsayisi", "postnatalgun", "APGAR2", 
                             "hekimhaftalikcalismasaati", "hemsirehaftalikcalismasaati", "hemsireayliknobetsayisi",
                             "J.Antiinfectives_for_systemic_use", "N.Nerveous_system",
                             "A.Alimentary_tract_and_metabolism", "R.Respiratory_system")
    
    input_data$toplamilacsayisi<-as.numeric(input_data$toplamilacsayisi)
    input_data$postnatalgun<-as.numeric(input_data$postnatalgun)
    input_data$APGAR2<-as.numeric(input_data$APGAR2)
    input_data$hekimhaftalikcalismasaati<-as.numeric(input_data$hekimhaftalikcalismasaati)
    input_data$hemsirehaftalikcalismasaati<-as.numeric(input_data$hemsirehaftalikcalismasaati)
    input_data$hemsireayliknobetsayisi<-as.numeric(input_data$hemsireayliknobetsayisi)
    
    JASU_var <- NNS_var <- AATM_var <- RRS_var <- NULL ####  
    
    if(input$JASU==25 & length(input$JASU) ==1) {
      JASU_var <- 0
    } else { 
      JASU_var <- 1
    }
    
    if(input$NNS==13 & length(input$NNS) ==1) {
      NNS_var <- 0
    } else { #"if(input$NNS!=13)
      NNS_var <- 1
    }
    
    if(input$AATM==30 & length(input$AATM) ==1) {
      AATM_var <- 0
    } else { 
      AATM_var <- 1
    }
    
    if(input$RRS==7 & length(input$RRS) ==1) {
      RRS_var <- 0
    } else { 
      RRS_var <- 1
    }
    
    test_data <- cbind(input_data$toplamilacsayisi, input_data$postnatalgun, input_data$APGAR2,
                       input_data$hekimhaftalikcalismasaati, input_data$hemsirehaftalikcalismasaati,
                       input_data$hemsireayliknobetsayisi,
                       JASU_var, NNS_var,  AATM_var, RRS_var)
    
    test_data <- as.data.frame(test_data)
    
    colnames(test_data) = c("toplamilacsayisi", "postnatalgun", "APGAR2", 
                            "hekimhaftalikcalismasaati", "hemsirehaftalikcalismasaati", "hemsireayliknobetsayisi",
                            "J.Antiinfectives_for_systemic_use", "N.Nerveous_system",
                            "A.Alimentary_tract_and_metabolism", "R.Respiratory_system")
    
    
    TND_scale <- c(scale(append(data$toplamilacsayisi, input$TND)))
    test_data$toplamilacsayisi <- TND_scale[length(TND_scale)]
    
    PNA_scale <- c(scale(append(data$postnatalgun, input$PNA)))
    test_data$postnatalgun <- PNA_scale[length(PNA_scale)]
    
    APGAR_scale <- c(scale(append(data$APGAR2, input$APGAR)))
    test_data$APGAR2 <- APGAR_scale[length(APGAR_scale)]
    
    DWWH_scale <- c(scale(append(data$hekimhaftalikcalismasaati, input$DWWH)))
    test_data$hekimhaftalikcalismasaati <- DWWH_scale[length(DWWH_scale)]
    
    NWWH_scale <- c(scale(append(data$hemsirehaftalikcalismasaati, input$NWWH)))
    test_data$hemsirehaftalikcalismasaati <- NWWH_scale[length(NWWH_scale)]
    
    NMS_scale <- c(scale(append(data$hemsireayliknobetsayisi, input$NMS)))
    test_data$hemsireayliknobetsayisi <- NMS_scale[length(NMS_scale)]
    
   
    return(list(input_data, test_data))
  })
  

  
  output$deneme <- renderPrint({
 
    str(predictions_class()[[2]])  
    Prediction = predict(class_model, predictions_class()[[2]])
    Prediction
  })
  
  
  output$result_class2 <- renderPrint({
    
    req(input$Get1)
    set.seed(1234)
    Prediction = predict(class_model, predictions_class()[[2]])
    Pred_new <- Prediction

    if(Pred_new == "0"){
      Pred_new <- revalue(Pred_new, c("0"= "According to the prediction from the medication error model, this patient will have no medication error.", 
                                      "1"= "According to the prediction from the medication error model, this patient will have medication error."))
      
      Pred_new <- as.data.frame(Pred_new)
      names(Pred_new) <- ""
      
      a <- data.frame(factor(Pred_new[nrow(Pred_new),]))
      names(a) <- ""
      cat(apply(a, 1, function(x){cat(x); cat("\n")}))
    } else { cat ("")}
  })
 
  output$warning1 <- renderPrint({
    req(input$Get1)
    set.seed(1234)
    Prediction = predict(class_model, predictions_class()[[2]])
    Pred_new <- Prediction
    if(Pred_new == "1"){
      cat("According to the prediction from the medication error model, this patient will have medication error.")
    }
  })
  
  observeEvent(input$Update1, {
    shinyjs::hide("result_class2")
    shinyjs::hide("warning1")
    
    updateNumericInput(session, "TND", value= "")
    updateNumericInput(session, "PNA", value= "")
    updateNumericInput(session, "APGAR", value= "")
    updateNumericInput(session, "DWWH", value= "")
    updateNumericInput(session, "NWWH", value= "")
    updateNumericInput(session, "NMS", value= "")
    
    updateSelectInput(session, "JASU", selected = 25)
    updateSelectInput(session, "NNS", selected = 13)
    updateSelectInput(session, "AATM", selected = 30)
    updateSelectInput(session, "RRS", selected = 7)
  })
  
  
  
  observeEvent(input$Get1, {
    shinyjs::show("result_class2")
    shinyjs::show("warning1")
  })
  
  
})
