
library("shinydashboard")
library("shinycssloaders")
library("caret")
library("shiny")
library("readr")
library("shinythemes")
library("DT")
library("dplyr")
library("tidyverse")
library("plyr")
library("shinyjs")


# UI 

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}


shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css", 
             ".shiny-output-error { visibility: hidden; }",
            # ".shiny-output-error:before { visibility: hidden; }"
  ),
 
 tags$head(
   tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
 ),
  
  # load page layout
  dashboardPage(
    
    skin = "purple",
      
    dashboardHeader(title="NEO-DEER", titleWidth = 350),
    
    dashboardSidebar(width = 350,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<a target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo.svg' width = '186'></a>",
          "<br>",
          "<br>"
        )),
        
        menuItem("About", tabName = "About", icon = icon("home")),
        
        convertMenuItem(menuItem("Prediction of Medication Error", tabName = "class", icon = icon("network-wired"), ##tree
        
                                 numericInput("TND", "Total number of drugs", NA, step = 1),
                                 numericInput("PNA", "Postnatal age", NA, step = 1),
                                 numericInput("APGAR", "5-minute APGAR", NA, step = 1),
                                 numericInput("DWWH", "Doctor weekly working hours", NA, step = 1),
                                 numericInput("NWWH", "Nurse weekly working hours", NA, step = 1),
                                 numericInput("NMS", "Nurse monthly shifts", NA, step = 1),
                                 
                                 selectInput("JASU", label = "J.Antiinfectives for systemic use", #label = h3("Select box"), 
                                             choices = list("Amikacin" = 1, "Amoxicillin" = 2, "Ampicillin" = 3,
                                                            "Ampicillin+sulbactam"=4, "Azithromycin "=5, "Cefazolin"=6,
                                                            "Cefotaxime"=7, "Ceftriaxone"=8, "Cefuroxime"=9, 
                                                            "Ciprofloxacin"=10, "Colistin"=11, "Fluconazole"=12,
                                                            "Gentamicin"=13, "Imipenem"=14, "Linezolid"=15,
                                                            "Meropenem"=16, "Metronidazole"=17, "Ornidazole"=18,
                                                            "Oseltamivir"=19, "Penicillin G"=20, "Piperacillin-tazobactam"=21,
                                                            "Teicoplanin"=22, "Vancomycin"=23, "Others"=24, " "=25), selected = 25,
                                             multiple = TRUE),
                                 
                                 
                                 selectInput("NNS", label = "N.Nervous system", #label = h3("Select box"), 
                                             choices = list("Acetaminophen" = 1, "Caffeine" = 2,
                                                            "Dexmedetomidine"= 3, "Diazepam"= 4, "Fentanyl"= 5,
                                                            "Ketamine"= 6, "Levetiracetam"=7, "Midazolam"= 8, 
                                                            "Morphine"= 9, "Phenobarbital"= 10, "Phenytoin"=11, 
                                                            "Others"=12, " " = 13
                                             ), selected = 13, multiple = TRUE),
                                 
                                 selectInput("AATM", label = "A.Alimentary tract and metabolism", #label = h3("Select box"), 
                                             choices = list("Aluminum hydroxide" = 1, "Calcitriol" = 2, "Calcium gluconate" = 3,
                                                            "Calcium lactate"=4, "Carglumic acid"=5, "Carnitine"=6,
                                                            "Famotidine"=7, "IV Solutions"=8, "Lansoprazole"=9, 
                                                            "Magnesium carbonate"=10, "Magnessium sulphate"=11, "Multivitamins"=12,
                                                            "Pantoprazole"=13, "Potassium chloride"=14, "Potassium citrate"=15,
                                                            "Probiotics"=16, "Ranitidine"=17, "Shohl's solution"=18,
                                                            "Sodium benzoate"=19, "Sodium bicarbonate"=20, "Sodium chloride"=21,
                                                            "Ursodiol"=22, "Vitamin A"=23, "Vitamin B"=24,
                                                            "Vitamin B6 (Pyridoxine)"=25, "Vitamin C"=26, "Vitamin D3 (Cholecalciferol)"=27,
                                                            "Zinc supplements"=28, "Others"=29, " "=30), selected = 30,
                                             multiple = TRUE),
                                 
                                 selectInput("RRS", label = "R.Respiratory system", #label = h3("Select box"), 
                                             choices = list("Acetylcysteine" = 1, "Dornase alfa" = 2, "Ipratropium" = 3,
                                                            "Salbutamol (Albuterol)"=4, "Surfactant"=5, "Others"=6,
                                                            " "=7), selected = 7,
                                             multiple = TRUE),
                               
                                 HTML('<br>'),
                                 HTML("Note 1: All values must be entered to obtain"),
                                 HTML('<br>'),
                                 HTML("predictions."),
                                 HTML('<br>'),
                                 HTML('<br>'),
                                 HTML("Note 2: To obtain a new prediction, the "),
                                 HTML('<br>'),
                                 HTML("previous information must first be deleted"),
                                 HTML('<br>'),
                                 HTML("using the clear button."),
                                 HTML('<br>'),
                                 HTML('<br>'),
                                 shinyjs::useShinyjs(),
                                 
                                 shiny::actionButton(inputId = "Get1", label = "Get prediction"),
                                 actionButton("Update1", "Clear"),
                                 HTML('<br>')), tabName = "class"),
                                
      
        HTML(paste0(
          "<br><br><br><br><br><br><br><br><br>",
          "<table style='margin-left:auto; margin-right:auto;'>",
            
          "</table>",
          "<br>"),
        HTML(paste0(
          "<script>",
          "</script>",
          "<p style = 'text-align: center;'>  </p>")
        ))
      )
      
    ), 
    
    dashboardBody(
      
      tags$style(HTML(".main-sidebar {background-color: #d3d3d3 !important;}
                       .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #808080;}
                      .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #d3d3d3;
                              color: #000000;}
                      .content-wrapper, .right-side {
                              background-color: white;
                      ")),
      
                                
                                
      tabItems(
        
        tabItem(tabName = "About", 
          
                HTML('<p> <b>Tool:</b> NEO-DEER: A Web-Tool for Machine Learning-Based Medication Error Prediction </b>'),
                HTML('<p> <b>Version:</b> 1.0</b>'),
                HTML('<p> <b>Published:</b> 2022-01-06</b>'),
                HTML('<p> <b>Contributors:</b> Nadir Yalcin, PhD <br>
                &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Merve Kasikci, MSc <br>
                &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Hasan Tolga Celik, Assoc Prof, MD <br>
                &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Karel Allegaert, Prof, MD <br>
                &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Kutay Demirkan, Prof, PharmD <br>
                &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Sule Yigit, Prof, MD<br>
                &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp Murat Yurdakok, Prof, MD</b>'),
                HTML('<p> <b>Logo Designer:</b> Nuri Beydemir </b>'),
                HTML('<p> <b>Description:</b> Prospectively 11,908 medication orders from 412 NICU patients over 17 months were comprehensively analyzed by a pediatric clinical pharmacist. In 174 (42.2%) of these patients, at least one type of medication error (prescribing and monitoring process) originating from a physician was determined. Also, in 235 (57.0%) of these patients, at least one type of medication error (preparation and administration process) originating from a nurse was determined. A machine learning-based medication error prediction tool was developed with these data. The positive predictive value and AUC value are 0.944 and 0.920, respectively. It is estimated that medication errors can be prevented before they occur, with the use of this free, user-friendly, online, non-registered, and high-performance web-tool, which predicts medication errors in each patient admitted to the NICUs.'),
                HTML('<br>'),
                HTML('<br>'),
                
                tags$video( type = "video/mp4",src = "Drug.mp4", #controls = "controls",
                            autoplay=TRUE,
                            muted=TRUE,
                            playsinline=TRUE,
                            loop=TRUE,
                            controls=FALSE,                         
                            style = 'position: relative; left:300px; top: 0px; width: 50%;'
                )   
        ),
        


tabItem(tabName = "class", verbatimTextOutput("result_class2"),  verbatimTextOutput("warning1"), #deneme result_class2
        tags$head(tags$style(HTML("
                            #result_class2 {
                              font-size: 18px;
                              #color: white;
                            }
                            "))),
        
        
        
        tags$head(tags$style(HTML("
                           #warning1 {
                              font-size: 18px;
                               color: #ff0000;
                               font-weight: bold; 
                            }
                           "))),
        
        tags$image(src="Drug_error.png",  
                   width="100%",  height="30%", 
                   style = '
                           top: 150px; 
                           left:1050px;
                opacity: 0.25') 
        
        
)

)
)
)


))
