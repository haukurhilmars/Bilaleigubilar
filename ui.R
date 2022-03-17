library(shiny)
library(DT)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(readxl)
library(truncnorm)
library(lubridate)
library(plotly)
library(scales)

stadir_orig <- read_xlsx('stadir.xlsx')
bilar <- read_xlsx("bilar.xlsx")

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

ui <- fluidPage(#theme = shinytheme("cosmo"),
  titlePanel(div("Þarfa- og kostnaðargreining á hleðsluinnviðum",img(src="EFLA hreint.jpg", width=80, height=25) )),
  fluidRow(
    column(3,
           fluidRow(
             column(12,
                    selectInput("stadur", "Staður",
                                c("Þingvellir (Hakið)",
                                  "Jökulsárlón",
                                  "Seljalandsfoss",
                                  "Hraunfossar",
                                  #"Hvítserkur",
                                  "Dimmuborgir (Mývatn)",
                                  "Reynisfjara",
                                  "Keflavík"))
             )
           ),
           fluidRow(
             column(12,
                    sliderInput("hledsla", "Meðal afl á hvern bíl (kW)", min=10, max=150, value=20, step=10)
             )
           ),
           fluidRow(
             column(12,
                    sliderInput("fj_i_bil", "Meðalfjöldi ferðamanna í bíl", min=1, max=3, value=2.5, step=0.1)
             )
           ),
           fluidRow(
             column(12,
                    sliderInput("hlutf_a_bil",label="Hlutfall ferðamanna á einkabíl", min = 0, max = 1, value = 0.6, step=0.1)
             )
           ),
           fluidRow(
             column(12,
                    selectInput("hlutf", "Hlutfall bílaleigubíla sem eru rafbílar",
                                c("10 %" = 0.1,
                                  "30 %" = 0.3,
                                  "60 %" = 0.6))
             )
           ),
           fluidRow(
             column(12,
                    numericInput("hermanir", "Fjöldi hermdra daga", value=25, min = 0, max=250, step=25))
           ),
           fluidRow(
             column(12,
                    checkboxInput("hotel", "Uppbygging hleðsluinnviða við gististaði?", TRUE))
           ),
           fluidRow(
             column(12,
                    checkboxInput("styring", "Bílar teknir úr hleðslu um leið og þeir eru búnir að hlaða?", FALSE))
           ),
           fluidRow(
             column(12,
                    radioButtons("vidmid", "Viðmið þegar bílar eru teknir úr hleðslu", 
                                 choices =c("80%","100%"),
                                 selected = "100%"))
           ),
           
           # fluidRow(
           #   column(12,
           #          leafletOutput("kort", width=300, height=300)
           #   )
           # ),
           fluidRow(
             column(
               12,
               actionButton("ferdamann_takk", "Ferðamaður að handahófi", icon("plane"), 
                            style="color: #000000; background-color: #ffffff; border-color:  #ffffff"),
               verbatimTextOutput("ferdamadur_random")
             )
           ),
           
           
    ),
    column(7,
           fluidRow(
             column(
               12, 
               dataTableOutput("test")
             )
           ),
           fluidRow(
             column(
               12,
               plotOutput("mynd2", width= "800px", height = "600px")
             )
           ),
           fluidRow(
             column(
               4,
               verbatimTextOutput("hamark_bila")
             ),
             column(
               4,
               verbatimTextOutput("hamark_rafbila")
             ),
             column(
               4,
               verbatimTextOutput("hamark_hlada")
             )
           ),
           fluidRow(
             column(
               4,
               verbatimTextOutput("medal_bila")
             ),
             column(
               4,
               verbatimTextOutput("medal_rafbila")
             ),
             column(
               4,
               verbatimTextOutput("medal_hlada")
             )
           ),
           fluidRow(
             column(
               4,
               verbatimTextOutput("upph_rafh")
             ),
             column(
               4,
               verbatimTextOutput("eknir_km")
             ),
             column(
               4,
               verbatimTextOutput("stada_rafh")
             )
           ),
           fluidRow(
             column(
               4,
               verbatimTextOutput("likur_ad_hlada")
             ),
             column(
               4,
               verbatimTextOutput("timi_100")
             ),
             column(
               4,
               verbatimTextOutput("timi_stopp")
             )
           ),
           fluidRow(
             column(
               4,
               verbatimTextOutput("timi_hlada")
             ),
             column(
               4,
               verbatimTextOutput("kwh_hladid")
             ),
             column(
               4,
               verbatimTextOutput("stada_rafh_brottf")
             )
             
           )
           
    ),
    column(2,
           fluidRow(
             column(
               12,
               radioButtons("man", "Mánuður", choices=c("janúar", "febrúar", "mars", "apríl", "maí", "júní", "júlí", "ágúst", "september",
                                                        "október", "nóvember", "desember"), selected = "ágúst")
             )
           ),
           fluidRow(
             column(12,
                    uiOutput("kef_moguleikar")
             )
           ),
           
           fluidRow(
             column(12,
                    textOutput("fj_i_man"))
           ),
           fluidRow(
             column(12,
                    textOutput("fj_bila_a_dag"))
           ),
           
           fluidRow(
             column(12,
                    textOutput("fj_i_kef"))
           )
           
           
    )
  )
  
  
  
)


