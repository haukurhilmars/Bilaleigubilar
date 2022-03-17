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

server <- function(input, output, session){
  
  fj_monte_carlo <- reactive({
    input$hermanir
  })
  
  #Input fangað 
  stadur <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(stadur)
    
    a[[1]]
  })
  
  km_std1 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(std_1_km)
    
    a[[1]]
  })
  
  km_std2 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(std_2_km)
    
    a[[1]]
  })
  
  km_std3 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(std_3_km)
    
    a[[1]]
  })
  
  hlutf_std1 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(hltf_1)
    
    a[[1]]
  })
  
  hlutf_std2 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(hltf_2)
    
    a[[1]]
  })
  
  hlutf_std3 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(hltf_3)
    
    a[[1]]
  })
  
  latid<- reactive({
    b <- filter(stadir_orig, stadur == input$stadur)%>%
      select(lat)
    
    b[[1]]
  })  
  
  longtd<- reactive({
    c <- filter(stadir_orig, stadur == input$stadur)%>%
      select(long)
    
    c[[1]]
  })
  
  stadar_df <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)
    
    a
  }) 
  
  
  hledsla_kw <- reactive({as.numeric(input$hledsla)})
  
  fj_per_bil <- reactive({as.numeric(input$fj_i_bil)})
  
  hlutf_a_bil <- reactive({as.numeric(input$hlutf_a_bil)})
  
  hlutf_rafbilar <- reactive({as.numeric(input$hlutf)})
  
  ferdam_sumar <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(avrg_sum)
    
    a[[1]]
  }) 
  
  ferdam_vetur <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(avrg_vet)
    
    a[[1]]
  })  
  
  manudur <- reactive({input$man})
  
  #Náum í gildi fyrir hvern mánuð
  
  jan <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(jan)
    
    a[[1]]
  })
  feb <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(feb)
    
    a[[1]]
  })
  mar <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(mar)
    
    a[[1]]
  })
  apr <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(apr)
    
    a[[1]]
  })
  mai <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(mai)
    
    a[[1]]
  })
  jun <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(jun)
    
    a[[1]]
  })
  jul <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(jul)
    
    a[[1]]
  })
  aug <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(aug)
    
    a[[1]]
  })
  sep <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(sep)
    
    a[[1]]
  })
  oct <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(oct)
    
    a[[1]]
  })
  nov <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(nov)
    
    a[[1]]
  })
  dec <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(dec)
    
    a[[1]]
  }) 
  
  
  manadar_fjoldi <- reactive({
    a <- tibble(
      "janúar" = jan(),
      "febrúar" = feb(),
      "mars" = mar(),
      "apríl" = apr(),
      "maí" = mai(),
      "júní" = jun(),
      "júlí" = jul(),
      "ágúst" = aug(),
      "september" = sep(),
      "október" = oct(),
      "nóvember" = nov(),
      "desember" = dec(),
    )%>%
      select(manudur())
    
    a[[1]]
  })
  
  #Fjöldi og dreifing
  fj_dag <- reactive({
    manadar_fjoldi()/30
  })
  
  toppur_1 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(toppur1)
    
    a[[1]]
  })
  toppur_2 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(toppur2)
    
    a[[1]]
  })
  
  breidd_1 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(breidd_topps1)
    
    a[[1]]
  })
  
  breidd_2 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(breidd_topps2)
    
    a[[1]]
  })
  
  hlutf_1 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(hlutf_toppur1)
    
    a[[1]]/100
  })
  
  hlutf_2 <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(hlutf_toppur2)
    
    a[[1]]/100
  })
  
  opn_timi <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(opntimi)
    
    a[[1]]
  })  
  lokunartimi <- reactive({
    a <- filter(stadir_orig, stadur == input$stadur)%>%
      select(opntimi)
    b <- filter(stadir_orig, stadur == input$stadur)%>%
      select(opnklst)
    
    a[[1]]+b[[1]]
  })
  opn_klst <- reactive({
    c <- filter(stadir_orig, stadur == input$stadur)%>%
      select(opnklst)
    
    c[[1]]
    
  })
  jafn_dr_fjoldi <- reactive({
    a <- manadar_fjoldi()
    
    c <- filter(stadir_orig, stadur == input$stadur)%>%
      select(opnklst)
    
    a[[1]]/30/c[[1]]
    
  })
  
  
  
  #Kort
  # output$kort <- renderLeaflet({
  #   leaflet() %>%
  #     setView(lng = -18.5, lat = 64.6, zoom = 5.4875)%>%
  #     addProviderTiles(providers$CartoDB.Positron)%>%
  #     addMarkers(longtd(),  latid(), label =stadur())
  # })
  
  
  
  
  # Dálkur 2
  
  ### Líkinda Mess
  #Miðum við fjölda bíla á dag út frá fjölda ferðamanna og hversu margir eru á einkabíl
  fj_bila_dag <- reactive({
    round(fj_dag()*hlutf_a_bil()/fj_per_bil(),0)
  })
  
  
  
  #Drögum tímasetningar bíla út frá toppi og breidd
  timasetningar <- reactive({
    if ( input$stadur != "Keflavík"){
      rtruncnorm(fj_bila_dag()*fj_monte_carlo(),opn_timi(), lokunartimi(), toppur_1(), breidd_1())
      
    } else {
      df <- tibble(
        fyrri = rtruncnorm(fj_bila_dag()*fj_monte_carlo(),opn_timi(), lokunartimi(), toppur_1(), breidd_1()),
        seinni = rtruncnorm(fj_bila_dag()*fj_monte_carlo(),opn_timi(), lokunartimi(), toppur_2(), breidd_2()),
        rand = sample(c(1,0),fj_bila_dag()*fj_monte_carlo(), replace = TRUE, prob=c(hlutf_1(), hlutf_2())))%>%
        mutate(val = ifelse(rand == 1, fyrri, seinni))
      
      as.numeric(unlist(df[,4]))
      
    }
  })
  
  
  
  #Hvort að bíll sé rafbíll eða ekki ræðst af hlutfalli rafbíla
  rafbill <- reactive({
    sample(c(0,1), fj_bila_dag()*fj_monte_carlo(), replace = T, 
           prob =c(1-hlutf_rafbilar(), hlutf_rafbilar())
    )
  })
  #Drögum upphafsstöðu rafhlöðu úr líkindadreifingu sem er fyrir áhrifum af uppbyggingu á gististöðum
  upphafst_rafh <- reactive({
    if(input$hotel == TRUE){
      rtruncnorm(fj_bila_dag()*fj_monte_carlo(), a = 0, b = 100, 95, 5)
    } else {
      rtruncnorm(fj_bila_dag()*fj_monte_carlo(),0,100, 60, 25)
    }
  })
  
  #Drögum tegund bíls úr líkindadreifingu með 3 möguleika sem eru fyrirfram ákveðnir
  teg_bils <- reactive({
    sample(c("teg1","teg2","teg3"),fj_bila_dag()*fj_monte_carlo(), replace=T,
           prob= c(0,1,0))
    
  })
  
  #Upphafsstaða rafhlöðu kwh
  temp <- filter(bilar, tegund == "teg1")%>%
    select(rafhlada)
  
  max1 <- temp[[1]]
  
  temp <- filter(bilar, tegund == "teg2")%>%
    select(rafhlada)
  
  max2 <- temp[[1]]
  
  temp <- filter(bilar, tegund == "teg3")%>%
    select(rafhlada)
  
  max3 <- temp[[1]]
  
  
  upph_stada_kwh <- function(tegund, upph_hledsla){
    if ( tegund == "teg1"){
      max  <-  max1
    }
    else if ( tegund == "teg2"){
      max <-  max2
    }
    else {
      max <- max3
    }
    max*upph_hledsla/100
  }
  
  #Drögum ekna km við komu á stað úr líkindadreifingu byggt á áætluðum upphafsstöðum
  eknir_km <- reactive({
    sample(c(km_std1(), km_std2(), km_std3()), fj_bila_dag()*fj_monte_carlo(), replace = T, 
           prob =c(hlutf_std1(), hlutf_std2(), hlutf_std3())
    )
  })
  
  #Eyðsla - 
  
  temp <- filter(bilar, tegund == "teg1")%>%
    select(eydsla)
  
  eydsla1 <- temp[[1]]
  
  temp <- filter(bilar, tegund == "teg2")%>%
    select(eydsla)
  
  eydsla2 <- temp[[1]]
  
  temp <- filter(bilar, tegund == "teg3")%>%
    select(eydsla)
  
  eydsla3 <- temp[[1]]
  
  
  eydsla_total <- function(tegund, km){
    if ( tegund == "teg1"){
      eydsla  <-  eydsla1
    }
    else if ( tegund == "teg2"){
      eydsla <-  eydsla2
    }
    else {
      eydsla <- eydsla3
    }
    eydsla*km
  }
  
  #Stada kwh við komu
  loka_stada_kwh <- function(upph_kwh, eydsla_kwh){
    if(upph_kwh-eydsla_kwh<0){
      v = sample(1:5,1)
    }else{
      v= upph_kwh-eydsla_kwh
    }
    v
  }
  
  #stada hlutfallslega við komu
  loka_stada <- function(upph_kwh, loka_kwh){
    upph_kwh/loka_kwh
  }
  
  
  #kwh 100
  #Skrifum þetta function m.v. að allir séu tegund 2 (tímasparnaður stutt í deadline)
  
  kwh_100 <-  function(kwh_upph_hledslu, kwh_lok_hledslu=100){
    kwh_lok_hledslu-kwh_upph_hledslu
    
  }
  
  #kwh 80
  #Skrifum þetta function m.v. að allir séu tegund 2 (tímasparnaður stutt í deadline)
  kwh_80 <-  function(kwh_upph_hledslu, kwh_lok_hledslu=80){
    ifelse(kwh_lok_hledslu-kwh_upph_hledslu<0,0,kwh_lok_hledslu-kwh_upph_hledslu) # Ef staða í upphafi er meira en 80% skilum við 0
  }
  
  #Tími sem er stoppað
  #Til bráðabirgða: klst með 15 min í staðalfrávik
  timi_stopp <- reactive({
    rtruncnorm(fj_bila_dag()*fj_monte_carlo(), 0, Inf, 1, 0.25)
  })
  
  #Hledslu condition
  hledslu_condt <-  reactive({
    if(input$styring == TRUE & input$vidmid == "100%"){
      condt <- 100
    } else if(input$styring == TRUE & input$vidmid == "80%"){
      condt <- 80
    } else {
      condt <- 0
    }
    condt
  })
  
  #Hleðsluvector
  hledsluvect <- reactive({
    rep(hledsla_kw(), fj_bila_dag()*fj_monte_carlo())
  })
  
  
  #Hleðslutími
  timi_hlada <- function(condt,hledsla_kw,kwh_100,kwh_80,stopptimi){
    if(condt == 100){
      timi_hlad <- kwh_100/hledsla_kw
    } else if(condt == 80){
      timi_hlad <- kwh_80/hledsla_kw
    } else {
      timi_hlad <- stopptimi
    }
    timi_hlad
  }
  
  #Tími tengdur
  
  timi_tengdur <- function(condt,hledsla_kw, kwh_100,kwh_80,stopptimi){
    if(condt == 100){
      timi_hlad <- ifelse(kwh_100/hledsla_kw<stopptimi,kwh_100/hledsla_kw,stopptimi)
    } else if(condt == 80){
      timi_hlad <- ifelse(kwh_80/hledsla_kw<stopptimi,kwh_80/hledsla_kw,stopptimi)
    } else {
      timi_hlad <- stopptimi
    }
    timi_hlad
  }
  
  #líkur á að hlaða , 
  prop_hlada <- function(stada, kef){
    if (kef != 1){
      if ( stada < 0.25){
        a=1
      }
      else if (stada < 0.5){
        a=0.8
      }
      else if (stada < 0.8){
        a=0.5
      }
      else{
        a=0
      }
      a
    }
    else{
      if ( stada < 0.25){
        a=1
      }
      else if (stada < 0.5){
        a=1
      }
      else if (stada < 0.8){
        a=0.9
      }
      else{
        a=0
      }
      a
    }
    
  }
  
  kef_func <- reactive({
    if (input$stadur == "Keflavík"){
      a <- rep(1, fj_bila_dag()*fj_monte_carlo())
    }
    else{
      a <- rep(0, fj_bila_dag()*fj_monte_carlo())
    }
    a
  })
  
  
  #hledur ? JÁ/NEI
  hledur <- function(prob){
    sample(c(0,1),1,replace=T, prob=c(1-prob, prob))
  }
  
  kef_hadegi_func <-  function(ehk, km){
    ifelse(ehk == 0, km,
           ifelse(km<100, km*2, km))
  }
  
  #Merking 
  merking <-  reactive({
    rep(c(1:fj_monte_carlo()), each=fj_bila_dag())
  })
  
  #breytanlegt df
  
  ferdamenn <- reactive({
    df <- tibble(
      timasetn_bill = timasetningar(),
      rafbill = rafbill(),
      upphafst_rafh = upphafst_rafh(),
      eknir_km = eknir_km(),
      keflavik = kef_func(),
      eftir_hadegi_kef = ifelse(timasetn_bill > 12 & keflavik == 1, 1, 0),
      eknir_km2 = as.numeric(map2(eftir_hadegi_kef, eknir_km, kef_hadegi_func)),
      teg_bils = teg_bils(),
      upph_stada_kwh = as.numeric(map2(teg_bils, upphafst_rafh, upph_stada_kwh)),
      eydsla_kwh = as.numeric(map2(teg_bils, eknir_km2, eydsla_total)),
      stada_kwh = as.numeric(map2(upph_stada_kwh, eydsla_kwh, loka_stada_kwh)),
      hledslu_condt = hledslu_condt(),
      kwh80 = as.numeric(map(stada_kwh, kwh_80)),
      kwh100 = as.numeric(map(stada_kwh, kwh_100)),
      stada_hlutf = as.numeric(map2(stada_kwh,upph_stada_kwh, loka_stada)),
      likur_hlada = as.numeric(map2(stada_hlutf, keflavik, prop_hlada)),
      hledur = as.numeric(map(likur_hlada, hledur)),
      merki = merking(),
      timistopp = timi_stopp(),
      hledslakw = hledsluvect(),
      timihlada = round(as.numeric(pmap(list(hledslu_condt,hledslakw,kwh100,kwh80,timistopp), timi_hlada)),2),
      timi_tengdur = round(as.numeric(pmap(list(hledslu_condt,hledslakw,kwh100,kwh80,timistopp), timi_tengdur)),2),
      kwh_hladid = ifelse(timistopp>timihlada & hledslu_condt == 100,kwh100,
                          ifelse(timistopp>timihlada & hledslu_condt == 80,kwh80,
                                 ifelse(timi_tengdur*hledsla_kw() > kwh100, kwh100 ,timi_tengdur*hledsla_kw()))),
      stada_brottf = ifelse(stada_kwh+kwh_hladid>100,100,stada_kwh+kwh_hladid), #Skrifað út frá tegund 2, þar er battery 100kWh
      stada_brottf_hlutf = stada_brottf/100, #Skrifað út frá tegund 2, þar er battery 100kWh
      brottfarar_timi = timasetn_bill+timistopp,
      otengdur_timi = timasetn_bill+ timi_tengdur)%>%
      mutate(across(where(is.numeric), round, 2))%>%
      separate(timasetn_bill, into=c("komu_klst","komu_min"), sep="\\.", convert=TRUE, fill="right")%>%
      separate(brottfarar_timi, into=c("brottf_klst","brottf_min"), sep="\\.", convert=TRUE, fill="right")%>%
      separate(otengdur_timi, into=c("otengdur_klst","otengdur_min"), sep="\\.", convert=TRUE, fill="right")%>%
      mutate(komu_min= replace_na(komu_min, 0),
             brottf_min = replace_na(brottf_min, 0),
             otengdur_min = replace_na(otengdur_min, 0))%>%
      mutate(komu_min = round((komu_min/100)*60,0),
             brottf_min = round((brottf_min/100)*60,0),
             otengdur_min = round((otengdur_min/100)*60,0))%>%
      mutate(komu_timi = ymd_hms(paste("2020-01-01",komu_klst,komu_min,"00", sep="-")),
             brottf_timi =ymd_hms(paste("2020-01-01",brottf_klst,brottf_min,"00", sep="-")),
             otengdur_timi= ymd_hms(paste("2020-01-01",otengdur_klst,otengdur_min,"00", sep="-")))
    
    df
    
  })
  
  allir <- reactive({
    df <- ferdamenn()%>%
      select(komu_timi, brottf_timi, merki)
    df
  })
  
  rafbilar <- reactive({
    df <- ferdamenn()%>%
      filter(rafbill == 1)%>%
      select(komu_timi, brottf_timi, merki)
    
    df
  })
  
  rafbilar_hlada <- reactive({
    df <- ferdamenn()%>%
      filter(hledur == 1 & rafbill==1)%>%
      select(komu_timi, otengdur_timi, merki)
    
    df
  })
  
  # Þurfum að breyta tímasetingum í timestamps og út frá því reikna rolling sums fyrir einkabíla, rafbíla og rafbíla að hlaða
  
  calc_roll_sum <- function(data, start_var, end_var,
                            begin_time='2020-01-01 00:00:00'){
    sv <- rlang::enquo(start_var)
    ev <- rlang::enquo(end_var)
    
    if(!inherits(begin_time, 'POSIXct')){
      begin_time <- lubridate::ymd_hms(begin_time)
    }
    
    #Keep track of timestamp issues
    incorrect_times <- data %>%
      dplyr::filter(!!ev < !!sv | is.na(!!ev) | is.na(!!sv))%>%
      dplyr::mutate(type = ifelse( is.na(!!ev),
                                   'Missing end time',
                                   ifelse(is.na(!!sv),
                                          'Missing start time',
                                          'End time before start time')))
    
    #Remove timestamp problems
    filtered_data <- data %>%
      dplyr::filter(!!sv <= !!ev)%>% # no end timestamps after start timestamps
      dplyr::filter(!is.na(!!ev))%>% # no missing end times
      dplyr::filter(!is.na(!!sv)) #no missing start times
    
    #get the base count
    pre <- filtered_data %>%
      dplyr::filter(!!sv<begin_time & !!ev >= begin_time)
    
    post <-  filtered_data%>%
      dplyr::filter(!!ev >= begin_time)
    
    #The arrivals
    arrivals <- post %>%
      dplyr::select(timestamp = !!sv)%>%
      dplyr::mutate(counter=1)%>% 
      dplyr::filter(timestamp >= begin_time)
    
    
    #The departures
    departures <- post%>%
      dplyr::select(timestamp = !!ev)%>%
      dplyr::mutate(counter = -1)
    
    #bind the arrivals and departures
    census_volumes <- arrivals %>%
      dplyr::bind_rows(departures) %>%
      dplyr::arrange(timestamp, counter)
    
    #Add the starting pre volume to the first row
    census_volumes$counter[1] <- census_volumes$counter[1]+nrow(pre)
    
    census_volumes <- census_volumes%>% #Arrange by time
      dplyr::mutate(volume=cumsum(counter))
    
    #Create a sequence of times from the start to end of available data
    start <-  min(census_volumes$timestamp)
    end <- max(census_volumes$timestamp)
    full_time_window <-  dplyr::tibble(timestamp = seq( start, end, by='mins'))
    
    census_volumes <-  census_volumes %>%
      right_join(full_time_window,
                 by='timestamp')%>% # bind with the full time window to fill gaps
      arrange(timestamp)%>%
      fill(volume, .direction='down') #take last observation carried forward
    
    if(nrow(incorrect_times)>0){
      incorrect_times_sum <- incorrect_times %>%
        dplyr::count(type)
      
      msg <- paste0(paste(incorrect_times_sum$type,
                          incorrect_times_sum$n, seo =': n = '),
                    collapse = '\n')
      msg <- paste0('Potential Data Issues:\n',
                    msg,
                    '\nThese timestamp errors have been removed from the data prior to volume calculation')
      message(msg)
    }
    
    return(census_volumes)  
    
  }
  
  #Sameinum df fyrir alla, rafbíla og rafbíla sem hlada
  
  
  # 15 min timestamp bil
  timar <-  seq(as.POSIXct("2020-01-01 00:00:00"),as.POSIXct("2020-01-02 00:00:00"), by = "30 min") 
  
  sameining <- reactive({
    
    allir_df <- allir()%>%
      dplyr::group_by(merki)%>%
      calc_roll_sum(start_var = komu_timi, end_var = brottf_timi)%>%
      ungroup()%>%
      mutate(merki = as.factor(merki),
             tegund = "allir")
    
    rafb_df <-  rafbilar()%>%
      dplyr::group_by(merki)%>%
      calc_roll_sum(start_var = komu_timi, end_var = brottf_timi)%>%
      ungroup()%>%
      mutate(merki = as.factor(merki),
             tegund = "rafbilar")
    
    rafb_hlada <-  rafbilar_hlada()%>%
      dplyr::group_by(merki)%>%
      calc_roll_sum(start_var = komu_timi, end_var = otengdur_timi)%>%
      ungroup()%>%
      mutate(merki = as.factor(merki), 
             tegund = "rafb_hlada")
    
    sameinad <-  allir_df%>%
      bind_rows(rafb_df)%>%
      bind_rows(rafb_hlada)
    
    avrg_tot <- sameinad%>%
      group_by(tegund, timestamp)%>%
      summarise(volume = round(mean(volume),0))%>%
      fill(volume)%>%
      mutate(merki = fj_monte_carlo()+1,
             tegund2 = tegund,
             tegund = ifelse(tegund2 == "allir", "allir_avrg",
                             ifelse(tegund2 == "rafbilar", "rafbilar_avrg", "rafb_hlada_avrg")))%>%
      select(merki, timestamp, volume, tegund)%>%
      mutate(merki = as.factor(merki))%>%
      filter(timestamp %in% timar)%>%
      mutate(volume = round(volume))
    
    
    plot_df <- sameinad%>%
      select(-counter)%>%
      bind_rows(avrg_tot)%>%
      arrange(timestamp)
    
    levels(plot_df$tegund) <- c("allir","allir_avrg","rafbilar","rafbilar_avrg","rafb_hlada","rafb_hlada_avrg")
    
    plot_df
  })
  
  
  
  #Render
  # 
  
  # output$test <- renderDataTable(
  #   ferdamenn()%>%
  #   filter(rafbill == 1, hledur == 1)
  # )
  
  # 
  # # 
  lims <- as.POSIXct(strptime(c("2020-01-01 00:00", "2020-01-02 01:00"),
                              format = "%Y-%m-%d %H:%M"))
  
  
  output$mynd2 <- renderPlot(
    sameining()%>%
      drop_na()%>%
      ggplot(aes(x=timestamp, y=volume, linetype=merki,color = tegund, alpha =tegund))+
      geom_line()+
      scale_x_datetime(limits = lims, date_breaks = "3 hours",
                       labels = date_format("%H:%M") )+
      scale_linetype_manual(values = rep(1,fj_monte_carlo()+1), guide='none')+
      scale_color_manual(values =c("red","blue","green","green","green","red"),
                         breaks = c('allir_avrg','rafbilar_avrg','rafb_hlada_avrg'),
                         labels =c("Meðalfjöldi bíla", "Meðalfjöldi rafbíla","Meðalfjöldi rafbíla að hlaða"),
                         name = "")+
      scale_alpha_manual(values = c(.15,1,.15,1,.15,1), guide='none')+
      labs(x = "Klukkustund", y ="Fjöldi bíla",
           title = paste0("Staður: ",input$stadur),
           subtitle= paste0("Hlutfall rafmagnsbíla: ", hlutf_rafbilar()*100,"%                    Mánuður: ", input$man))+
      theme_minimal()+
      theme(legend.position = "bottom")
  )
  
  # Texta niðurstöður
  
  #Hamarksfjoldi bila
  max_bilar <- reactive({
    sameining()%>%
      filter(tegund == "allir_avrg")%>%
      select(volume)%>%
      pull()%>%
      max()
    
  })
  
  output$hamark_bila <- renderText(
    paste0("Hámarksfjöldi bílaleigubíla:\n", round(max_bilar(),0)," bílar")
  )
  
  max_rafbilar <- reactive({
    sameining()%>%
      filter(tegund == "rafbilar_avrg")%>%
      select(volume)%>%
      pull()%>%
      max()
    
  })
  
  output$hamark_rafbila <- renderText(
    paste0("Hámarksfjöldi rafbíla:\n", round(max_rafbilar(),0)," bílar")
  )
  
  max_hlada <- reactive({
    sameining()%>%
      filter(tegund == "rafb_hlada_avrg")%>%
      select(volume)%>%
      pull()%>%
      max()
    
  })
  
  output$hamark_hlada <- renderText(
    paste0("Hámarksfjöldi bíla sem vill hlaða:\n", round(max_hlada(),0)," bílar")
  )
  
  
  #Meðalfjöldi bíla
  avrg_bilar <- reactive({
    sameining()%>%
      filter(tegund == "allir_avrg")%>%
      select(volume)%>%
      pull()%>%
      mean()
    
  })
  
  output$medal_bila <- renderText(
    paste0("Meðalfjöldi bílaleigubíla:\n", round(avrg_bilar(),0)," bílar")
  )
  
  avrg_rafbilar <- reactive({
    sameining()%>%
      filter(tegund == "rafbilar_avrg")%>%
      select(volume)%>%
      pull()%>%
      mean()
    
  })
  
  output$medal_rafbila <- renderText(
    paste0("Meðalfjöldi rafbíla:\n", round(avrg_rafbilar(),0)," bílar")
  )
  
  avrg_rafbilar_hlada <- reactive({
    sameining()%>%
      filter(tegund == "rafb_hlada_avrg")%>%
      select(volume)%>%
      pull()%>%
      mean()
    
  })
  
  output$medal_hlada <- renderText(
    paste0("Meðalfjöldi rafbíla sem vill hlaða:\n", round(avrg_rafbilar_hlada(),0)," bílar")
  )
  
  
  
  #Meðalstaða rafhlöðu í upphafi
  avrg_rafh <- reactive({
    ferdamenn()%>%
      select(upphafst_rafh)%>%
      pull()%>%
      mean()
    
  })
  
  output$upph_rafh <- renderText(
    paste0("Meðalstaða rafhlöðu í upphafi:\n", round(avrg_rafh(),1),"%")
  )
  
  # Meðalfjöldi km
  
  avrg_km <- reactive({
    ferdamenn()%>%
      select(eknir_km2)%>%
      pull()%>%
      mean()
    
  })
  
  output$eknir_km <- renderText(
    paste0("Meðalfjarlægð ekin við komu:\n", round(avrg_km(),0)," km")
  )
  
  #Meðalstaða rafhlöðu við komu
  
  
  avrg_rafh_koma <- reactive({
    ferdamenn()%>%
      select(stada_hlutf)%>%
      pull()%>%
      mean()
    
  })
  
  output$stada_rafh <- renderText(
    paste0("Meðalstaða rafhlöðu við komu\n(allir rafbílar):\n", round(avrg_rafh_koma()*100,1),"%")
  )
  
  # Meðal líkur á að hlaða
  avrg_likur <- reactive({
    ferdamenn()%>%
      filter(rafbill == 1)%>%
      select(likur_hlada)%>%
      pull()%>%
      mean()
    
  })
  
  output$likur_ad_hlada <- renderText(
    paste0("Meðal líkur á að hlaða\n(allir rafbílar):\n", round(avrg_likur()*100,0),"%")
  )
  
  
  
  #Meðaltími að hlaða 100%
  avrg_100 <- reactive({
    ferdamenn()%>%
      filter(rafbill == 1, hledur == 1)%>%
      select(kwh100, hledslakw)%>%
      mutate(avrg_100 = kwh100/hledslakw*60)%>%
      select(avrg_100)%>%
      pull()%>%
      mean()
    
  })
  
  
  output$timi_100 <- renderText(
    paste0("Meðaltími sem tæki að fullhlaða\n(hjá rafbílum sem hlaða):\n",round(avrg_100(),0)," min" )
  )
  #Meðaltstöðvunartími á ferðamannastað
  # avrg_stop <- reactive({
  #   ferdamenn()%>%
  #     select(timistopp)%>%
  #     mutate(timistopp = timistopp*60)%>%
  #     pull()%>%
  #     mean()
  #   
  # })
  # 
  # 
  # output$timi_stopp <- renderText(
  #   paste0("Meðaltími sem bílar stöðva\n(allir bílar) :\n",round(avrg_stop(),0)," min" )
  # )
  avrg_stop <- reactive({
    ferdamenn()%>%
      filter(rafbill == 1, hledur == 1)%>%
      select(timistopp)%>%
      mutate(timistopp = timistopp*60)%>%
      pull()%>%
      mean()
    
  })
  
  
  output$timi_stopp <- renderText(
    paste0("Meðaltími sem bílar stöðva\n(rafbílar sem hlaða) :\n",round(avrg_stop(),0)," min" )
  ) 
  
  
  #Meðal tími að hlaða
  avrg_hlada <- reactive({
    ferdamenn()%>%
      filter(rafbill == 1, hledur == 1)%>%
      select(timi_tengdur)%>%
      mutate(timi_tengdur = timi_tengdur*60)%>%
      pull()%>%
      mean()
    
  })
  
  
  output$timi_hlada <- renderText(
    paste0("Meðaltími sem bílar hlaða\n(rafbílar sem hlaða) :\n",round(avrg_hlada(),0)," min" )
  )
  
  
  # Meðal kWh hlaðið
  
  avrg_kwh <- reactive({
    ferdamenn()%>%
      filter(rafbill == 1, hledur == 1)%>%
      select(kwh_hladid)%>%
      pull()%>%
      mean()
    
  })
  
  output$kwh_hladid <- renderText(
    paste0("Meðal kWh hlaðið \n(rafbílar sem hlaða) :\n",round(avrg_kwh(),0)," kWh" )
  )
  
  #Staða rafhlöðu við brottför
  avrg_brottf <- reactive({
    ferdamenn()%>%
      filter(rafbill == 1)%>%
      select(stada_brottf)%>%
      pull()%>%
      mean()
    
  })
  
  output$stada_rafh_brottf <- renderText(
    paste0("Meðal staða rafhlöðu við brottför \n(allir rafbílar) :\n",round(avrg_brottf(),0),"%" )
  )
  
  ### Líkinda mess ends
  
  
  
  
  
  # Dálkur 3
  
  output$kef_moguleikar <- renderUI({
    if (input$stadur == "Keflavík" ){
      radioButtons("kef_moguleikar", "Sviðsmyndir fyrir Keflavík", 
                   choiceNames = c("Lítil/Meðalstór bílaleiga í eyjarekstri","Sameiginleg þjónusta fyrir bíla undir x% rafhlöðu",
                                   "Sameiginlegur söfnunarstaður fyrir alla"),
                   choiceValues = c("litil","ehf","allir"))
    } else {
      NULL
    }
    
  })  
  
  
  output$fj_i_man <- renderText({
    
    paste0("Áætlaður fjöldi ferðamanna í\n", input$man, " eru ", round(manadar_fjoldi()/1000,0), "\nþúsund ferðamenn")  
    
  })
  
  output$fj_bila_a_dag <- renderText({
    
    paste0("Áætlaður fjöldi bíla á dag í\n", input$man, " eru ", round(fj_bila_dag(),0), "\nbílar")  
    
  })
  
  output$fj_i_kef <- renderText({
    if (input$stadur == "Keflavík" ){
      paste0("Áætlaður fjöldi ferðamanna miðað við\n", input$kef_moguleikar, "\neru xxx þúsund ferðamenn")  
    }
  })
  random_selection <- eventReactive(input$ferdamann_takk,{
    sample_num <- sample(1:nrow(ferdamenn()),1, replace=FALSE)
    sample_num
  })
  
  
  ferdamadur_rand <- reactive({
    df <- ferdamenn()
    
    rad_nr <- random_selection()
    
    komuklst <- df[rad_nr,1]
    komumin <-  df[rad_nr,2]
    rafbill <- df[rad_nr,3]
    upphafst_batt <- df[rad_nr,4]
    km <-  df[rad_nr,5]
    lokast_batt <-  df[rad_nr,13]*100
    hledur <- df[rad_nr,16]
    brottf_klst <- df[rad_nr,17]
    brottf_min <- df[rad_nr,18]
    
    if(rafbill == 0){
      text <- paste0("Koma á ferdamannastað kl. ", str_pad(komuklst,width=2, pad="0"),":",str_pad(komumin,width=2, pad="0"), " eftir að hafa ekið\n", km,
                     " km. Þú ert ekki á rafbíl svo þú leggur í stæði\nog yfirgefur svo svæðið kl. ", str_pad(brottf_klst,width=2, pad="0"),":",str_pad(brottf_min,width=2, pad="0"))
    }
    else if(hledur==0){
      text <-  paste0("Koma á ferdamannastað kl. ", str_pad(komuklst,width=2, pad="0"),":",str_pad(komumin,width=2, pad="0"), " eftir að hafa ekið\n", km,
                      " km. Þú ert á rafbíl, upphafsstaða rafhlöðu var\n", upphafst_batt, "%,batterý við komu er ", lokast_batt,
                      "%,\nþú hleður ekki bílinn\nog yfirgefur svæðið kl. ",  str_pad(brottf_klst,width=2, pad="0"),":",str_pad(brottf_min,width=2, pad="0"))
    }
    else {
      text <-  paste0("Koma á ferdamannastað kl. ", str_pad(komuklst,width=2, pad="0"),":",str_pad(komumin,width=2, pad="0"), " eftir að hafa ekið\n", km,
                      " km. Þú ert á rafbíl, upphafsstaða rafhlöðu var\n", upphafst_batt, "%,staða rafhlöðu við komu er ", lokast_batt,
                      "%,\nþú hleður bílinn\nog yfirgefur svæðið kl. ", str_pad(brottf_klst,width=2, pad="0"),":",str_pad(brottf_min,width=2, pad="0"))
    }
    
    text
  })
  
  output$ferdamadur_random <-  renderText({
    
    ferdamadur_rand()
  })
  
  
}