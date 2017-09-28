
library(shiny)
library(ggplot2)
library(png)
library(grid)
library(dplyr)
library(DT)
library(doParallel)
library(data.table)
library(shinycssloaders)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(knitr)
library(rmarkdown)
library(ggradar)
library(scales)

source("helpers.R")
source('ggradar_edit.R')

a <- readRDS("data/Rzuty.RDS")
b <- readRDS("data/Strefy.RDS")
z <- readRDS("data/KwadStrefy.RDS")
asysty <- readRDS("data/Asysty.RDS")
sp <- readRDS('data/StrefyOgolne.RDS')
xvalue <- readRDS('data/DaneXValue.RDS')
spacing <- readRDS('data/DaneSpacing.RDS')
figury = readRDS('data/ListaFigur.RDS')
tabela_skutecznosc = readRDS('data/SkutecznoscTabela.RDS')
zones_data = readRDS('data/SkutecznoscStrefyZawodnicy.RDS')
radar = readRDS('data/DaneRadar.RDS')

boisko <- readPNG("data/boisko3.png")
boisko <- rasterGrob(boisko, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

boisko2 <- readPNG("data/boisko2.png")
boisko2 <- rasterGrob(boisko2, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

boisko3 <- readPNG("data/boisko4.png")
boisko3 <- rasterGrob(boisko3, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

boisko4 <- readPNG("data/boisko5.png")
boisko4 <- rasterGrob(boisko4, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

dat_comp = readRDS('data/DanePorownania.RDS')

strefy_zaw = readRDS('data/StrefyZaw.RDS')

shinyServer(function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  

       observe({
         
       if(input$sezon == "Wszystkie") {
           a$zawodnik <- as.character(a$zawodnik)
      g <- sort(unique(a$zawodnik)) 
      updateSelectInput(session, "zawodnik", choices = g)
      
         
        } else {
          a$zawodnik <- as.character(a$zawodnik)
         g <- a[a$Sezon == input$sezon,]
         g <- sort(unique(g$zawodnik)) 
         updateSelectInput(session, "zawodnik", choices = g)
        }
})
       
       observe({
         
         if(input$sezon_2 == "Wszystkie"){
           g1 <- sort(unique(asysty$asysta)) 
           updateSelectInput(session, "zawodnik_2", choices = g1)
           
           
         } else {
           g <- asysty[asysty$Sezon == input$sezon_2,]
           g1 <- sort(unique(g$asysta)) 
           updateSelectInput(session, "zawodnik_2", choices = g1)
         }
       })
       
       
       observe({
         
         if(input$sezon_2 == "Wszystkie"){
           g1 <- sort(unique(asysty$asysta)) 
           updateSelectInput(session, "zawodnik_2", choices = g1)
           
           
         } else {
           g <- asysty[asysty$Sezon == input$sezon_2,]
           g1 <- sort(unique(g$asysta)) 
           updateSelectInput(session, "zawodnik_2", choices = g1)
         }
       })
       
       
       observe({
         if(input$porownanie_sezon1 == 'Wszystkie') {
           zawodnicy = unique(a$zawodnik)
           updateSelectInput(session, 'porownanie_zawodnik1', choices = zawodnicy, selected = '') 
         } else {
           zawodnicy <- a[a$Sezon == input$porownanie_sezon1,]
           zawodnicy = unique(zawodnicy$zawodnik)
           updateSelectInput(session, 'porownanie_zawodnik1', choices = zawodnicy, selected = '')
         }
           
       })
       
       observe({
         if(input$porownanie_sezon2 == 'Wszystkie') {
           zawodnicy = unique(a$zawodnik)
           updateSelectInput(session, 'porownanie_zawodnik2', choices = zawodnicy, selected = '') 
         } else {
           zawodnicy <- a[a$Sezon == input$porownanie_sezon2,]
           zawodnicy = unique(zawodnicy$zawodnik)
           updateSelectInput(session, 'porownanie_zawodnik2', choices = zawodnicy, selected = '')
         }
         
       })
       
       
       observe({
         dat = dane_asysty_tab()
         dat = dat$Rzucający
         if(length(dat)>=10) {
           dat = dat[1:10]
         }
         updateCheckboxGroupInput(session, 'wybierz', choices = dat, selected = dat)
       })
       
  
  data <- reactive({
    
    if(input$sezon == "Wszystkie") {
      
      b$zawodnik <- as.character(b$zawodnik)
      l <- b[b$zawodnik == input$zawodnik,]
      l$zawodnik <- NULL
      l$Sezon <- NULL
      n <- l
      n$Średnia <- c("60%", "35%", "33%", "36%", "36%")
      podkosz <- n[n$Strefa == "Spod Kosza",]
      podkosz$FGM <- sum(podkosz$FGM)
      podkosz$FGA <- sum(podkosz$FGA)
      podkosz$FGpr <- podkosz$FGM/podkosz$FGA
      podkosz$FGpr <- percent(podkosz$FGpr)
      podkosz <- podkosz[1,]
      
      mid <- n[n$Strefa == "Midrange",]
      mid$FGM <- sum(mid$FGM)
      mid$FGA <- sum(mid$FGA)
      mid$FGpr <- mid$FGM/mid$FGA
      mid$FGpr <- percent(mid$FGpr)
      mid <- mid[1,]
      
      ps3 <- n[n$Strefa == "3pt Srodek",]
      ps3$FGM <- sum(ps3$FGM)
      ps3$FGA <- sum(ps3$FGA)
      ps3$FGpr <- ps3$FGM/ps3$FGA
      ps3$FGpr <- percent(ps3$FGpr)
      ps3 <- ps3[1,]
      
      pl3 <- n[n$Strefa == "3pt Lewy Naroznik",]
      pl3$FGM <- sum(pl3$FGM)
      pl3$FGA <- sum(pl3$FGA)
      pl3$FGpr <- pl3$FGM/pl3$FGA
      pl3$FGpr <- percent(pl3$FGpr)
      pl3 <- pl3[1,]
      
      pp3 <- n[n$Strefa == "3pt Prawy Naroznik",]
      pp3$FGM <- sum(pp3$FGM)
      pp3$FGA <- sum(pp3$FGA)
      pp3$FGpr <- pp3$FGM/pp3$FGA
      pp3$FGpr <- percent(pp3$FGpr)
      pp3 <- pp3[1,]
      
      n <- rbind(podkosz, mid, ps3, pl3, pp3)
      

      
      n[,5][n[,5] == "NaN%"] <- "0%"
      
      
      n$FGM <- as.character(n$FGM)
      n$FGpr <-  gsub("NA%", "0%", n$FGpr)
      n$FGA <- as.character(n$FGA)
      n$FGM[is.na(n$FGM)] <- 0
      n$FGA[is.na(n$FGA)] <- 0
      n = n[,c(3,1,2,5,4)]
      colnames(n)[4] = 'FG%'
      n[,4][n[,4]=='NaN%'] = '0%'
      return(n)
      
      
      
    }else {
      b$zawodnik <- as.character(b$zawodnik)
      l <- b[b$Sezon == input$sezon,]  
      l <- l[l$zawodnik == input$zawodnik,]
      l$zawodnik <- NULL
      l$Sezon <- NULL
      n <- l
      n$Średnia <- c("60%", "35%", "33%", "36%", "36%")
      n$`FG%` = percent(n$FGM/n$FGA)
      n$FGM <- as.character(n$FGM)
      n$`FG%` <-  gsub("NA%", "0%", n$`FG%`)
      n$FGA <- as.character(n$FGA)
      n$FGM[is.na(n$FGM)] <- 0
      n$FGA[is.na(n$FGA)] <- 0
      n = n[,c(3,1,2,5,4)]
      n[,4][n[,4]=='NaN%'] = '0%'
      return(n)
    }
    return(n)
  })
  


  
  statystyki = reactive({
    spacing_stat = spacing[spacing$Zawodnik == input$zawodnik & spacing$Sezon == input$sezon,]
    xvalue_stat = xvalue[xvalue$Zawodnik == input$zawodnik & xvalue$Sezon == input$sezon,]
    wartosc_oczekiwana = paste(xvalue_stat$XValue, ' ' ,'(', (xvalue_stat$XValuePercentyl*100), ' ', 'centyl', ')', sep = '')
    procent_stref_za3 = paste(spacing_stat$ProcentStref, ' ' ,'(', (spacing_stat$ProcentStref*100), ' ', 'centyl', ')', sep = '')
    spacing_rating = paste(spacing_stat$SpacingRating, ' ' ,'(', (spacing_stat$SpacingRatingPercentyl*100), ' ', 'centyl', ')', sep = '')
    
    if(length(xvalue_stat$XValue)==0) {
      wartosc_oczekiwana <- ''
    }
    if(length(spacing_stat$ProcentStref)==0) {
      procent_stref_za3 <- ''
    }
    if(length(spacing_stat$SpacingRating)==0) {
      spacing_rating <- ''
    }
    df = data.frame(wartosc_oczekiwana, procent_stref_za3, spacing_rating)
    colnames(df) = c('Wartość oczekiwana rzutu', 'Procent zajętych stref 3pt', 'Spacing Rating')
    df = df[, c(2,3,1)]
    return(df)
  })
  

  
  podobni_strzelcy = function(top5 = TRUE) {
    xy_zawodnika = data.frame(X = dat_comp$strefy_zawodnika[dat_comp$Zawodnik == input$zawodnik], 
                              Y = dat_comp$wartosc[dat_comp$Zawodnik == input$zawodnik])
    id_zawodnika =  rownames(dat_comp[dat_comp$Zawodnik == input$zawodnik,])
    dystans = data.frame(dist = sqrt(((xy_zawodnika$X - dat_comp$strefy_zawodnika)^2) + ((xy_zawodnika$Y - dat_comp$wartosc)^2)))
    dystans$id = 1:nrow(dystans)
    dystans = dystans[!dystans$id == id_zawodnika,]
    dystans = arrange(dystans, dist)
    if(top5==TRUE) {
      rows = dystans$id[1:5]
      dystans$dist = round(dystans$dist,2)
      podobni = data.frame(Zawodnik = dat_comp$Zawodnik[rows], Dystans = dystans$dist[1:5])
    } else {
      rows = dystans$id
      dystans$dist = round(dystans$dist,2)
      podobni = data.frame(Zawodnik = dat_comp$Zawodnik[rows], Dystans = dystans$dist)  
    }
    return(podobni)
  }
  podobni_strelcy_strefy = function(top5 = TRUE) {
    id = strefy_zaw$Zawodnik$ID[strefy_zaw$Zawodnik$Zawodnik == input$zawodnik]
    dat = unlist(strefy_zaw$Strefy[id])
    n = 1:587
    n = n[!n==id]
    ranking = lapply(n, function(x) {
      comp = strefy_zaw$Strefy[[x]]
      strefy_wspolne = length(intersect(dat, comp))
      podobienstwo = round(((strefy_wspolne/length(dat)) * (strefy_wspolne/length(comp))), 3)
      return(podobienstwo)
    })
    ranking = do.call(rbind.data.frame, ranking)
    colnames(ranking)[1] = 'podobienstwo'
    ranking$id = 1:nrow(ranking)
    if(top5==TRUE) {
      podobni = head(arrange(ranking, desc(podobienstwo)),5)
    } else {
      podobni = arrange(ranking, desc(podobienstwo)) 
    }
    podobni$id = strefy_zaw$Zawodnik$Zawodnik[podobni$id]
    colnames(podobni) = c('Podobieństwo', 'Zawodnik')
    podobni = podobni[,c(2,1)]
    return(podobni)
  }
  ##Zajmowane Strefy
  #KWad
  plot_line2 = reactive({
    if(input$zawodnik1 != '' & input$zawodnik2 != '' & input$zawodnik3 != '' & input$zawodnik4 != '' & input$zawodnik5 != '') {
    p1 = kwad(input$zawodnik1)
    p2 = kwad(input$zawodnik2)
    p3 = kwad(input$zawodnik3)
    p4 = kwad(input$zawodnik4)
    p5 = kwad(input$zawodnik5)
    p = rbind(p1, p2, p3, p4, p5)
    n = unique(p$id)
    
    otrzymaj_rzuty_lineupy = function(nr) {
      wybor = p[p$id==n[nr],]
      if(length(unique(wybor$zawodnik))==1) {
        wybor = wybor
      } else {
        sprawdz = wybor %>%
          group_by(zawodnik) %>%
          summarize(FGA = sum(FGA1))
        sprawdz = arrange(sprawdz, desc(FGA))
        zaw = sprawdz$zawodnik[1]
        wybor = wybor[wybor$zawodnik == zaw,]
      }
      return(wybor)
    }
    
    rzuty_lineup = data.frame()
    foreach(i = 1:length(n), .errorhandling = 'remove') %dopar% {
      rzuty_lineup = rbind(rzuty_lineup, otrzymaj_rzuty_lineupy(i))
    }
    colnames(rzuty_lineup)[5] = 'Zawodnik'
    
    tytul = data.frame(x = 815, y = 1100, label = 'Najczęściej rzucający')
    
    plot = ggplot() +
      annotation_custom(boisko4, -60, 1570, -10, 1150) +
      geom_point(data = rzuty_lineup, aes(size = FGA1, x = xadj, y = yadj, colour = Zawodnik), alpha = .90, stroke = 0.3, shape = 15)  +
      scale_size(range = c(3, 9.5))  +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      geom_text(data = tytul, aes(x = x, y = y, label = label), size = 11, colour = 'black') +
      guides(size = FALSE) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = c(0.86, 0.865),
            legend.background = element_rect(fill="transparent", 
                                             size=0.9, linetype="solid"),
            legend.text = element_text(colour = 'white', size = 15),
            legend.title = element_text(colour = 'white', size = 17))
    }
    return(plot)
  })
  
  
  ###Zajmowane Strefy nbastats.com
  plot_line1 = reactive({
    if(input$zawodnik1 != '' & input$zawodnik2 != '' & input$zawodnik3 != '' & input$zawodnik4 != '' & input$zawodnik5 != '') {
    p1 = zones_data[zones_data$Zawodnik == input$zawodnik1 & zones_data$Sezon == 'Wszystkie',]
    p2 = zones_data[zones_data$Zawodnik == input$zawodnik2 & zones_data$Sezon == 'Wszystkie',]
    p3 = zones_data[zones_data$Zawodnik == input$zawodnik3 & zones_data$Sezon == 'Wszystkie',]
    p4 = zones_data[zones_data$Zawodnik == input$zawodnik4 & zones_data$Sezon == 'Wszystkie',]
    p5 = zones_data[zones_data$Zawodnik == input$zawodnik5 & zones_data$Sezon == 'Wszystkie',]
    dat = rbind(p1, p2, p3, p4, p5)
    
    lineup_dat =  lapply(1:16, function(nr) {  
      dat1 = dat[dat$id==nr,]
      dat2 = dat1[dat1$fga >= 20,]
      if(nrow(dat2)==0) {
        dat2 <- arrange(dat1, desc(fga))[1,]
      }
      dat2 = arrange(dat2, desc(fgp))[1,]
      return(dat2)
    })
    lineup_dat = do.call(rbind.data.frame, lineup_dat) 
    
    tytul = data.frame(x = c(755), y = c(1095), dane = as.character(''))
    tytul$dane = c('Najskuteczniejsi')
    
    Srodki = readRDS('data/SrodkiFigur.RDS')
    text1 = data.frame(data = lineup_dat$fgp, Srodki[,2:3])
    text1$data = paste((text1$data*100), '%', sep = '')
    text1$Y = text1$Y-60
    ##Zawodnik
    text2 = data.frame(data = lineup_dat$Zawodnik, Srodki[,2:3])
    
    p1 = lineup_dat
    zawodnicy = unique(p1$Zawodnik)
    p1$kolor = ''
    p1$kolor[p1$Zawodnik == zawodnicy[1]] = '#9C27B0'
    p1$kolor[p1$Zawodnik == zawodnicy[2]] = '#2196F3'
    p1$kolor[p1$Zawodnik == zawodnicy[3]] = '#4CAF50'
    p1$kolor[p1$Zawodnik == zawodnicy[4]] = '#FFEB3B'
    p1$kolor[p1$Zawodnik == zawodnicy[5]] = '#795548'
    p1$alpha = 0.5
    p1$alpha[p1$fga==0] = 0
    
    
    ##Ladowanie listy figur i ustalanie parametrów mapy
    plot = ggplot() +
      annotation_custom(boisko4, -60, 1570, -10, 1150)  +
      geom_polygon(data = figury[[1]], aes(x = long, y = lat, group = group), fill = p1$kolor[1], alpha = p1$alpha[1], colour = '#424242') +
      geom_polygon(data = figury[[2]], aes(x = long, y = lat, group = group), fill = p1$kolor[2], alpha = p1$alpha[2], colour = '#424242') +
      geom_polygon(data = figury[[3]], aes(x = long, y = lat, group = group), fill = p1$kolor[3], alpha = p1$alpha[3], colour = '#424242') +
      geom_polygon(data = figury[[4]], aes(x = long, y = lat, group = group), fill = p1$kolor[4], alpha = p1$alpha[4], colour = '#424242') +
      geom_polygon(data = figury[[5]], aes(x = long, y = lat, group = group), fill = p1$kolor[5], alpha = p1$alpha[5], colour = '#424242') +
      geom_polygon(data = figury[[6]], aes(x = long, y = lat, group = group), fill = p1$kolor[6], alpha = p1$alpha[6], colour = '#424242') +
      geom_polygon(data = figury[[7]], aes(x = long, y = lat, group = group), fill = p1$kolor[7], alpha = p1$alpha[7], colour = '#424242') +
      geom_polygon(data = figury[[8]], aes(x = long, y = lat, group = group), fill = p1$kolor[8], alpha = p1$alpha[8], colour = '#424242') +
      geom_polygon(data = figury[[9]], aes(x = long, y = lat, group = group), fill = p1$kolor[9], alpha = p1$alpha[9], colour = '#424242') +
      geom_polygon(data = figury[[10]], aes(x = long, y = lat, group = group), fill = p1$kolor[10], alpha = p1$alpha[10], colour = '#424242') +
      geom_polygon(data = figury[[11]], aes(x = long, y = lat, group = group), fill = p1$kolor[11], alpha = p1$alpha[11], colour = '#424242') +
      geom_polygon(data = figury[[12]], aes(x = long, y = lat, group = group), fill = p1$kolor[12], alpha = p1$alpha[12], colour = '#424242') +
      geom_polygon(data = figury[[13]], aes(x = long, y = lat, group = group), fill = p1$kolor[13], alpha = p1$alpha[13], colour = '#424242') +
      geom_polygon(data = figury[[14]], aes(x = long, y = lat, group = group), fill = p1$kolor[14], alpha = p1$alpha[14], colour = '#424242') +
      geom_polygon(data = figury[[15]], aes(x = long, y = lat, group = group), fill = p1$kolor[15], alpha = p1$alpha[15], colour = '#424242') +
      geom_polygon(data = figury[[16]], aes(x = long, y = lat, group = group), fill = p1$kolor[16], alpha = p1$alpha[16], colour = '#424242') +
      geom_polygon(data = figury[[17]], aes(x = long, y = lat, group = group), fill = '#424242', alpha = 0.9, colour = '#424242') +
      geom_text(data = text1, aes(x = X, y = Y, label = data), colour = 'white', size = 5, family = 'AvantGarde', fontface = 'bold') +
      geom_text(data = text2, aes(x = X, y = Y, label = data), colour = 'white', size = 4, family = 'AvantGarde', fontface = 'bold') +
      geom_text(data = tytul, aes(x = x, y = y, label = dane), colour = 'white', size = 7, family = 'AvantGarde', fontface = 'bold') +
      xlim(-61, 1571) +
      ylim(-11, 1151) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.margin = element_blank())
    }
    return(plot)
  })
  
  
  output$statystyki <- DT::renderDataTable({
    statystyki()
  }, options = list(dom = 't'), rownames = FALSE, class = 'cell-border stripe')
  
  
  output$table <- renderDataTable({
    data()
}, options = list(dom = 't'), rownames = FALSE, class = 'cell-border stripe')
  
  output$spacing = renderDataTable({
    dat = radar
    names(dat)[3:7] = c('xValue', '% Zajętych Stref 3pt', 'Spacing Rating', 
                         '% FGM asystowanych', 'eFG%')
    dat = dat[dat$Sezon == input$sezon2,]
    return(dat)
  }, rownames = FALSE,  class = 'cell-border stripe', options = list(pageLength = 15,
                               scrollX = TRUE,
                               language = list(search = 'Szukaj:', 
                                              info = 'Pozycje od _START_ do _END_ z _TOTAL_ łącznie',
                                              lengthMenu = 'Pokaż _MENU_ pozycji',
                                              infoEmpty = "Pozycji 0 z 0 dostępnych",
                                              infoFiltered = "(filtrowanie spośród _MAX_ dostępnych pozycji)",
                                              loadingRecords = "Wczytywanie...",
                                              zeroRecords = "Nie znaleziono pasujących pozycji",
                                              emptyTable = 'Brak danych',
                                              paginate = list(`next` = 'Następna', `previous` = 'Poprzednia'))))
  
  output$skutecznosc = renderDataTable({
    shinyjs::addClass(selector = "body", class = "sidebar-collapse") 
    dat = subset(tabela_skutecznosc, tabela_skutecznosc$Sezon == input$sezon2)
    return(dat)
  }, rownames = FALSE,  class = 'cell-border stripe', options = list(pageLength = 15,
                                                                     scrollX = TRUE,
                                                                     language = list(search = 'Szukaj:', 
                                                                                     info = 'Pozycje od _START_ do _END_ z _TOTAL_ łącznie',
                                                                                     lengthMenu = 'Pokaż _MENU_ pozycji',
                                                                                     infoEmpty = "Pozycji 0 z 0 dostępnych",
                                                                                     infoFiltered = "(filtrowanie spośród _MAX_ dostępnych pozycji)",
                                                                                     loadingRecords = "Wczytywanie...",
                                                                                     zeroRecords = "Nie znaleziono pasujących pozycji",
                                                                                     emptyTable = 'Brak danych',
                                                                                     paginate = list(`next` = 'Następna', `previous` = 'Poprzednia'))))
  
  output$porownanie <- DT::renderDataTable({
    if(input$metoda == 'Dystans 2D') {
    podobni_strzelcy()
    } else if(input$metoda == 'Wspólne Strefy') {
      podobni_strelcy_strefy()
    } else if(input$metoda == 'Średnia Pozycja') {
      dat1 = podobni_strzelcy(top5 = FALSE)
      dat2 = podobni_strelcy_strefy(top5 = FALSE)
      dat1$rank = 1:nrow(dat1)
      dat2$rank = 1:nrow(dat2)
      dat2$Podobieństwo = NULL
      dat1$Dystans = NULL
      dat = merge(dat1, dat2, by = 'Zawodnik')
      dat$avg = (dat$rank.x+dat$rank.y)/2
      dat = head(arrange(dat[,c(1,4)], avg), 5)
      colnames(dat)[2] = 'Średnia Pozycja'
      return(dat)
    }
  }, options = list(dom = 't'), rownames = FALSE, class = 'cell-border stripe')
  
  
  output$barplot = renderPlotly({
    if(input$sezon == 'Wszystkie') {
      dat = b[b$zawodnik == input$zawodnik,]
      d1 = dat[dat$Strefa == 'Spod Kosza',]
      d1$FGM[1] = sum(d1$FGM)
      d1$FGA[1] = sum(d1$FGA)
      d1 = d1[1,]
      d2 = dat[dat$Strefa == 'Midrange',]
      d2$FGM[1] = sum(d2$FGM)
      d2$FGA[1] = sum(d2$FGA)
      d2 = d2[1,]
      d3 = dat[dat$Strefa == '3pt Srodek',]
      d3$FGM[1] = sum(d3$FGM)
      d3$FGA[1] = sum(d3$FGA)
      d3 = d3[1,]
      d4 = dat[dat$Strefa == '3pt Lewy Naroznik',]
      d4$FGM[1] = sum(d4$FGM)
      d4$FGA[1] = sum(d4$FGA)
      d4 = d4[1,]
      d5 = dat[dat$Strefa == '3pt Prawy Naroznik',]
      d5$FGM[1] = sum(d5$FGM)
      d5$FGA[1] = sum(d5$FGA)
      d5 = d5[1,]
      dat = rbind(d1, d2, d3, d4, d5)
    } else {
      dat = b[b$zawodnik == input$zawodnik & b$Sezon == input$sezon,]
    }
    
    t <- list(
      family = "sans serif",
      size = 9)
    
    plot_bar =  plot_ly(dat, x = ~Strefa, y = ~FGM, type = 'bar', name = 'FGM', marker = list(color = 'rgb(76,175,80)')) %>%
      add_trace(y = ~FGA, name = 'FGA', marker = list(color = 'rgb(96,125,139)')) %>%
      layout(barmode = 'group',
             yaxis = list(title = ' '), xaxis = list(title = ''), font = t)
    print(plot_bar)
  })
  
  output$boxplot = renderPlot({
    
    dat1 = radar
    dat1[is.na(dat1)] = 0
    names(dat1)[3:7] = c('xValue', '% Zajętych Stref 3pt', 'Spacing Rating', 
                         '% FGM asystowanych', 'eFG%')
    temp = melt(dat1)
    punkty = temp[temp$Zawodnik == input$zawodnik & temp$Sezon == input$sezon,]
    if(nrow(punkty)>3) {
      d1 = punkty[punkty$variable =='% Zajętych Stref 3pt',]
      d1$value[1] = round(mean(d1$value),2)
      d1 = d1[1,]
      d2 = punkty[punkty$variable =='Spacing Rating',]
      d2$value[1] = round(mean(d2$value),2)
      d2 = d2[1,]
      d3 = punkty[punkty$variable =='xValue',]
      d3$value[1] = round(mean(d3$value),2)
      d3 = d3[1,]
      d4 = punkty[punkty$variable =='% FGM asystowanych',]
      d4$value[1] = round(mean(d4$value),2)
      d4 = d4[1,]
      d5 = punkty[punkty$variable =='eFG%',]
      d5$value[1] = round(mean(d5$value),2)
      d5 = d5[1,]
      punkty = rbind(d1, d2, d3, d4, d5)
    }
    
    
    plot = ggplot(temp, aes(x = variable, y = value)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.2)  +
      geom_point(data = punkty, aes(x = variable, y = value, colour = Zawodnik),
                 size = 3.5, alpha = 0.9) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 1.75)) +
      theme(legend.position = c(0.1, 0.9),
            legend.background = element_rect(fill="white", 
                                             size=0.9, linetype="solid"),
            legend.title = element_text(size = 13, family = 'sans-serif'),
            legend.text = element_text(size = 11, family = 'sans-serif'),
            axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11),
            axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
            plot.title = element_text(hjust = 0.5, margin = unit(c(0, 5, 0, 0), "mm"),
                                      size = 14, family = 'sans-serif')) +
      labs(x = 'Statystyka', y = 'Wartość', title = 'Wykres pudełkowy statystyk rzutowych') 
    print(plot)
    
  })
  
  dane_asysty_plot = reactive({
    if(input$sezon_2 == 'Wszystkie') {
      dat = asysty[asysty$asysta == input$zawodnik_2,]
    } else {
      dat = asysty[asysty$asysta == input$zawodnik_2 & asysty$Sezon == input$sezon_2,]  
    }
    tab_asysty = dat %>%
      group_by(zawodnik) %>%
      summarize(Asysty = n())
    tab_asysty = tab_asysty[tab_asysty$Asysty>=5,]
    tab_asysty = arrange(tab_asysty, desc(Asysty))
    if(nrow(tab_asysty>=10)) {
    dat = dat[dat$zawodnik %in%  tab_asysty$zawodnik[1:10],]
    } else {
    dat = dat[dat$zawodnik %in%  tab_asysty$zawodnik,]  
    }
    colnames(dat)[1] = 'Rzucający'
    return(dat)
  })
  
  dane_asysty_tab = reactive({
    if(input$sezon_2 == 'Wszystkie') {
      dat = asysty[asysty$asysta == input$zawodnik_2,]
    } else {
      dat = asysty[asysty$asysta == input$zawodnik_2 & asysty$Sezon == input$sezon_2,]  
    }
    tab_asysty = dat %>%
      group_by(zawodnik) %>%
      summarize(Asysty = n())
    dat = tab_asysty[tab_asysty$Asysty>=5,]
    colnames(dat)[1] = 'Rzucający'
    dat = arrange(dat, desc(Asysty))
    return(dat)
  })
  

  output$plot <- reactivePlot(function() {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse") 
    if(input$sezon == "Wszystkie") {
      
      a$zawodnik <- as.character(a$zawodnik)
      h <- a[a$zawodnik == input$zawodnik,]
      
      zaw <- h$zawodnik[1]
      
  
      len <- length(h$zawodnik)
      
      team <- h$druzyna[len]
      team <- as.character(team)

     
      


if(input$chart == "HeatMapa") {
  
p <- ggplot(h, aes(x=x,y=y)) +
  stat_density2d(geom="raster", aes(fill = ..density..), contour = FALSE, na.rm = FALSE, n = 250) +
  scale_fill_gradientn(colors = col, guide = FALSE )+
  annotation_custom(boisko, -60, 1570, -10, 1150)  +
  guides(alpha = FALSE, size = FALSE, fill = FALSE) +
  annotate("text", x = 770, y = 1020, label = team , size = 8, family="sans", fontface="bold", color = "white") +
  annotate("text", x = 770, y = 1100, label = zaw , size = 8.5, family="sans", fontface="bold", color = "white") +
  xlim(-60, 1570) +
  ylim(-10, 1150) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.margin = element_blank(),
        legend.title = element_blank(),
        plot.title = element_blank())
print(p)

  } else if(input$chart == "Standardowy") {

    p <- ggplot(h, aes(x=x,y=y)) +
      annotation_custom(boisko2, -60, 1570, -10, 1150) +
      geom_point(aes(colour = trafienie), size = 6, alpha = 0.7, stroke = 2) +
      scale_color_manual(values = c("#00C853", "#f44336")) +
      guides(alpha = FALSE, size = FALSE, fill = FALSE) +
      annotate("text", x = 770, y = 1020, label = team , size = 8, family="sans", fontface="bold", color = "#263238") +
      annotate("text", x = 770, y = 1100, label = zaw , size = 10, family="sans", fontface="bold", color = "#263238") +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            plot.title = element_blank())
    print(p)
    
  } else if(input$chart == 'Strefowa #1') {
      p1 = zones_data[zones_data$Zawodnik == input$zawodnik & zones_data$Sezon == input$sezon,]
      if(nrow(p1) > 16) {
        srednia = p1$avgfgp[1:16]
        Zawodnik = p1$Zawodnik
        Sezon = p1$Sezon
        id = 1:16
        dat2 = lapply(1:16, function(nr) {
          dat1 = p1[p1$id==nr,]
          fgm = sum(dat1$fgm)
          fga = sum(dat1$fga)
          fgp = round((fgm/fga), 2)
          df = data.frame(fgm, fga, fgp)
          return(df)
        })
        dat2 = do.call(rbind.data.frame, dat2)
        dat2$fgp[dat2$fgp=='NaN'] <- 0
        p1 = data.frame(dat2, avgfgp = srednia, id, Zawodnik, Sezon)
      }
      
      tytul = data.frame(x = c(755, 755), y = c(1115, 1060), dane = as.character(''))
      tytul$dane = c(input$zawodnik, input$sezon)
      
      Srodki = readRDS('data/SrodkiFigur.RDS')
      text1 = data.frame(data = p1$fgp, Srodki[,2:3])
      text1$data = paste((text1$data*100), '%', sep = '')
      ##FGM/FGA
      text2 = data.frame(fgm = p1$fgm, fga = p1$fga, Srodki[,2:3])
      text2$data = paste('(', text2$fgm, '/', text2$fga, ')', sep = '')
      text2$Y = text2$Y-40
      text2$id = 1:16
      ids = text2$id[text2$fga==0]
      text2$data[ids] = ''
      text1$data[ids] = ''
      
      p1$minFgp[1:8] = p1$avgfgp[1:8]-0.02
      p1$minFgp[16] = p1$avgfgp[16]-0.02
      p1$minFgp[9:15] = p1$avgfgp[9:15]-0.02 
      p1$maxFgp[1:8] = p1$avgfgp[1:8]+0.02
      p1$maxFgp[16] = p1$avgfgp[16]+0.02
      p1$maxFgp[9:15] = p1$avgfgp[9:15]+0.02
      p1$kolor = ''
      p1$kolor[p1$fgp<p1$minFgp] <- '#e53935'
      p1$kolor[p1$fgp>p1$maxFgp] <- '#43A047'
      p1$kolor[p1$fgp>=p1$minFgp & p1$fgp<= p1$maxFgp] <- '#FFB300'
      p1$alpha = 0.5
      p1$alpha[p1$fga==0] = 0
      
      ##Ladowanie listy figur i ustalanie parametrów mapy
      alpha = 0.5
      plot = ggplot() +
        annotation_custom(boisko4, -60, 1570, -10, 1150)  +
        geom_polygon(data = figury[[1]], aes(x = long, y = lat, group = group), fill = p1$kolor[1], alpha = p1$alpha[1], colour = '#424242') +
        geom_polygon(data = figury[[2]], aes(x = long, y = lat, group = group), fill = p1$kolor[2], alpha = p1$alpha[2], colour = '#424242') +
        geom_polygon(data = figury[[3]], aes(x = long, y = lat, group = group), fill = p1$kolor[3], alpha = p1$alpha[3], colour = '#424242') +
        geom_polygon(data = figury[[4]], aes(x = long, y = lat, group = group), fill = p1$kolor[4], alpha = p1$alpha[4], colour = '#424242') +
        geom_polygon(data = figury[[5]], aes(x = long, y = lat, group = group), fill = p1$kolor[5], alpha = p1$alpha[5], colour = '#424242') +
        geom_polygon(data = figury[[6]], aes(x = long, y = lat, group = group), fill = p1$kolor[6], alpha = p1$alpha[6], colour = '#424242') +
        geom_polygon(data = figury[[7]], aes(x = long, y = lat, group = group), fill = p1$kolor[7], alpha = p1$alpha[7], colour = '#424242') +
        geom_polygon(data = figury[[8]], aes(x = long, y = lat, group = group), fill = p1$kolor[8], alpha = p1$alpha[8], colour = '#424242') +
        geom_polygon(data = figury[[9]], aes(x = long, y = lat, group = group), fill = p1$kolor[9], alpha = p1$alpha[9], colour = '#424242') +
        geom_polygon(data = figury[[10]], aes(x = long, y = lat, group = group), fill = p1$kolor[10], alpha = p1$alpha[10], colour = '#424242') +
        geom_polygon(data = figury[[11]], aes(x = long, y = lat, group = group), fill = p1$kolor[11], alpha = p1$alpha[11], colour = '#424242') +
        geom_polygon(data = figury[[12]], aes(x = long, y = lat, group = group), fill = p1$kolor[12], alpha = p1$alpha[12], colour = '#424242') +
        geom_polygon(data = figury[[13]], aes(x = long, y = lat, group = group), fill = p1$kolor[13], alpha = p1$alpha[13], colour = '#424242') +
        geom_polygon(data = figury[[14]], aes(x = long, y = lat, group = group), fill = p1$kolor[14], alpha = p1$alpha[14], colour = '#424242') +
        geom_polygon(data = figury[[15]], aes(x = long, y = lat, group = group), fill = p1$kolor[15], alpha = p1$alpha[15], colour = '#424242') +
        geom_polygon(data = figury[[16]], aes(x = long, y = lat, group = group), fill = p1$kolor[16], alpha = p1$alpha[16], colour = '#424242') +
        geom_polygon(data = figury[[17]], aes(x = long, y = lat, group = group), fill = '#424242', alpha = 0.9, colour = '#424242') +
        geom_text(data = text1, aes(x = X, y = Y, label = data), colour = 'white', size = 6, family = 'AvantGarde', fontface = 'bold') +
        geom_text(data = text2, aes(x = X, y = Y, label = data), colour = 'white', size = 5, family = 'AvantGarde', fontface = 'bold') +
        geom_text(data = tytul, aes(x = x, y = y, label = dane), colour = 'white', size = 7, family = 'AvantGarde', fontface = 'bold') +
        xlim(-61, 1571) +
        ylim(-11, 1151) +
        theme(line = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              panel.background = element_blank(),
              axis.text.y = element_blank(),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.margin = element_blank(),
              legend.title = element_blank())
      print(plot)
    
  
  } else if(input$chart == "Strefowa #2") {
    zaw <- h$zawodnik[1]
    zaw <- as.character(zaw)
    
    team <- h$druzyna[1]
    team <- as.character(team)
    
    sezon <- h$Sezon[1]
    sezon <- as.character(sezon)
    
    sh <- kwad(zaw)

    p <- ggplot(sh, aes(x=xadj,y=yadj)) +
      annotation_custom(boisko3, -60, 1570, -10, 1150) +
      geom_point(aes(colour = vsAvg, size = FGA1), alpha = .90, stroke = 0.3, shape = 15)  +
      scale_size(range = c(2.5, 8.5)) +
      annotate("text", x = 770, y = 1100, label = zaw , size = 8.5, family="sans", fontface="bold", color = "black") +
      guides(alpha = FALSE, size = FALSE, fill = FALSE) +
      scale_colour_gradientn(colors = col2) +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            plot.title = element_blank())
    print(p)
}
 } else { 

   
  a$zawodnik <- as.character(a$zawodnik)
  

  
  h <- a[a$Sezon == input$sezon,]
  h <- h[h$zawodnik == input$zawodnik,]
  
  
  zaw <- h$zawodnik[1]
  zaw <- as.character(zaw)
  
  team <- h$druzyna[1]
  team <- as.character(team)
  
  sezon <- h$Sezon[1]
  sezon <- as.character(sezon)

  

  if(input$chart == "HeatMapa") {

    
    p <- ggplot(h, aes(x=x,y=y)) +
      stat_density2d(geom="raster", aes(fill = ..density..), contour = FALSE, na.rm = FALSE, n = 250) +
      scale_fill_gradientn(colors = col, guide = FALSE )+
      annotation_custom(boisko, -60, 1570, -10, 1150)  +
      guides(alpha = FALSE, size = FALSE, fill = FALSE) +
      annotate("text", x = 770, y = 1020, label = team , size = 8, family="sans", fontface="bold", color = "white") +
      annotate("text", x = 770, y = 1100, label = zaw , size = 8.5, family="sans", fontface="bold", color = "white") +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.margin = element_blank(),
            legend.title = element_blank(),
            plot.title = element_blank())
    print(p)
    
  }else if(input$chart == "Strefowa #2") {
    zaw <- h$zawodnik[1]
    zaw <- as.character(zaw)
    
    team <- h$druzyna[1]
    team <- as.character(team)
    
    sezon <- h$Sezon[1]
    sezon <- as.character(sezon)
    
    sh <- kwad2(zaw, sezon)
    p <- ggplot(sh, aes(x=xadj,y=yadj)) +
      annotation_custom(boisko3, -60, 1570, -10, 1150) +
      geom_point(aes(colour = vsAvg, size = FGA1), alpha = .90, stroke = 0.3, shape = 15)  +
      scale_size(range = c(2.5, 8.5))  +
      annotate("text", x = 770, y = 1100, label = zaw , size = 8.5, family="sans", fontface="bold", color = "black") +
      guides(alpha = FALSE, size = FALSE, fill = FALSE) +
      scale_colour_gradientn(colors = col2) +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            plot.title = element_blank())
    print(p)
    
    
    
    
  }else if(input$chart == "Standardowy") {

    p <- ggplot(h, aes(x=x,y=y)) +
      annotation_custom(boisko2, -60, 1570, -10, 1150) +
      geom_point(aes(colour = trafienie), size = 6, alpha = 0.7, stroke = 2) +
      scale_color_manual(values = c("#00C853", "#f44336")) +
      guides(alpha = FALSE, size = FALSE, fill = FALSE) +
      annotate("text", x = 770, y = 1020, label = team , size = 8, family="sans", fontface="bold", color = "#263238") +
      annotate("text", x = 770, y = 1100, label = zaw , size = 10, family="sans", fontface="bold", color = "#263238") +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            plot.title = element_blank())
    print(p)
    
  } else  if(input$chart == 'Strefowa #1') {
      p1 = zones_data[zones_data$Zawodnik == input$zawodnik & zones_data$Sezon == input$sezon,]
      tytul = data.frame(x = c(755, 755), y = c(1115, 1060), dane = as.character(''))
      tytul$dane = c(input$zawodnik, input$sezon)
      
      Srodki = readRDS('data/SrodkiFigur.RDS')
      text1 = data.frame(data = p1$fgp, Srodki[,2:3])
      text1$data = paste((text1$data*100), '%', sep = '')
      ##FGM/FGA
      text2 = data.frame(fgm = p1$fgm, fga = p1$fga, Srodki[,2:3])
      text2$data = paste('(', text2$fgm, '/', text2$fga, ')', sep = '')
      text2$Y = text2$Y-40
      text2$id = 1:16
      ids = text2$id[text2$fga==0]
      text2$data[ids] = ''
      text1$data[ids] = ''
      
      p1$minFgp[1:8] = p1$avgfgp[1:8]-0.035 
      p1$minFgp[16] = p1$avgfgp[16]-0.035
      p1$minFgp[9:15] = p1$avgfgp[9:15]-0.023 
      p1$maxFgp[1:8] = p1$avgfgp[1:8]+0.035 
      p1$maxFgp[16] = p1$avgfgp[16]+0.035
      p1$maxFgp[9:15] = p1$avgfgp[9:15]+0.023 
      p1$kolor = ''
      p1$kolor[p1$fgp<p1$minFgp] <- '#e53935'
      p1$kolor[p1$fgp>p1$maxFgp] <- '#43A047'
      p1$kolor[p1$fgp>=p1$minFgp & p1$fgp<= p1$maxFgp] <- '#FFB300'
      p1$alpha = 0.5
      p1$alpha[p1$fga==0] = 0
      
      ##Ladowanie listy figur i ustalanie parametrów mapy
      alpha = 0.5
      plot = ggplot() +
        annotation_custom(boisko4, -60, 1570, -10, 1150)  +
        geom_polygon(data = figury[[1]], aes(x = long, y = lat, group = group), fill = p1$kolor[1], alpha = p1$alpha[1], colour = '#424242') +
        geom_polygon(data = figury[[2]], aes(x = long, y = lat, group = group), fill = p1$kolor[2], alpha = p1$alpha[2], colour = '#424242') +
        geom_polygon(data = figury[[3]], aes(x = long, y = lat, group = group), fill = p1$kolor[3], alpha = p1$alpha[3], colour = '#424242') +
        geom_polygon(data = figury[[4]], aes(x = long, y = lat, group = group), fill = p1$kolor[4], alpha = p1$alpha[4], colour = '#424242') +
        geom_polygon(data = figury[[5]], aes(x = long, y = lat, group = group), fill = p1$kolor[5], alpha = p1$alpha[5], colour = '#424242') +
        geom_polygon(data = figury[[6]], aes(x = long, y = lat, group = group), fill = p1$kolor[6], alpha = p1$alpha[6], colour = '#424242') +
        geom_polygon(data = figury[[7]], aes(x = long, y = lat, group = group), fill = p1$kolor[7], alpha = p1$alpha[7], colour = '#424242') +
        geom_polygon(data = figury[[8]], aes(x = long, y = lat, group = group), fill = p1$kolor[8], alpha = p1$alpha[8], colour = '#424242') +
        geom_polygon(data = figury[[9]], aes(x = long, y = lat, group = group), fill = p1$kolor[9], alpha = p1$alpha[9], colour = '#424242') +
        geom_polygon(data = figury[[10]], aes(x = long, y = lat, group = group), fill = p1$kolor[10], alpha = p1$alpha[10], colour = '#424242') +
        geom_polygon(data = figury[[11]], aes(x = long, y = lat, group = group), fill = p1$kolor[11], alpha = p1$alpha[11], colour = '#424242') +
        geom_polygon(data = figury[[12]], aes(x = long, y = lat, group = group), fill = p1$kolor[12], alpha = p1$alpha[12], colour = '#424242') +
        geom_polygon(data = figury[[13]], aes(x = long, y = lat, group = group), fill = p1$kolor[13], alpha = p1$alpha[13], colour = '#424242') +
        geom_polygon(data = figury[[14]], aes(x = long, y = lat, group = group), fill = p1$kolor[14], alpha = p1$alpha[14], colour = '#424242') +
        geom_polygon(data = figury[[15]], aes(x = long, y = lat, group = group), fill = p1$kolor[15], alpha = p1$alpha[15], colour = '#424242') +
        geom_polygon(data = figury[[16]], aes(x = long, y = lat, group = group), fill = p1$kolor[16], alpha = p1$alpha[16], colour = '#424242') +
        geom_polygon(data = figury[[17]], aes(x = long, y = lat, group = group), fill = '#424242', alpha = 0.9, colour = '#424242') +
        geom_text(data = text1, aes(x = X, y = Y, label = data), colour = 'white', size = 6, family = 'AvantGarde', fontface = 'bold') +
        geom_text(data = text2, aes(x = X, y = Y, label = data), colour = 'white', size = 5, family = 'AvantGarde', fontface = 'bold') +
        geom_text(data = tytul, aes(x = x, y = y, label = dane), colour = 'white', size = 7, family = 'AvantGarde', fontface = 'bold') +
        xlim(-61, 1571) +
        ylim(-11, 1151) +
        theme(line = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              panel.background = element_blank(),
              axis.text.y = element_blank(),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.margin = element_blank(),
              legend.title = element_blank())
      print(plot)
    
    }
  
  
  
  
  
}  
  })
  
  
  output$line_plot1 = renderPlot({
    shinyjs::addClass(selector = "body", class = "sidebar-collapse") 
      input$zaladuj
      plot1 = plot_line1()
      print(plot1)
  })

  output$line_plot2 = renderPlot({
      input$zaladuj
      plot2 = plot_line2()
      print(plot2)
  })
  
  output$lineup = renderText({
    if(input$zawodnik1 != '' & input$zawodnik2 != '' & input$zawodnik3 != '' & input$zawodnik4 != '' & input$zawodnik5 != '') {
    text = paste('Skład:', input$zawodnik1, ',', input$zawodnik2, ',', input$zawodnik3, ',', input$zawodnik4, ',', input$zawodnik5)
}
    print(text)
  })
  
  
  output$plot_asysty = renderPlot({
    dat = dane_asysty_plot()
    dat = dat[dat$Rzucający %in% input$wybierz,]
    tytul = unique(dat$asysta)
    tytul = paste('Rzuty po asystach:', tytul)
    plot = ggplot() +
      annotation_custom(boisko4, -60, 1570, -10, 1150) +
      geom_point(data = dat, aes(x = x, y = y, colour = Rzucający), size = 6, alpha = 0.8) +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      annotate("text", x = 770, y = 1100, label = tytul , size = 8.5, family="sans", fontface="bold", color = "white") +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_blank(),
            legend.position = c(0.87, 0.8),
            legend.background = element_rect(fill="transparent", 
                                             size=0.6, linetype="solid"),
            legend.text = element_text(colour = 'white', size = 14),
            legend.title = element_text(colour = 'white', size = 15))
    print(plot)
  })

  output$asysty_tab = renderDataTable({
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")  
    dane_asysty_tab()
  }, options = list(dom = 't'), rownames = FALSE, class = 'cell-border stripe')
  
output$porownanie_mapa1 = renderPlot({
  p1 = zones_data[zones_data$Zawodnik == input$porownanie_zawodnik1 & zones_data$Sezon == input$porownanie_sezon1,]
  if(nrow(p1) > 16) {
    srednia = p1$avgfgp[1:16]
    Zawodnik = p1$Zawodnik
    Sezon = p1$Sezon
    id = 1:16
    dat2 = lapply(1:16, function(nr) {
      dat1 = p1[p1$id==nr,]
      fgm = sum(dat1$fgm)
      fga = sum(dat1$fga)
      fgp = round((fgm/fga), 2)
      df = data.frame(fgm, fga, fgp)
      return(df)
    })
    dat2 = do.call(rbind.data.frame, dat2)
    dat2$fgp[dat2$fgp=='NaN'] <- 0
    p1 = data.frame(dat2, avgfgp = srednia, id, Zawodnik, Sezon)
  }
  
  tytul = data.frame(x = c(755, 755), y = c(1115, 1060), dane = as.character(''))
  tytul$dane = c(input$porownanie_zawodnik1, input$porownanie_sezon1)
  
  Srodki = readRDS('data/SrodkiFigur.RDS')
  text1 = data.frame(data = p1$fgp, Srodki[,2:3])
  text1$data = paste((text1$data*100), '%', sep = '')
  ##FGM/FGA
  text2 = data.frame(fgm = p1$fgm, fga = p1$fga, Srodki[,2:3])
  text2$data = paste('(', text2$fgm, '/', text2$fga, ')', sep = '')
  text2$Y = text2$Y-40
  text2$id = 1:16
  ids = text2$id[text2$fga==0]
  text2$data[ids] = ''
  text1$data[ids] = ''
  
  p1$minFgp[1:8] = p1$avgfgp[1:8]-0.02
  p1$minFgp[16] = p1$avgfgp[16]-0.02
  p1$minFgp[9:15] = p1$avgfgp[9:15]-0.02 
  p1$maxFgp[1:8] = p1$avgfgp[1:8]+0.02
  p1$maxFgp[16] = p1$avgfgp[16]+0.02
  p1$maxFgp[9:15] = p1$avgfgp[9:15]+0.02
  p1$kolor = ''
  p1$kolor[p1$fgp<p1$minFgp] <- '#e53935'
  p1$kolor[p1$fgp>p1$maxFgp] <- '#43A047'
  p1$kolor[p1$fgp>=p1$minFgp & p1$fgp<= p1$maxFgp] <- '#FFB300'
  p1$alpha = 0.5
  p1$alpha[p1$fga==0] = 0
  
  ##Ladowanie listy figur i ustalanie parametrów mapy
  alpha = 0.5
  plot = ggplot() +
    annotation_custom(boisko4, -60, 1570, -10, 1150)  +
    geom_polygon(data = figury[[1]], aes(x = long, y = lat, group = group), fill = p1$kolor[1], alpha = p1$alpha[1], colour = '#424242') +
    geom_polygon(data = figury[[2]], aes(x = long, y = lat, group = group), fill = p1$kolor[2], alpha = p1$alpha[2], colour = '#424242') +
    geom_polygon(data = figury[[3]], aes(x = long, y = lat, group = group), fill = p1$kolor[3], alpha = p1$alpha[3], colour = '#424242') +
    geom_polygon(data = figury[[4]], aes(x = long, y = lat, group = group), fill = p1$kolor[4], alpha = p1$alpha[4], colour = '#424242') +
    geom_polygon(data = figury[[5]], aes(x = long, y = lat, group = group), fill = p1$kolor[5], alpha = p1$alpha[5], colour = '#424242') +
    geom_polygon(data = figury[[6]], aes(x = long, y = lat, group = group), fill = p1$kolor[6], alpha = p1$alpha[6], colour = '#424242') +
    geom_polygon(data = figury[[7]], aes(x = long, y = lat, group = group), fill = p1$kolor[7], alpha = p1$alpha[7], colour = '#424242') +
    geom_polygon(data = figury[[8]], aes(x = long, y = lat, group = group), fill = p1$kolor[8], alpha = p1$alpha[8], colour = '#424242') +
    geom_polygon(data = figury[[9]], aes(x = long, y = lat, group = group), fill = p1$kolor[9], alpha = p1$alpha[9], colour = '#424242') +
    geom_polygon(data = figury[[10]], aes(x = long, y = lat, group = group), fill = p1$kolor[10], alpha = p1$alpha[10], colour = '#424242') +
    geom_polygon(data = figury[[11]], aes(x = long, y = lat, group = group), fill = p1$kolor[11], alpha = p1$alpha[11], colour = '#424242') +
    geom_polygon(data = figury[[12]], aes(x = long, y = lat, group = group), fill = p1$kolor[12], alpha = p1$alpha[12], colour = '#424242') +
    geom_polygon(data = figury[[13]], aes(x = long, y = lat, group = group), fill = p1$kolor[13], alpha = p1$alpha[13], colour = '#424242') +
    geom_polygon(data = figury[[14]], aes(x = long, y = lat, group = group), fill = p1$kolor[14], alpha = p1$alpha[14], colour = '#424242') +
    geom_polygon(data = figury[[15]], aes(x = long, y = lat, group = group), fill = p1$kolor[15], alpha = p1$alpha[15], colour = '#424242') +
    geom_polygon(data = figury[[16]], aes(x = long, y = lat, group = group), fill = p1$kolor[16], alpha = p1$alpha[16], colour = '#424242') +
    geom_polygon(data = figury[[17]], aes(x = long, y = lat, group = group), fill = '#424242', alpha = 0.9, colour = '#424242') +
    geom_text(data = text1, aes(x = X, y = Y, label = data), colour = 'white', size = 6, family = 'AvantGarde', fontface = 'bold') +
    geom_text(data = text2, aes(x = X, y = Y, label = data), colour = 'white', size = 5, family = 'AvantGarde', fontface = 'bold') +
    geom_text(data = tytul, aes(x = x, y = y, label = dane), colour = 'white', size = 7, family = 'AvantGarde', fontface = 'bold') +
    xlim(-61, 1571) +
    ylim(-11, 1151) +
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.margin = element_blank(),
          legend.title = element_blank())
  if(input$porownanie_zawodnik1 !=  '' & input$porownanie_zawodnik2 != '') {
    print(plot) 
  }
})  

output$porownanie_mapa2 = renderPlot({
  p1 = zones_data[zones_data$Zawodnik == input$porownanie_zawodnik2 & zones_data$Sezon == input$porownanie_sezon2,]
  if(nrow(p1) > 16) {
    srednia = p1$avgfgp[1:16]
    Zawodnik = p1$Zawodnik
    Sezon = p1$Sezon
    id = 1:16
    dat2 = lapply(1:16, function(nr) {
      dat1 = p1[p1$id==nr,]
      fgm = sum(dat1$fgm)
      fga = sum(dat1$fga)
      fgp = round((fgm/fga), 2)
      df = data.frame(fgm, fga, fgp)
      return(df)
    })
    dat2 = do.call(rbind.data.frame, dat2)
    dat2$fgp[dat2$fgp=='NaN'] <- 0
    p1 = data.frame(dat2, avgfgp = srednia, id, Zawodnik, Sezon)
  }
  
  tytul = data.frame(x = c(755, 755), y = c(1115, 1060), dane = as.character(''))
  tytul$dane = c(input$porownanie_zawodnik2, input$porownanie_sezon2)
  
  Srodki = readRDS('data/SrodkiFigur.RDS')
  text1 = data.frame(data = p1$fgp, Srodki[,2:3])
  text1$data = paste((text1$data*100), '%', sep = '')
  ##FGM/FGA
  text2 = data.frame(fgm = p1$fgm, fga = p1$fga, Srodki[,2:3])
  text2$data = paste('(', text2$fgm, '/', text2$fga, ')', sep = '')
  text2$Y = text2$Y-40
  text2$id = 1:16
  ids = text2$id[text2$fga==0]
  text2$data[ids] = ''
  text1$data[ids] = ''
  
  p1$minFgp[1:8] = p1$avgfgp[1:8]-0.02
  p1$minFgp[16] = p1$avgfgp[16]-0.02
  p1$minFgp[9:15] = p1$avgfgp[9:15]-0.02 
  p1$maxFgp[1:8] = p1$avgfgp[1:8]+0.02
  p1$maxFgp[16] = p1$avgfgp[16]+0.02
  p1$maxFgp[9:15] = p1$avgfgp[9:15]+0.02
  p1$kolor = ''
  p1$kolor[p1$fgp<p1$minFgp] <- '#e53935'
  p1$kolor[p1$fgp>p1$maxFgp] <- '#43A047'
  p1$kolor[p1$fgp>=p1$minFgp & p1$fgp<= p1$maxFgp] <- '#FFB300'
  p1$alpha = 0.5
  p1$alpha[p1$fga==0] = 0
  
  ##Ladowanie listy figur i ustalanie parametrów mapy
  alpha = 0.5
  plot = ggplot() +
    annotation_custom(boisko4, -60, 1570, -10, 1150)  +
    geom_polygon(data = figury[[1]], aes(x = long, y = lat, group = group), fill = p1$kolor[1], alpha = p1$alpha[1], colour = '#424242') +
    geom_polygon(data = figury[[2]], aes(x = long, y = lat, group = group), fill = p1$kolor[2], alpha = p1$alpha[2], colour = '#424242') +
    geom_polygon(data = figury[[3]], aes(x = long, y = lat, group = group), fill = p1$kolor[3], alpha = p1$alpha[3], colour = '#424242') +
    geom_polygon(data = figury[[4]], aes(x = long, y = lat, group = group), fill = p1$kolor[4], alpha = p1$alpha[4], colour = '#424242') +
    geom_polygon(data = figury[[5]], aes(x = long, y = lat, group = group), fill = p1$kolor[5], alpha = p1$alpha[5], colour = '#424242') +
    geom_polygon(data = figury[[6]], aes(x = long, y = lat, group = group), fill = p1$kolor[6], alpha = p1$alpha[6], colour = '#424242') +
    geom_polygon(data = figury[[7]], aes(x = long, y = lat, group = group), fill = p1$kolor[7], alpha = p1$alpha[7], colour = '#424242') +
    geom_polygon(data = figury[[8]], aes(x = long, y = lat, group = group), fill = p1$kolor[8], alpha = p1$alpha[8], colour = '#424242') +
    geom_polygon(data = figury[[9]], aes(x = long, y = lat, group = group), fill = p1$kolor[9], alpha = p1$alpha[9], colour = '#424242') +
    geom_polygon(data = figury[[10]], aes(x = long, y = lat, group = group), fill = p1$kolor[10], alpha = p1$alpha[10], colour = '#424242') +
    geom_polygon(data = figury[[11]], aes(x = long, y = lat, group = group), fill = p1$kolor[11], alpha = p1$alpha[11], colour = '#424242') +
    geom_polygon(data = figury[[12]], aes(x = long, y = lat, group = group), fill = p1$kolor[12], alpha = p1$alpha[12], colour = '#424242') +
    geom_polygon(data = figury[[13]], aes(x = long, y = lat, group = group), fill = p1$kolor[13], alpha = p1$alpha[13], colour = '#424242') +
    geom_polygon(data = figury[[14]], aes(x = long, y = lat, group = group), fill = p1$kolor[14], alpha = p1$alpha[14], colour = '#424242') +
    geom_polygon(data = figury[[15]], aes(x = long, y = lat, group = group), fill = p1$kolor[15], alpha = p1$alpha[15], colour = '#424242') +
    geom_polygon(data = figury[[16]], aes(x = long, y = lat, group = group), fill = p1$kolor[16], alpha = p1$alpha[16], colour = '#424242') +
    geom_polygon(data = figury[[17]], aes(x = long, y = lat, group = group), fill = '#424242', alpha = 0.9, colour = '#424242') +
    geom_text(data = text1, aes(x = X, y = Y, label = data), colour = 'white', size = 6, family = 'AvantGarde', fontface = 'bold') +
    geom_text(data = text2, aes(x = X, y = Y, label = data), colour = 'white', size = 5, family = 'AvantGarde', fontface = 'bold') +
    geom_text(data = tytul, aes(x = x, y = y, label = dane), colour = 'white', size = 7, family = 'AvantGarde', fontface = 'bold') +
    xlim(-61, 1571) +
    ylim(-11, 1151) +
    theme(line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.margin = element_blank(),
          legend.title = element_blank())
  if(input$porownanie_zawodnik1 !=  '' & input$porownanie_zawodnik2 != '') {
  print(plot) 
  }
})  
  
 output$porownanie_skutecznosci = renderPlot({
   skut1 = tabela_skutecznosc[tabela_skutecznosc$Zawodnik == input$porownanie_zawodnik1 & tabela_skutecznosc$Sezon == input$porownanie_sezon1,]
   skut2 = tabela_skutecznosc[tabela_skutecznosc$Zawodnik == input$porownanie_zawodnik2 & tabela_skutecznosc$Sezon == input$porownanie_sezon2,]
   skut1 = skut1[,c(1,2,5,8,11,14,17)]
   skut2 = skut2[,c(1,2,5,8,11,14,17)]
   dat = melt(rbind(skut1,skut2))
   dat$Zawodnik = paste(dat$Zawodnik, dat$Sezon)
   dat$Sezon = NULL
   limit = max(dat$value)+5
   dat$text_y = 0
   dat$alpha = 0.8
   zaw_dat = dat
   zaw_dat$alpha[zaw_dat$value==0] <- 0.3
   value_dat = dat
   value_dat$value_y = value_dat$value
   value_dat$value[value_dat$value == 0] <- ''
   p = ggplot(dat, aes(x = variable, y = value, fill = Zawodnik)) + 
     geom_bar(stat = 'identity', position = position_dodge()) +
     geom_text(data = value_dat, aes(label = value, x = variable, y = value_y), position = position_dodge(width = 0.8), hjust = -0.2,
               size = 4) +
     geom_text(data = zaw_dat, aes(label = Zawodnik, x = variable, y = text_y, alpha = alpha), 
               position = position_dodge(width = 0.8), size = 4, hjust = -0.04) +
     scale_y_continuous(breaks = seq(0,100,50), limits = c(0, limit)) +
     coord_flip() + 
     theme_minimal() +
     labs(title = 'Porównanie skuteczności') +
     theme(axis.title.y = element_blank(),
           axis.title.x = element_blank(),
           axis.text.y = element_text(size = 13, margin = margin(r = -12)),
           axis.text.x = element_text(margin = margin(t = 10)),
           plot.title = element_text(size = 27, margin = margin(b = 20, t = 10))) +
     guides(fill = FALSE, alpha = FALSE)
   if(input$porownanie_zawodnik1 !=  '' & input$porownanie_zawodnik2 != '') {
     print(p) 
   }
   
 })

 output$porownanie_statystyk = renderPlot({
   shinyjs::addClass(selector = "body", class = "sidebar-collapse") 
   sub_dat = radar[,c(1,2)]
   sub_dat$index = 1:nrow(sub_dat)
   radar = radar[,-c(1,2)]
   index1 = sub_dat$index[sub_dat$Zawodnik == input$porownanie_zawodnik1 & sub_dat$Sezon == input$porownanie_sezon1]
   index2 = sub_dat$index[sub_dat$Zawodnik == input$porownanie_zawodnik2 & sub_dat$Sezon == input$porownanie_sezon2]
   
   radar %>%
     add_rownames( var = "group" ) %>%
     mutate_each(funs(rescale), -group) -> dat_radar
   
   dat_radar = dat_radar[c(index1, index2),]
   group = c(paste(as.character(sub_dat$Zawodnik[index1]), as.character(sub_dat$Sezon[index1])), 
             paste(as.character(sub_dat$Zawodnik[index2]), as.character(sub_dat$Sezon[index2])))
   group = as.factor(group)
   dat_radar$group = group
   names(dat_radar)[5] = '%FGM AST'
   names(dat_radar)[6] = 'eFG%'
   
   plot = ggradar(dat_radar, axis.label.size = 3.5, grid.label.size = 3.5,
                  legend.text.size = 10.5, group.point.size = 3,
                  gridline.mid.colour = "#D7D6D1", 
                  plot.title = 'Porównanie statystyk rzutowych')
   
   if(input$porownanie_zawodnik1 !=  '' & input$porownanie_zawodnik2 != '') {
     print(plot) 
   }
   
 })


})
  



  




