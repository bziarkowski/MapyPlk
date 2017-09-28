
library(shiny)
library(ggplot2)
library(png)
library(grid)
library(dplyr)
library(DT)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(knitr)
library(rmarkdown)
library(ggradar)
library(scales)

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"


a <- readRDS("data/Rzuty.RDS")
zawodnicy = unique(a$zawodnik)
zawodnicy = c(zawodnicy, '')


dashboardPage(
  skin = 'purple',
  dashboardHeader(title = 'Mapy Rzutów', titleWidth = 200,
  tags$li(class = "dropdown",
          tags$a(href="http://twitter.com/bziarkowski", target="_blank", 
                 tags$img(height = "20px", alt="twitter Logo", src="https://directsoftwareoutlet.com/wp-content/uploads/2016/04/twitterLogo-white.png")
          )),
  tags$li(class = "dropdown",
          tags$a(href='http://www.pulsbasketu.pl/author/bziarkowski/', target="_blank", 
                 tags$img(height = "25px", alt="web Logo", src="http://static.wixstatic.com/media/51092d_1055480a0fe2402dba63bb799c019757.png")
          )),
  tags$li(class = "dropdown",
          tags$a(href='mailto:bziarkowski1@gmail.com', target="_blank", 
                 tags$img(height = "25px", alt="mail Logo", src="http://3g28wn33sno63ljjq514qr87.wpengine.netdna-cdn.com/wp-content/themes/maginess/img/social/email.gif")
          )),
  tags$li(class = "dropdown",
          tags$a(href='http://github.com/bziarkowski/MapyPlk', target="_blank", 
                 tags$img(height = "25px", alt="git Logo", src="http://www.mirkokroese.nl/assets/img/social/github.png")
          ))
  ),
  dashboardSidebar(width = 200,
  sidebarMenu(
    menuItem('Mapy Rzutów', tabName = 'mapy', icon = icon('area-chart')),
    menuItem('Porównanie', tabName = 'porownanie', icon = icon('balance-scale')),
    menuItem('Asysty', tabName = 'asysty', icon = icon('area-chart')),
    menuItem('Statystyki rzutowe', tabName = 'statystyki', icon = icon('table')),
    menuItem('Kreator składów', tabName = 'lineupy', icon = icon('paint-brush'),
             badgeLabel = 'BETA', badgeColor = 'red'),
    menuItem('Wyjaśnienie', tabName = 'opis', icon = icon('info-circle'))
    )),
                   
  dashboardBody(
    useShinyjs(),
    includeCSS("da.css"),
    tabItems(
      tabItem(tabName = 'mapy',
              
  tags$head(includeScript("data/google-analytics.js")),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),


  
fluidRow(style='margin-left:200px;margin-right:200px;',
  column(4,
      selectInput("sezon", "Sezon",
                  choices = c("2012-13", "2013-14", "2014-15",
                "2015-16", "2016-17", "Wszystkie"), 
                selected = "2016-17", width="220px")),
  column(4,
      selectInput("chart", "Typ",
                  choices = c("HeatMapa", "Standardowy", "Strefowa #2", 'Strefowa #1'),
                  selected = "Strefowa #1", width="220px")),
  column(4,
    selectInput("zawodnik", "Zawodnik", "", width="220px"))),
      

      fluidRow(
        column(12, align = 'center',
      withSpinner(plotOutput("plot", height = 620, width = 820)))),

      fluidRow(style='margin-left:250px;margin-right:250px;',
        column(12, align = 'center',
      h1(' ', style = 'margin-bottom:20px;'),
      h1('Skuteczność', style = 'font-family:"Trebuchet MS", Helvetica, sans-serif;'),
      h1(' ', style = 'margin-bottom:20px;'),
      withSpinner(plotlyOutput('barplot')),
      h1(' ', style = 'margin-bottom:40px;'),
      DT::dataTableOutput('table'),
      h1(' ', style = 'margin-bottom:80px;'),
      h1('Statystyki', style = 'font-family:"Trebuchet MS", Helvetica, sans-serif;'),
      h1(' ', style = 'margin-bottom:50px;'),
      withSpinner(plotOutput('boxplot', height = 600)),
      h1(' ', style = 'margin-bottom:80px;'),
      h1('Najbardziej podobni', style = 'font-family:"Trebuchet MS", Helvetica, sans-serif;'),
      h5('(Obliczenia z wykorzystaniem danych z wszystkich sezonów)', style = 'font-family:"Trebuchet MS", Helvetica, sans-serif;'),
      h1(' ', style = 'margin-bottom:20px;'),
      selectInput('metoda', 'Metoda',
                  choices = c('Dystans 2D', 'Wspólne Strefy', 'Średnia Pozycja'),
                 selected = 'Wspólne Strefy'),
      DT::dataTableOutput('porownanie'),
      h1(' ', style = 'margin-bottom:100px;')))),
tabItem(tabName = 'statystyki',
        fluidRow(column(12, align = 'center',
                 selectInput('sezon2', 'Sezon',
                             choices = c('2016-17', '2015-16', '2014-15',
                                         '2013-14', '2012-13', 'Wszystkie'),
                             selected = '2016-17'))),
        fluidRow(style = 'margin-left:100px;margin-right:100px;',
                 column(12, align = 'center',
                 h1('Skuteczność')),
                 h1(' ', style = 'margin-bottom:20px;'),
                 DT::dataTableOutput('skutecznosc'),
                 h1(' ', style = 'margin-bottom:50px;'),
                 column(12, align = 'center',
                 h1('Statystyki rzutowe')),
                 h1(' ', style = 'margin-bottom:20px;'),
                 DT::dataTableOutput('spacing'),
                 h1(' ', style = 'margin-bottom:20px;'))
        
        ),
tabItem(tabName = 'lineupy',
        fluidRow(column(12, align = 'center',
          h1('Kreator składów'),
          h1(' ', style = 'margin-bottom:30px;'))),
        fluidRow(style = 'margin-left:200px;margin-right:200px;',
          column(4,
                 selectInput('zawodnik1', 'Zawodnik #1',
                             choices = zawodnicy, selected = '')),
          column(4,
                 selectInput('zawodnik2', 'Zawodnik #2',
                             choices = zawodnicy, selected = '')),
          column(4,
                 selectInput('zawodnik3', 'Zawodnik #3',
                             choices = zawodnicy, selected = ''))),
        fluidRow(style = 'margin-left:200px;margin-right:200px;',
          column(4,
                 selectInput('zawodnik4', 'Zawodnik #4',
                             choices = zawodnicy, selected = '')),
          column(4,
                 selectInput('zawodnik5', 'Zawodnik #5',
                             choices = zawodnicy, selected = '')),
          column(4, align = 'center',
                 actionButton('zaladuj', 'Załaduj', style = 'margin-top:25px;width:200px;background-color:#673AB7;color:#FAFAFA;'),
                 h6('(może zająć do 20s)'))),
        fluidRow(column(12, align = 'center',
                        h1(' ', style = 'margin-bottom:30px;'),
                        span(textOutput('lineup'), style = 'font-size:25px;'))),
        fluidRow(style = 'margin-left:200px;margin-right:200px;',
                 plotOutput('line_plot1', height = 620, width = 820),
                 plotOutput('line_plot2', height = 620, width = 820))
        ),
tabItem(tabName = 'asysty',
        fluidRow(column(12, align = 'center', h1('Asysty'),
                        h1(' ', style = 'margin-bottom:30px;'))),
        fluidRow(style='margin-left:350px;margin-right:350px;',
                 column(6,
                        selectInput("sezon_2", "Sezon",
                                    choices = c("2012-13", "2013-14", "2014-15",
                                                "2015-16", "2016-17", "Wszystkie"), 
                                    selected = "2016-17", width="220px")),
                 column(6,
                        selectInput("zawodnik_2", "Zawodnik", "", width="220px"))),
        fluidRow(column(12, align = 'center',
                        h1(' ', style = 'margin-bottom:30px;'),
                        withSpinner(plotOutput("plot_asysty", height = 620, width = 820)))),
        fluidRow(style='margin-left:200px;margin-right:200px;',
                 h1(' ', style = 'margin-bottom:30px;'),
          column(9, align = 'center',
        dataTableOutput('asysty_tab')),
        column(3, style='margin-top:7px;',
               checkboxGroupInput('wybierz', 'Wyświetl', ''),
        tags$style("#wybierz {
                    font-size:20px;
                    height:80px;
           }")))
        ),
tabItem(tabName = 'opis',
        fluidRow(style = 'margin-left:200px;margin-right:200px;',
        includeMarkdown('Wyjasnienie.html'))),

tabItem(tabName = 'porownanie',
        fluidRow(style = 'margin-left:300px;margin-right:300px', align = 'center',
                 h1('Porównanie zawodników')),
        fluidRow(style='margin-left:50px;margin-right:50px;',
                 column(6, align = 'center',
                        h4('Zawodnik #1'),
                        selectInput("porownanie_sezon1", "Sezon",
                                    choices = c("2012-13", "2013-14", "2014-15",
                                                "2015-16", "2016-17", "Wszystkie"), 
                                    selected = "2016-17", width="220px")),
                 column(6, align = 'center',
                        h4('Zawodnik #2'),
                        selectInput("porownanie_sezon2", "Sezon",
                                    choices = c("2012-13", "2013-14", "2014-15",
                                                "2015-16", "2016-17", "Wszystkie"), 
                                    selected = "2016-17", width="220px"))),
        fluidRow(style = 'margin-left:50px;margin-right:50px;',
                 column(6, align = 'center',
                        selectInput("porownanie_zawodnik1", "Zawodnik", "", width="220px")),
                 column(6, align = 'center',
                        selectInput("porownanie_zawodnik2", "Zawodnik", "", width="220px"))),
        fluidRow(style = 'margin-left:10px;margin-right:10px',
                 h1(' ', style = 'margin-bottom:10px'),
                 column(6, align = 'center',
                        plotOutput('porownanie_mapa1', height = 434, width = 574)
                        ),
                 column(6, align = 'center',
                        plotOutput('porownanie_mapa2', height = 434, width = 574))),
        
        fluidRow(style = 'margin-left:10px;margin-right:10px', algin = 'center',
                 h1(' ', style = 'margin-bottom:20px'),
                 column(6, align = 'center',
                        h1(' ', style = 'margin-bottom:45px'),
                        plotOutput('porownanie_skutecznosci', height = 434, width = 574)),
                 column(6, align = 'center',
                        h1(' ', style = 'margin-bottom:50px'),
                        plotOutput('porownanie_statystyk', height = 434, width = 574)),
                 h1(' ', style = 'margin-bottom:50px')
                 )
        )

)
)
)
  
  

