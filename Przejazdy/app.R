pacman::p_load(
  shiny,
  bslib,
  RColorBrewer, 
  ggplot2, 
  stringr,
  lubridate,
  sf,
  rmapshaper
)

# Załadowanie danych
load("C:/Users/Iga/OneDrive/Studia - materiały/# Informatyka i Ekonometria/0.4 SEMESTR IV/Zaawansowane metody wizualizacji i raportowania danych/R/Projekt-zaliczeniowy-Wizualizacje-w-R/przejazdy.RData")

load("C:/Users/Iga/OneDrive/Studia - materiały/# Informatyka i Ekonometria/0.4 SEMESTR IV/Zaawansowane metody wizualizacji i raportowania danych/R/Projekt-zaliczeniowy-Wizualizacje-w-R/punkty_pomiarowe.RData")

paleta_kolorow <- brewer.pal(9, "RdPu")

# Agregacje i nowe zmienne
przejazdy$Miesiac <- month(przejazdy$Data, 
                           label = TRUE, 
                           abbr = FALSE)

przejazdy$DzienTyg <- wday(przejazdy$Data, 
                           label = TRUE, 
                           abbr = FALSE, 
                           week_start = 1)

przejazdy$TypDnia <- ifelse(wday(przejazdy$Data, week_start = 1) %in% c(6, 7), 
                            "Weekend", "Dzień powszedni")

przejazdy$Czy_Pada <- ifelse(przejazdy$Opady_dzień > 0, 
                             "Deszcz", "Brak opadu")

# Porządkowanie stacji wg mediany
stacje_order <- aggregate(Licznik ~ Stacja, 
                          data = przejazdy, 
                          median)

stacje_order <- stacje_order$Stacja[order(stacje_order$Licznik)]

przejazdy$Stacja_f <- factor(przejazdy$Stacja, 
                             levels = stacje_order)

# Lista stacji do interfejsu (unikalne wartości do filtra)
lista_stacji <- unique(przejazdy$Stacja)

# Lista zmiennych pogodowych
zmienne_pogodowe <- c(
  "Temperatura", 
  "Wiatr", 
  "Zachmurzenie", 
  "Wilgotność", 
  "Ciśnienie_woda", 
  "Ciśnienie_stacja", 
  "Ciśnienie_morze", 
  "Opady_dzień", 
  "Opady_noc"
)

# Kafelki
liczba_stacji <- length(unique(przejazdy$Stacja))

suma_przejazdow <- sum(przejazdy$Licznik, 
                       na.rm = TRUE)

suma_na_stacje <- aggregate(Licznik ~ Stacja, 
                            data = przejazdy, 
                            sum)

naj_stacja <- suma_na_stacje$Stacja[which.max(suma_na_stacje$Licznik)]

# Pobranie mapy Gdańska
mapa.gdansk <- st_read(
  dsn = "C:/Users/Iga/OneDrive/Studia - materiały/# Informatyka i Ekonometria/0.4 SEMESTR IV/Zaawansowane metody wizualizacji i raportowania danych/R/Projekt-zaliczeniowy-Wizualizacje-w-R/dzielnice/Dzielnice.shp"
)

# Usunięcie Zatoki Gdańskiej
mapa.gdansk <- mapa.gdansk[mapa.gdansk$NAZWA != "Zatoka Gdańska", ]

# Uproszczenie granic
mapa.gdansk <- ms_simplify(
  input = mapa.gdansk,
  keep = 0.02,
  keep_shapes = T, # zapewnienie spójnych obszarów (bez luk pomiędzy) pomimo uproszczenia granic
  weighting = 0.7
)

# Suma przejazdów
stat_suma <- aggregate(Licznik ~ Stacja, 
                       data = przejazdy, 
                       sum)

colnames(stat_suma)[2] <- "Suma"

# Mediana przejazdów 
stat_mediana <- aggregate(Licznik ~ Stacja, 
                          data = przejazdy, 
                          median)

colnames(stat_mediana)[2] <- "Mediana"

# Złączenie statystyk w jedną tabelę
statystyki_stacji <- merge(stat_suma, 
                           stat_mediana, 
                           by = "Stacja")

# Połączenie statystyk z plikiem z punktami
punkty_sf <- merge(punkty, 
                   statystyki_stacji, 
                   by.x = "stacja", # w pliku z punktami stacja z małej litery
                   by.y = "Stacja" # a z przejazdami z wielkiej
)

# Interfejs użytkownika (UI)
ui <- page_navbar(
  title = "Ruch rowerowy w Gdańsku",
  theme = bs_theme(bg = "white", 
                   fg = "#202123"),

  
  # ZAKŁADKA 1 (ogólne statystyki):
  
  nav_panel(
    title = "Ogólne statystyki",
    
    # Kafelki
    layout_columns(
      fill = FALSE,
      max_height = "130px",
      
      # Liczba wszystkich aktywnych stacji pomiarowych
      value_box(
        title = "Aktywne stacje pomiarowe",
        value = liczba_stacji,
        theme = "secondary",
      ),
      
      # Łączna liczba przejazdów 
      value_box(
        title = "Łączna liczba przejazdów",
        value = format(suma_przejazdow, 
                       big.mark = " ", # separator tysięczny
                       scientific = FALSE
                       ),
        theme = "info"
      ),
      
      # Najpopularniejsza stacja
      value_box(
        title = "Najpopularniejsza stacja",
        value = tags$span(naj_stacja, 
                          style = "font-size: 2.5em;" # zmniejszenie czcionki
                          ),
        theme = value_box_theme(bg = "#C51B7D", 
                                fg = "white"
                                ),
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      
      # Wykres (Pkt) 1: Rozkład liczby dni pomiarowych w poszczególnych punktach
      card(
        card_header("Rozkład liczby dni pomiarowych w poszczególnych punktach"),
        card_body(plotOutput("wykres1", 
                             height = "800px"))
      ),
      
      # Wykres (Pkt) 3: Porównanie natężenia we wszystkich punktach
      card(
        card_header("Porównanie natężenia we wszystkich punktach"),
        card_body(plotOutput("wykres3", 
                             height = "800px"))
      )
    )
  ),
  
  # ZAKŁADKA 2 (analiza w czasie):
  
  nav_panel(
    title = "Analiza w czasie",
    
    # Pasek boczny do filtrowania
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        bg = "#e0e0e0",
        
        h4("Pojedyncza stacja"),
        
        # Filtr wyboru pojedynczej stacji
        selectInput("wybrana_stacja_czas", 
                    "Wybierz stację:", 
                    choices = lista_stacji, 
                    selected = "ul. Chłopska"),
        
        hr(class = "my-1"),
        h4("Porównanie stacji"),
        
        # Filtr wyboru kilku stacji
        selectizeInput("wybrane_stacje_por", 
                       "Wybierz stacje do porównania:", 
                       choices = lista_stacji, 
                       multiple = TRUE, 
                       selected = c("ul. Kartuska", 
                                    "al. Grunwaldzka (UG)", 
                                    "ul. Chłopska")),
        
        hr(class = "my-1"),
        h4("Wymiar czasu"),
        
        # Filtr wymiaru czasu
        selectInput("wymiar_czasu", 
                    "Pokaż natężenie według:", 
                    choices = c("Miesiąc" = "Miesiac", 
                                "Dzień tygodnia" = "DzienTyg", 
                                "Typ dnia (Weekend/Powszedni)" = "TypDnia"),
                    selected = "Miesiac"),
      ),
      
      layout_columns(
        col_widths = c(5, 7),
        
        # Wykres (Pkt) 2: Rozkład liczby przejazdów dla wybranego punktu
        card(
          card_header("Rozkład liczby przejazdów dla wybranego punktu"),
          card_body(plotOutput("wykres2", 
                               height = "400px"))
        ),
        
        # Wykres (Pkt) 4: Sezonowość przejazdów dla wybranego punktu
        card(
          card_header("Sezonowość przejazdów dla wybranego punktu"),
          card_body(plotOutput("wykres4", 
                               height = "400px"))
        )
      ),
      
        # Wykres (Pkt) 5: Porównanie natężenia w czasie między stacjami
        card(
          card_header("Porównanie natężenia w czasie między stacjami"),
          card_body(plotOutput("wykres5", 
                              height = "600px"))
      )
    )
  ),
  
  # ZAKŁADKA 3 (warunki pogodowe):
  
  nav_panel(
    title = "Warunki pogodowe",
    
    # Pasek boczny do filtrowania
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        bg = "#e0e0e0",
        
        h4("Pojedyncza stacja"),
        
        # Filtr wyboru pojedynczej stacji
        selectInput("wybrana_stacja_pogoda", 
                    "Wybierz stację:", 
                    choices = lista_stacji, 
                    selected = "ul. Chłopska"),
        
        
        hr(class = "my-1"),
        h4("Porównanie stacji"),
        
        # Filtr wyboru kilku stacji
        selectizeInput("wybrane_stacje_pogoda_por", 
                       "Wybierz stacje do porównania:", 
                       choices = lista_stacji, 
                       multiple = TRUE, 
                       selected = c("ul. Kartuska", 
                                    "al. Grunwaldzka (UG)", 
                                    "ul. Chłopska")),
        
        hr(class = "my-1"),
        h4("Warunki pogodowe"),
        
        # Filtr wyboru warunków pogodowych
        radioButtons("parametr_pogody", 
                     "Analizowany parametr:",
                     choices = c("Temperatura", 
                                 "Opady deszczu", 
                                 "Opady (uproszczone)"))
      ),
      
      # Wykres (Pkt) 6: Zależność przejazdów od pogody dla wybranej stacji
      card(
        card_header("Zależność przejazdów od pogody dla wybranej stacji"),
        card_body(plotOutput("wykres6", 
                             height = "400px"))
      ),
      
      # Wykres (Pkt 7): Porównanie wrażliwości stacji na wybrane warunki pogodowe
      card(
        card_header("Porównanie wrażliwości stacji na wybrane warunki pogodowe"),
        card_body(plotOutput("wykres7", 
                             height = "500px"))
      )
    )
  ),
  
  # ZAKŁADKA 4 (mapa):
  
  nav_panel(
    title = "Mapa",
    layout_columns(
      col_widths = c(6, 6),
      
      # Wykresy (Pkt 8): Mapy z wybranymi statystykami dotyczącymi przejazdów w punktach
      card(
        card_header("Natężenie ruchu rowerowego w punktach pomiarowych na mapie Gdańska"),
        card_body(plotOutput("wykres8_1", 
                             height = "500px"))
      ),
      card(
        card_header("Typowe dzienne natężenie ruchu rowerowego na mapie Gdańska"),
        card_body(plotOutput("wykres8_2", 
                             height = "500px"))
      )
    )
  )
  
  # ZAKŁADKA 5 (zależności pogodowe):
  , # <-- ten przecinek łączy tę zakładkę z poprzednią!
  nav_panel(
    title = "Zależności pogodowe",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        bg = "#e0e0e0",
        
        h4("Warunki pogodowe"),
        hr(class = "my-1"),
        
        # Wybór zmiennej na oś X
        selectInput("pogoda_x", 
                    "Zmienna na osi X:", 
                    choices = zmienne_pogodowe, 
                    selected = "Temperatura"),
        
        # Wybór zmiennej na oś Y
        selectInput("pogoda_y", 
                    "Zmienna na osi Y:", 
                    choices = zmienne_pogodowe, 
                    selected = "Wilgotność")
      ),
      
      # Wykres (Pkt) 9: Zależności między zmiennymi opisującymi warunki pogodowe
      card(
        card_header("Zależność między wybranymi zmiennymi opisującymi warunki pogodowe"),
        card_body(plotOutput("wykres9", height = "600px"))
      )
    )
  )
)


# Serwer
server <- function(input, output, session) {
  
  # Wykres (Pkt) 1: Rozkład liczby dni pomiarowych w poszczególnych punktach
  output$wykres1 <- renderPlot({
    ggplot(przejazdy, aes(x = reorder(Stacja, Stacja, function(x) length(x)))) +
      geom_bar(fill = "#C51B7D", 
               width = 0.6,
               alpha = 0.8) +
      labs(x = "Punkt pomiarowy",
           y = "Liczba dni") +
      theme_light() +
      theme(
        axis.text.x = element_text(hjust = 0.5, size = 8),
        plot.title = element_text(hjust = 0.5),
      ) + coord_flip() 
  })
  
  # Wykres (Pkt) 3: Porównanie natężenia we wszystkich punktach
  output$wykres3 <- renderPlot({
    ggplot(przejazdy, aes(x = Stacja_f, y = Licznik)) +
      geom_boxplot(fill = "#C51B7D", 
                   outlier.color = "#DE77AE", 
                   outlier.size = 0.5,
                   alpha = 0.8) +
      coord_flip(ylim = c(0, 4500)) +
      labs(x = "Punkt pomiarowy",
           y = "Liczba przejazdów dziennie") +
      theme_light() +
      theme(
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_blank()
        )
  })
  
  dane_stacja_czas <- reactive({
    przejazdy[przejazdy$Stacja == input$wybrana_stacja_czas, ]
  })
  
  # Wykres (Pkt) 2: Rozkład liczby przejazdów dla wybranego punktu
  output$wykres2 <- renderPlot({
    ggplot(dane_stacja_czas(), aes(x = Licznik)) +
      geom_histogram(bins = 20, 
                     fill = "#C51B7D", 
                     alpha = 0.7) +
      labs(title = paste("Rozkład liczby przejazdów -", input$wybrana_stacja_czas),
           x = "Liczba przejazdów dziennie",
           y = "Gęstość") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # Wykres (Pkt) 4: Sezonowość przejazdów dla wybranego punktu
  output$wykres4 <- renderPlot({
    dane <- dane_stacja_czas()
    
    # Filtr wymiaru czasu
    etykieta <- names(which(c("Miesiac" = "Miesiąc", 
                              "DzienTyg" = "Dzień tygodnia", 
                              "TypDnia" = "Typ dnia") == input$wymiar_czasu))
    
    ggplot(dane, aes(x = .data[[input$wymiar_czasu]], 
                     y = Licznik, 
                     fill = .data[[input$wymiar_czasu]])) +
      geom_boxplot(alpha = 0.6, 
                   show.legend = FALSE) +
      stat_summary(fun = "mean", 
                   geom = "point", 
                   color = "black", 
                   size = 2, 
                   show.legend = FALSE) +
      labs(title = paste("Sezonowość przejazdów -", input$wybrana_stacja_czas),
           x = etykieta,
           y = "Liczba przejazdów dziennie") +
      theme_light() +
      scale_fill_manual(values = colorRampPalette(paleta_kolorow)(length(unique(dane[[input$wymiar_czasu]])))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Wykres (Pkt) 5: Porównanie natężenia w czasie między stacjami
  output$wykres5 <- renderPlot({
    req(input$wybrane_stacje_por)
    dane_porownanie <- przejazdy[przejazdy$Stacja %in% input$wybrane_stacje_por, ]
    dane_porownanie$Stacja <- factor(dane_porownanie$Stacja, 
                                     levels = input$wybrane_stacje_por)
    
    ile_stacji <- length(input$wybrane_stacje_por)
    kolory_dynamiczne <- colorRampPalette(c(paleta_kolorow[4], paleta_kolorow[8]))(ile_stacji)
    
    slownik_etykiet <- c("Miesiac" = "miesiąc", 
                         "DzienTyg" = "dzień tygodnia", 
                         "TypDnia" = "typ dnia")
    
    etykieta <- slownik_etykiet[input$wymiar_czasu]
    
    ggplot(dane_porownanie, aes(x = .data[[input$wymiar_czasu]], 
                                y = Licznik, 
                                fill = Stacja)) +
      geom_boxplot(alpha = 0.7, 
                   show.legend = FALSE, 
                   outlier.size = 0.5) +
      stat_summary(fun = "mean", 
                   geom = "point", 
                   color = "white", 
                   shape = 18, 
                   size = 2, 
                   show.legend = FALSE) +
      facet_wrap(vars(Stacja), 
                 ncol = 1, 
                 scales = "free_y") + 
      scale_fill_manual(values = kolory_dynamiczne) +
      
      labs(x = stringr::str_to_sentence(etykieta),
           y = "Dzienny licznik przejazdów") +
      
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(color = "black", face = "bold"),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Wykres (Pkt) 6: Zależność przejazdów od pogody dla wybranej stacji
  output$wykres6 <- renderPlot({
    dane_stacja <- przejazdy[przejazdy$Stacja == input$wybrana_stacja_pogoda, ]
    
    if(input$parametr_pogody == "Temperatura") {
      ggplot(dane_stacja, aes(x = Temperatura, 
                              y = Licznik)) +
        geom_point(color = paleta_kolorow[5], 
                   alpha = 0.5) +
        geom_smooth(method = "loess", 
                    color = paleta_kolorow[8], 
                    fill = paleta_kolorow[2]) +
        labs(title = "Wpływ temperatury na natężenie ruchu",
             subtitle = paste("Stacja:", input$wybrana_stacja_pogoda),
             x = "Średnia dobowa temperatura [°C]",
             y = "Liczba przejazdów") +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5))
      
    } else if(input$parametr_pogody == "Opady deszczu") {
      ggplot(dane_stacja, aes(x = Opady_dzień, 
                              y = Licznik)) +
        geom_point(color = paleta_kolorow[4], 
                   alpha = 0.4) +
        geom_smooth(method = "loess", 
                    color = paleta_kolorow[7], 
                    fill = paleta_kolorow[2], 
                    fullrange = FALSE) +
        labs(title = "Wpływ opadów na natężenie ruchu",
             subtitle = paste("Stacja:", input$wybrana_stacja_pogoda),
             x = "Suma opadu dzień [mm]", 
             y = "Liczba przejazdów") +
        theme_light() + expand_limits(y = 0) + coord_cartesian(ylim = c(0, NA)) + 
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5))
      
    } else {
      ggplot(dane_stacja, aes(x = Czy_Pada, 
                              y = Licznik, 
                              fill = Czy_Pada)) +
        geom_violin(alpha = 0.5, 
                    show.legend = FALSE) +
        geom_boxplot(width = 0.1, 
                     outlier.shape = NA, 
                     show.legend = FALSE) +
        stat_summary(fun = "mean", 
                     geom = "point", 
                     color = "black", 
                     size = 2, 
                     show.legend = FALSE) +
        labs(title = "Wpływ deszczu na natężenie ruchu",
             subtitle = paste("Stacja:", input$wybrana_stacja_pogoda),
             x = "", 
             y = "Liczba przejazdów") +
        scale_fill_manual(values = c(paleta_kolorow[3], 
                                     paleta_kolorow[6])) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5))
    }
  })
  
  # Wykres (Pkt 7): Porównanie wrażliwości stacji na pogodę
  output$wykres7 <- renderPlot({
    req(input$wybrane_stacje_pogoda_por)
    dane_por_pogoda <- przejazdy[przejazdy$Stacja %in% input$wybrane_stacje_pogoda_por, ]
    dane_por_pogoda$Stacja <- factor(dane_por_pogoda$Stacja, 
                                     levels = input$wybrane_stacje_pogoda_por)
    
    ile_stacji <- length(input$wybrane_stacje_pogoda_por)
    kolory_dynamiczne <- colorRampPalette(c(paleta_kolorow[4], paleta_kolorow[8]))(ile_stacji)
    
    if(input$parametr_pogody %in% c("Temperatura", "Opady (uproszczone)")) {
      ggplot(dane_por_pogoda, aes(x = Temperatura, 
                                  y = Licznik, 
                                  color = Stacja)) +
        geom_point(alpha = 0.3, 
                   show.legend = FALSE) +
        geom_smooth(method = "loess", 
                    color = "black", 
                    se = FALSE, 
                    size = 1) +
        facet_wrap(vars(Stacja), 
                   ncol = 3, 
                   scales = "free_y") +
        scale_color_manual(values = kolory_dynamiczne) +
        expand_limits(y = 0) + coord_cartesian(ylim = c(0, NA)) +
        labs(title = "Porównanie wrażliwości stacji na temperaturę",
             subtitle = "Wykorzystanie wygładzania lokalnego (loess) dla zachowania realizmu",
             x = "Średnia dobowa temperatura [°C]", 
             y = "Liczba przejazdów") +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              strip.text = element_text(face = "bold", color = "black"),
              strip.background = element_rect(fill = "grey90"))
    } else {
      ggplot(dane_por_pogoda, aes(x = Opady_dzień, 
                                  y = Licznik, 
                                  color = Stacja)) +
        geom_point(alpha = 0.3, 
                   show.legend = FALSE) +
        geom_smooth(method = "loess", 
                    color = "black", 
                    se = FALSE, 
                    size = 1) +
        facet_wrap(vars(Stacja), 
                   ncol = 3, 
                   scales = "free_y") +
        scale_color_manual(values = kolory_dynamiczne) +
        expand_limits(y = 0) + coord_cartesian(ylim = c(0, NA)) +
        labs(title = "Porównanie wrażliwości stacji na opady deszczu",
             subtitle = "Krzywa loess pokazuje tempo spadku liczby rowerzystów",
             x = "Suma opadu dzień [mm]", 
             y = "Liczba przejazdów") +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5),
              strip.text = element_text(face = "bold", color = "black"),
              strip.background = element_rect(fill = "grey90"))
    }
  })
  
  output$wykres8_1 <- renderPlot({
    ggplot() +
      geom_sf(data = mapa.gdansk, fill = "grey90", color = "darkgray") +
      
      # Kartogram punktowy
      geom_sf(data = punkty_sf, 
              aes(fill = Suma), # kolor zależy od liczby przejazdów
              size = 6,
              shape = 21,
              color = "grey40",
              alpha = 0.9) +                 
      
      theme_void() + 
      
      # Skala przedziałowa (skokowa)
      scale_fill_fermenter(
        name = "Łączna liczba\nprzejazdów",
        palette = "RdPu",                    
        direction = 1,                       
        # Własne granice, żeby lepiej było widać różnice
        breaks = c(250000, 500000, 1000000, 2000000, 4000000, 6000000), 
        labels = function(x) format(x, big.mark = " ", scientific = FALSE) 
      ) +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  
  output$wykres8_2 <- renderPlot({
    ggplot() +
      geom_sf(data = mapa.gdansk, fill = "grey90", color = "darkgray") +
      
      # Kartogram punktowy
      geom_sf(data = punkty_sf, 
              aes(fill = Mediana), # kolor zależy od mediany
              size = 6,
              shape = 21,
              color = "grey40",
              alpha = 0.9) +                 
      
      theme_void() + 
      
      # Skala przedziałowa (skokowa)
      scale_fill_fermenter(
        name = "Mediana dziennej\nliczby przejazdów", 
        palette = "RdPu",                 
        direction = 1,                       
        # Własne granice, żeby lepiej było widać różnice
        breaks = c(200, 400, 600, 1000, 1500, 2000), 
        labels = function(x) format(x, big.mark = " ", scientific = FALSE) 
      ) +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  
  # Wykres (Pkt) 9: Zależności między zmiennymi opisującymi warunki pogodowe
  output$wykres9 <- renderPlot({
    
    zmienna_x <- input$pogoda_x
    zmienna_y <- input$pogoda_y
    
    ggplot(przejazdy, aes(x = .data[[zmienna_x]], 
                          y = .data[[zmienna_y]])) +
      geom_point(color = paleta_kolorow[6], 
                 alpha = 0.3) +
      geom_smooth(method = "loess", 
                  color = "black", 
                  fill = "grey80",
                  se = FALSE) +
      labs(
        x = str_replace_all(zmienna_x, "_", " "), # usunięcie "podłogi" z nazw osi
        y = str_replace_all(zmienna_y, "_", " ")
      ) +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
}

shinyApp(ui, server)