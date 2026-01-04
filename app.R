# app.R

# ==================================
# 1. MEMUAT LIBRARY
# ==================================
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl) 
library(DT) 

# ========================================================
# PEMUATAN DATA (Global Scope)
# ========================================================
# PASTIKAN FILE stroke_preprocessed.xlsx ADA DI DIRECTORY UTAMA
stroke_data <- read_excel("stroke_preprocessed.xlsx", sheet = "Sheet1")
stroke_data$gender <- as.factor(stroke_data$gender)
stroke_data$stroke <- as.factor(stroke_data$stroke)
stroke_data$hypertension <- as.factor(stroke_data$hypertension)
stroke_data$heart_disease <- as.factor(stroke_data$heart_disease)

# Membuat kolom baru untuk kombinasi faktor risiko
stroke_data <- stroke_data %>%
  mutate(Risk_Group = case_when(
    hypertension == 1 & heart_disease == 1 ~ "Hipertensi & Jantung",
    hypertension == 1 & heart_disease == 0 ~ "Hanya Hipertensi",
    hypertension == 0 & heart_disease == 1 ~ "Hanya Jantung",
    TRUE ~ "Tanpa Penyakit"
  ))
stroke_data$Risk_Group <- as.factor(stroke_data$Risk_Group)

# Menghilangkan baris dengan gender 'Other'
stroke_data <- stroke_data %>% filter(gender != "Other") 
# ========================================================

# ==================================
# 2. DEFINISI SERVER (Logika Aplikasi)
# ==================================

server <- function(input, output) {
  
  # --- PENGHITUNGAN UMUM ---
  output$total_data <- renderText({
    paste("Total Data Pasien Stroke: ", nrow(stroke_data))
  })
  
  # ------------------------------------------------------------------
  # LOGIKA SUMMARY STATISTICS
  # ------------------------------------------------------------------
  
  process_summary_numeric <- function(col_data, col_name) {
    s <- summary(col_data)
    metrics <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    values <- as.character(unname(s[metrics]))
    return(data.frame(Variable = rep(col_name, 6), Metric = metrics, Value = values, stringsAsFactors = FALSE))
  }
  
  output$summary_numeric <- renderDataTable({
    numeric_cols <- stroke_data %>% select(age, avg_glucose_level, bmi)
    summary_list <- lapply(names(numeric_cols), function(name) {
      process_summary_numeric(numeric_cols[[name]], name)
    })
    summary_df <- do.call(rbind, summary_list) 
    
    datatable(summary_df, rownames = FALSE, options = list(dom = 't', autoWidth = TRUE)) %>%
      formatStyle('Variable', target = 'row', backgroundColor = '#EE6983', color = 'white', fontWeight = 'bold') %>%
      formatRound('Value', digits = 2)
  })
  
  output$summary_categorical <- renderDataTable({
    categorical_cols <- stroke_data %>% 
      select(gender, hypertension, heart_disease, ever_married, work_type, Residence_type, smoking_status, stroke)
    
    summary_list <- lapply(names(categorical_cols), function(col_name) {
      t <- table(categorical_cols[[col_name]])
      p <- round(prop.table(t) * 100, 2)
      
      df <- data.frame(Variable = col_name, Category = names(t), Count = as.integer(t),
                       Percentage = paste0(p, "%"), stringsAsFactors = FALSE)
      return(df)
    })
    
    final_df <- do.call(rbind, summary_list) 
    
    datatable(final_df, rownames = FALSE, options = list(pageLength = 15, dom = 'tp', autoWidth = TRUE)) %>%
      formatStyle('Variable', target = 'row', backgroundColor = '#FFC4C4', fontWeight = 'bold')
  })
  
  # ------------------------------------------------------------------
  # LOGIKA VISUALISASI
  # ------------------------------------------------------------------
  
  data_filtered <- reactive({
    stroke_data %>%
      filter(gender == input$gender_filter)
  })
  
  # 1. Plot Distribusi Glukosa (Histogram)
  output$visual_plot_glucose <- renderPlot({ 
    color_palette <- c("0" = "#FFC4C4", "1" = "#850E35") 
    
    ggplot(data_filtered(), aes(x = avg_glucose_level, fill = stroke)) +
      geom_histogram(binwidth = 10, position = "stack", color = "white") +
      labs(
        title = paste("1. Distribusi Glukosa berdasarkan Status Stroke (Gender:", input$gender_filter, ")"),
        x = "Rata-rata Tingkat Glukosa",
        y = "Frekuensi",
        fill = "Stroke"
      ) +
      scale_fill_manual(values = color_palette) + 
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16), axis.title = element_text(size = 12))
  })
  
  # 2. Plot Distribusi BMI (Box Plot)
  output$visual_plot_bmi <- renderPlot({
    
    ggplot(data_filtered(), aes(x = stroke, y = bmi, fill = stroke)) +
      geom_boxplot(outlier.shape = 16, outlier.colour = "#850E35") +
      scale_fill_manual(values = c("0" = "#FFC4C4", "1" = "#EE6983")) +
      labs(
        title = paste("2. Distribusi BMI berdasarkan Status Stroke (Gender:", input$gender_filter, ")"),
        x = "Status Stroke (0: Tidak, 1: Ya)",
        y = "Indeks Massa Tubuh (BMI)",
        fill = "Stroke"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16), axis.title = element_text(size = 12))
  })
  
  # 3. Plot Hubungan Glukosa dengan Faktor Risiko Lain
  output$visual_plot_risk <- renderPlot({
    stroke_data_binned <- stroke_data %>%
      mutate(Glucose_Bin = cut(avg_glucose_level, 
                               breaks = c(50, 100, 150, 200, Inf),
                               labels = c("Normal (<100)", "Sedang (100-150)", "Tinggi (150-200)", "Sangat Tinggi (>200)")))
    
    plot_data <- stroke_data_binned %>%
      filter(gender == input$gender_filter)
    
    ggplot(plot_data, aes(x = Glucose_Bin, fill = Risk_Group)) +
      geom_bar(position = "fill", color = "white") +
      scale_fill_manual(values = c("Tanpa Penyakit" = "#FCF5EE", "Hanya Hipertensi" = "#FFC4C4", 
                                   "Hanya Jantung" = "#EE6983", "Hipertensi & Jantung" = "#850E35")) +
      labs(
        title = paste("3. Proporsi Faktor Risiko berdasarkan Tingkat Glukosa (Gender:", input$gender_filter, ")"),
        x = "Kelompok Tingkat Glukosa",
        y = "Proporsi",
        fill = "Kelompok Risiko"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 14), axis.title = element_text(size = 12))
  })
  
  # Output Tabel Cuplikan Data
  output$stroke_table <- renderDataTable({
    stroke_data %>%
      select(-id) %>%
      head(10) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
}

# ==================================
# 3. DEFINISI UI (Tampilan)
# ==================================

ui <- navbarPage(
  # Judul utama di NavBar (Hanya Icon dan Teks)
  title = span(
    icon("brain"), 
    "Analisis Data Stroke"
  ), 
  
  # CSS dan Palet Warna
  header = tags$head(
    tags$style(HTML(paste0("
          /* Palet Warna: 850E35, EE6983, FFC4C4, FCF5EE */
          .navbar { background-color: #EE6983; border-color: #850E35; }
          .navbar-default .navbar-brand { color: #850E35; } 
          .navbar-default .navbar-nav > li > a { color: #850E35; } 
          
          .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus {
              color: white; background-color: #850E35; 
          }
          .navbar-default .navbar-nav > li > a:hover {
              color: white; background-color: #FFC4C4; 
          }
          body { background-color: #FCF5EE; }
          .well { background-color: #FFC4C4; border-color: #EE6983; }
          .btn-primary { background-color: #850E35; border-color: #850E35; }
          .btn-primary:hover { background-color: #EE6983; border-color: #850E35; }
      "))),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans")
  ),
  
  # -----------------------------------------------------
  # TAB 1: HOME 
  # -----------------------------------------------------
  tabPanel("🏠 Home",
           # KODE LOGO ITS/STATISTIKA (MENGGUNAKAN URL EKSTERNAL)
           fluidRow(
             column(width = 12,
                    tags$div(style = "text-align: center; margin-top: 20px; margin-bottom: 30px;",
                             # LOGO 1: ITS (URL EKSTERNAL)
                             tags$img(src = "https://www.its.ac.id/wp-content/uploads/2020/07/Logo-ITS-1-300x185.png", 
                                      height = "60px", width = "90px", 
                                      style = "margin-right: 30px;"),
                             # LOGO 2: STATISTIKA ITS (URL EKSTERNAL)
                             tags$img(src = "https://www.its.ac.id/statistika/wp-content/uploads/sites/43/2018/03/logo-statistika-white-border.png", 
                                      height = "60px", width = "60px", 
                                      style = "margin-left: 30px;")
                    )
             )
           ),
           
           fluidRow(
             column(width = 8,
                    h1("Selamat Datang! 👋", style = "color: #850E35;"),
                    h2("Dashboard Analisis Data Stroke"),
                    p("Dashboard ini menyajikan ringkasan dan visualisasi data pasien stroke yang tersedia di Kaggle. Gunakan menu navigasi di atas untuk menjelajahi berbagai analisis."),
                    tags$a(href = "https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset", "Pelajari lebih lanjut tentang dataset di Kaggle", target = "_blank"),
                    hr(style = "border-top: 1px solid #EE6983;"),
                    h3("Apa Itu Stroke?"),
                    p("Stroke adalah gangguan pergerakan progresif pada sistem saraf. Ini menyebabkan sel-sel saraf (neuron) di bagian otak melemah, rusak, dan mati, yang mengarah pada gejala-gejala seperti masalah pergerakan, tremor, kekakuan, dan gangguan keseimbangan. Ketika gejala memburuk, pasien dapat mengalami kesulitan berjalan, berbicara, atau menyelesaikan tugas sederhana."),
                    tags$a(href = "https://www.ninds.nih.gov/health-information/disorders/stroke", "Baca lebih lanjut: National Institute of Neurological Disorders and Stroke", target = "_blank"),
                    
                    # ====== TEMUAN KUNCI ======
                    hr(style = "border-top: 1px solid #EE6983;"),
                    h3("🎯 Tiga Temuan Kunci dari Analisis Data"),
                    p("Berdasarkan analisis data dan visualisasi yang disajikan dalam Dashboard ini, kami mengidentifikasi beberapa temuan utama yang signifikan mengenai faktor risiko stroke:"),
                    tags$ol(style = "margin-left: -20px;",
                            tags$li(tags$b("Usia dan Hiperglikemia adalah Prediktor Utama:"), " Usia pasien adalah faktor risiko non-modifikasi yang paling dominan. Secara klinis, pasien dengan rata-rata tingkat glukosa darah yang tinggi (di atas 200 mg/dL) menunjukkan korelasi terkuat dengan kejadian stroke."),
                            tags$li(tags$b("Kombinasi Penyakit Penyerta Meningkatkan Risiko Berlipat Ganda:"), " Risiko stroke meningkat tajam pada individu yang memiliki riwayat Hipertensi dan Penyakit Jantung secara bersamaan, jauh lebih tinggi dibandingkan mereka yang hanya memiliki salah satu penyakit tersebut."),
                            tags$li(tags$b("Pentingnya Manajemen BMI dan Gaya Hidup:"), " Pasien yang mengalami stroke cenderung memiliki distribusi BMI yang lebih tinggi dibandingkan yang tidak, menunjukkan obesitas sebagai faktor risiko penting. Selain itu, faktor gaya hidup seperti status merokok juga memainkan peran yang perlu diperhatikan.")
                    ),
                    # ==========================
                    
                    # ====== DOSEN PENGAMPU (GAMBAR) - BINGKAI KOTAK + UKURAN DIBESARKAN ======
                    hr(style = "border-top: 1px solid #850E35;"),
                    h3("🎓 Dosen Pengampu"),
                    fluidRow(
                      # FOTO DOSEN 1: PAK NOVRI (NS.png)
                      column(width = 4, 
                             tags$div(style = "text-align: center; margin-top: 15px; margin-bottom: 20px;",
                                      tags$img(src = "https://www.its.ac.id/statistika/wp-content/uploads/sites/43/2025/11/NS.png", 
                                               height = "250px", # DIPERBESAR
                                               style = "border: 3px solid #850E35;") 
                             )
                      ),
                      column(width = 4, 
                             tags$div(style = "text-align: center; margin-top: 15px; margin-bottom: 20px;",
                                      tags$img(src = "https://www.its.ac.id/statistika/wp-content/uploads/sites/43/2025/11/RNA.png", 
                                               height = "250px", # DIPERBESAR
                                               style = "border: 3px solid #850E35;") 
                             )
                      ),
                      
                      # FOTO DOSEN 2: PAK AHSAN (AH.png)
                      column(width = 4,
                             tags$div(style = "text-align: center; margin-top: 15px; margin-bottom: 20px;",
                                      tags$img(src = "https://www.its.ac.id/statistika/wp-content/uploads/sites/43/2025/11/AH.png", 
                                               height = "250px", # DIPERBESAR
                                               style = "border: 3px solid #850E35;") 
                             )
                      )
                    )
                    # ============================================
             ),
             
             # SIDEBAR BERITA
             column(width = 4,
                    wellPanel(
                      h3("Berita Tentang Stroke", style = "color: #850E35;"),
                      p("Berikut beberapa berita tentang Stroke. Klik pada link untuk membaca berita lengkap."),
                      hr(),
                      
                      # BERITA 1
                      tags$div(
                        h5(tags$b("Studi Baru Hubungan Gula Darah dan Stroke")),
                        p("Penelitian terbaru menunjukkan bahwa manajemen glukosa darah yang ketat mengurangi risiko stroke hingga 30% pada pasien berusia di atas 65 tahun."),
                        tags$a(href = "https://www.who.int/news-room/fact-sheets/detail/stroke", "Baca Selengkapnya...", target = "_blank")
                      ),
                      hr(),
                      
                      # BERITA 2
                      tags$div(
                        h5(tags$b("Pentingnya Waktu Respons (Golden Hour)")),
                        p("Pencegahan dan pengobatan stroke harus dilakukan dalam 'golden hour' untuk meminimalisir kerusakan otak permanen."),
                        tags$a(href = "https://www.cdc.gov/stroke/treatments.htm", "Lihat Data Klinis...", target = "_blank")
                      ),
                      hr(),
                      
                      # BERITA 3
                      tags$div(
                        h5(tags$b("Tekanan Darah Tinggi: Penyebab Utama Stroke")),
                        p("Hipertensi yang tidak terkontrol secara konsisten menjadi faktor risiko terbesar di seluruh dunia untuk stroke iskemik dan hemoragik."),
                        tags$a(href = "https://www.ahajournals.org/doi/10.1161/STROKEAHA.121.036952", "Lihat Jurnal...", target = "_blank")
                      ),
                      hr(),
                      
                      # BERITA 4
                      tags$div(
                        h5(tags$b("Peran Aktif Fisik dalam Pencegahan Stroke")),
                        p("Gaya hidup kurang gerak (sedentary lifestyle) secara signifikan meningkatkan risiko stroke, sementara olahraga teratur dapat mengurangi risiko hingga 25%."),
                        tags$a(href = "https://www.stroke.org/en/about-stroke/stroke-risk-factors/physical-activity", "Baca Selengkapnya...", target = "_blank")
                      ),
                      hr(),
                      
                      # BERITA 5
                      tags$div(
                        h5(tags$b("Implikasi Merokok pada Usia Muda")),
                        p("Merokok, bahkan pada tingkat sedang, dapat mempercepat penuaan pembuluh darah dan meningkatkan kemungkinan stroke di usia 40-an."),
                        tags$a(href = "https://www.mayoclinic.org/diseases-conditions/stroke/symptoms-causes/syc-20353997", "Baca Selengkapnya...", target = "_blank")
                      ),
                      hr(),
                      
                      # BERITA 6
                      tags$div(
                        h5(tags$b("Tanda Peringatan Stroke (FAST)")),
                        p("Ketahui tanda-tanda FAST (Face drooping, Arm weakness, Speech difficulty, Time to call 911) untuk tindakan cepat yang menyelamatkan nyawa."),
                        tags$a(href = "https://www.strokeassociation.org/", "Pelajari FAST...", target = "_blank")
                      ),
                      hr(),
                      
                      h4(textOutput("total_data"), style = "color: #555555; text-align: center;")
                    )
             )
           )
  ), 
  
  tabPanel("📖 About Dataset",
           fluidPage(
             h2("Deskripsi dan Struktur Dataset Stroke", style = "color: #850E35;"),
             p("Dataset ini bertujuan untuk memprediksi kemungkinan seseorang menderita stroke berdasarkan berbagai fitur input."),
             hr(style = "border-top: 1px solid #EE6983;"),
             
             fluidRow(
               column(width = 6, wellPanel(h4(tags$b("👨‍🦳 Gender & Age (Karakteristik Demografi)"), style = "color: #850E35;"), p("Variabel yang mencakup jenis kelamin dan usia pasien."), p(tags$i("Age adalah numerik, Gender adalah kategorikal.")))),
               column(width = 6, wellPanel(h4(tags$b("❤️ Penyakit Penyerta (Comorbidities)"), style = "color: #850E35;"), p("Meliputi status hipertensi dan riwayat penyakit jantung (1 = Ya, 0 = Tidak)."), p(tags$i("Keduanya adalah variabel biner.")))),
             ),
             fluidRow(
               column(width = 6, wellPanel(h4(tags$b("🧪 Faktor Klinis (Gula Darah & BMI)"), style = "color: #850E35;"), p("Meliputi rata-rata kadar glukosa dalam darah dan Indeks Massa Tubuh (BMI)."), p(tags$i("Kedua variabel ini bersifat numerik.")))),
               column(width = 6, wellPanel(h4(tags$b("🎯 Stroke (Variabel Target)"), style = "color: #850E35;"), p("Variabel biner yang menunjukkan apakah pasien menderita stroke (1) atau tidak (0)."), p(tags$i("Variabel biner.")))),
             ),
             
             hr(style = "border-top: 1px solid #850E35;"),
             
             h3("Cuplikan Data Awal"),
             p("Tabel di bawah menunjukkan 10 baris pertama data yang digunakan untuk analisis:"),
             dataTableOutput("stroke_table")
           )
  ),
  
  navbarMenu("📊 Dataset Analysis",
             
             tabPanel("Summary Statistics",
                      fluidPage(
                        h2("Ringkasan Statistik Data", style = "color: #850E35;"),
                        p("Ringkasan statistik data dibagi berdasarkan tipe variabel (Numerik dan Kategorikal) untuk memudahkan pembacaan."),
                        hr(),
                        
                        fluidRow(
                          column(width = 5, 
                                 h3("Statistik Variabel Numerik", style = "color: #EE6983;"),
                                 dataTableOutput("summary_numeric") 
                          ),
                          column(width = 7,
                                 h3("Statistik Variabel Kategorikal (Frekuensi)", style = "color: #EE6983;"),
                                 dataTableOutput("summary_categorical") 
                          )
                        )
                      )
             ),
             
             tabPanel("Visualization",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Opsi Filter Data"),
                          selectInput(
                            inputId = "gender_filter",
                            label = "Pilih Jenis Kelamin:",
                            choices = unique(stroke_data$gender), 
                            selected = "Female"
                          ),
                          hr(), 
                          p("Semua visualisasi di halaman ini akan diperbarui berdasarkan filter jenis kelamin di atas.")
                        ),
                        mainPanel(
                          h2("Visualisasi Interaktif Faktor Risiko Stroke"),
                          plotOutput("visual_plot_glucose", height = "500px"),
                          br(),
                          plotOutput("visual_plot_bmi", height = "500px"),
                          br(),
                          plotOutput("visual_plot_risk", height = "500px")
                        )
                      )
             )
  ),
  
  tabPanel("📧 Contact",
           fluidPage(
             
             wellPanel(
               h2("Dashboard Goals", style = "color: #850E35;"),
               p("This dashboard was developed as a requirement for the Final Project for the Data Mining dan Visualisasi course in the Statistics Undergraduate Study Program, Department of Statistics, Faculty of Science and Data Analytics, Sepuluh Nopember Institute of Technology, Surabaya."),
               tags$a(href = "https://www.its.ac.id/", "Kunjungi Website Resmi ITS", target = "_blank"),
             ),
             
             hr(style = "border-top: 1px solid #EE6983;"),
             
             h2("Our Team", style = "color: #850E35; margin-top: 20px;"),
             fluidRow(
               column(width = 4,
                      wellPanel(
                        h3("Veronica Febriani Putri", style = "color: #EE6983;"),
                        tags$p(tags$b("NRP:"), " 5003231121"),
                        tags$p(tags$b("Kelas:"), " K")
                      )
               ),
               column(width = 4,
                      wellPanel(
                        h3("Nadiarista Yustiaputri", style = "color: #EE6983;"),
                        tags$p(tags$b("NRP:"), " 5003231208"),
                        tags$p(tags$b("Kelas:"), " K")
                      )
               )
               ,
               column(width = 4,
                      wellPanel(
                        h3("Dhea Ary Shofyan", style = "color: #EE6983;"),
                        tags$p(tags$b("NRP:"), " 5003231210"),
                        tags$p(tags$b("Kelas:"), " K")
                      )
               )
             )
           )
  )
)

# ==================================
# 4. MENJALANKAN APLIKASI
# ==================================

shinyApp(ui = ui, server = server)