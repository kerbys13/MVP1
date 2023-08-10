# Establecer la información de la cuenta
rsconnect::setAccountInfo(name='kerbysjguerrero13', token='169BC4F01280B8AFA362C9FE9E415171', secret='mXAblp+uCs28Ci79QqPsLbdz6ckj0yU5SBSdJ394')

# Desplegar la aplicación
rsconnect::deployApp(appDir = getwd())

# Load packages 
library(shiny)
library(conflicted)
library(rsconnect)
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(openxlsx)
library(shinyWidgets)
library(ggsci)
library(DT)

data0 <- read_csv("FLR-2018-2022 (Contado RF, Estructurada 12 3 4, Venta FWD RF).csv")
data1 <- read_excel("Liq. MS RF 1er Sem. 23 (Excluye Préstamos y Oper. Estr. 2RF).xlsx")

data0 <- data0[,c(3:8,10:30)]
data0 <- data0[,c(1:4,27,5:26)]

# Format Dates
data0$DATE_LIQUIDACION_REAL1 <- dmy(data0$DATE_LIQUIDACION_REAL) # Output es ymd
data1$DATE_LIQUIDACION_REAL1 <- ymd(data1$DATE_LIQUIDACION_REAL)
data0$VENCIMIENTO1 <- dmy(data0$VENCIMIENTO)
data1$VENCIMIENTO1 <- dmy(data1$VENCIMIENTO)

data <- rbind(data0, data1)
print(data)

data$VENCIMIENTO2 <- format(data$VENCIMIENTO1,format="%d-%b-%Y")
data$EMISOR1 <- ifelse(data$EMISOR == "BANCO CENTRAL DE LA REPUBLICA DOMINICANA", "BANCO CENTRAL RD", data$EMISOR)
data$EMISOR1 <- ifelse(data$EMISOR1 == "MINISTERIO DE HACIENDA", "MINISTERIO HACIENDA", data$EMISOR1)

# ---- Summary ---- 
isines <- unique(data$ISIN)
resume_isines <- count(data, ISIN)
resume_emisores <- count(data, EMISOR1)

#s <- filter(data, ISIN == "DO1005241125")

#f <- count(s, vendedor)


data$yrs_vencimiento <- round(time_length(difftime(data$VENCIMIENTO1,Sys.Date()), "years"),1)

data$vigente <- ifelse(difftime(data$VENCIMIENTO1,Sys.Date()) > 0, "VIGENTE", "VENCIDO")

data_vigentes <- subset(data, data$vigente == "VIGENTE")

isines_vigentes <- unique(data_vigentes$ISIN)

#write_xlsx(data_vigentes, "data_vigentes.xlsx")

# ---- MERGE CON DATA BVRD ---- 

maestro1 <- read_excel("MAESTRO-TITULOS.xlsx")
maestro2 <- read_excel("MAESTRO-FINAL-2.xlsx")

names(maestro2)[4] <- "ISIN"
names(maestro1)[6] <- "EMISOR"

maestro <- rbind(maestro1, maestro2)

maestro$duplicates <- duplicated(maestro$ISIN)

# DROP los TRUE para quedarte con una observación por ISIN
maestro0 <- subset(maestro, maestro$duplicates == "FALSE")

data <- left_join(data, maestro0, by = "ISIN")


# ----- tipo de instrumentos ----
data$NOMBRE_INSTRUMENTO2 <- data$NOMBRE_INSTRUMENTO
instrumentos2 <- count(data, NOMBRE_INSTRUMENTO2)

data$NOMBRE_INSTRUMENTO1 <- is.na(data$NOMBRE_INSTRUMENTO)

na_instrumentos <- subset(data, is.na(data$NOMBRE_INSTRUMENTO))

unique(na_instrumentos$ISIN)

# ----- rename ----
data$NOMBRE_INSTRUMENTO2 <- gsub("Bonos", "Bono", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
data$NOMBRE_INSTRUMENTO2 <- gsub("Corporativos", "Corporativo", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
data$NOMBRE_INSTRUMENTO2 <- gsub("Subordinados", "Subordinado", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
data$NOMBRE_INSTRUMENTO2 <- gsub("Valores", "Valor", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
data$NOMBRE_INSTRUMENTO2 <- gsub("Papeles Comerciales", "Papel Comercial", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
data$NOMBRE_INSTRUMENTO2 <- gsub("Bono Corporativo USD", "Bono Corporativo", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
data$NOMBRE_INSTRUMENTO2 <- gsub("Certificados Inversion Especial BC", "Certificado de Inversión Especial", data$NOMBRE_INSTRUMENTO2, ignore.case = TRUE)
# ----- PERIODICIDAD CUPON ----

periodicidad <- count(data, NOMBRE_PERIODO)
data$NOMBRE_PERIODO <- gsub("Semestral", "semestrales", data$NOMBRE_PERIODO)
data$NOMBRE_PERIODO <- gsub("Anual", "anuales", data$NOMBRE_PERIODO)
data$NOMBRE_PERIODO <- gsub("Mensual", "mensuales", data$NOMBRE_PERIODO)
data$NOMBRE_PERIODO <- gsub("Trimestral", "trimestrales", data$NOMBRE_PERIODO)

# ----- EMISORES  ----

data$EMISOR1 <- gsub("FIDEICOMISO PARA LA OPERACION MANTENIMIENTO Y EXPANSION DE LA RED VIAL PRINCIPAL DE LA REPUBLICA DOMINICANA", "FIDEICOMISO RDVIAL", data$EMISOR1)
data$EMISOR1 <- gsub("FIDEICOMISO DE OFERTA PUBLICA DE VALORES LARIMAR I NO 04 F P", "FIDEICOMISO LARIMAR I NO 04 F P", data$EMISOR1)
data$EMISOR1 <- gsub("FIDEICOMISO DE OFERTA PUBLICA DE VALORES INMOBILIARIOS BONA CAPIT", "FIDEICOMISO DE VALORES INMOBILIARIOS BONA CAPITAL", data$EMISOR1)
data$EMISOR1 <- gsub("BANCO MULTIPLE PROMERICA DE LA REPUBLICA DOMINICANA", "BANCO MULTIPLE PROMERICA", data$EMISOR1)
data$EMISOR1 <- gsub("ALPHA SOCIEDAD DE VALORES, S. A. PUESTO DE BOLSA", "ALPHA - PUESTO DE BOLSA", data$EMISOR1)

data_vigentes <- subset(data, data$vigente == "VIGENTE")

emisores <- unique(data_vigentes$EMISOR1)

# ----- TASAS VARIABLES... NECESITAS UN THIRDLINE DIFERENTE
# ---- CUANTOS ISINES ESTAN ASI --- 
tasas <- count(data,TIPO_TASA)


# --------------------------------------------------------
# ------------------ OPTIONS PARA PUESTOS DE BOLSA ----------
# --------------------------------------------------------
ven <- count(data, DEP_VENDEDOR) %>%
  arrange(desc(n))

puestos <- c("PARVA", "ALPHA",  "TIVALSA","UNICA",  "INRES", 
             "CCI", "JMMB",  "IPSA", "BHDVAL",  "PRIMMA",  
             "EXCEL","VERTEX",  "ISANTACR",  "MPBMULTI", "CITIV")

data$vendedor <- ifelse(data$DEP_VENDEDOR %in% puestos, data$DEP_VENDEDOR, "Otras EIF")

vendedor <- unique(data$vendedor) # Choices que quiero mostrar

vendedor_fct <- c("PARVA", "ALPHA",  "TIVALSA","UNICA",  "INRES", 
                  "CCI", "JMMB",  "IPSA", "BHDVAL",  "PRIMMA",  
                  "EXCEL","VERTEX",  "ISANTACR",  "MPBMULTI", "CITIV",
                  "Otras EIF")

vendedor <- vendedor[match(vendedor_fct, vendedor)] # Choices reordered


# --------------------------------------------------------
# ------------------ OPTIONS PARA tipo de operacion ----------
# --------------------------------------------------------

data$SUB_OPERACION1 <- ifelse(grepl("OPERACION ESTRUCTURADA", data$SUB_OPERACION), "OP. ESTRUCTURADAS", data$SUB_OPERACION)
data$SUB_OPERACION1 <- gsub("REPORTO A PLAZO BCRD RF", "REPO A PLAZO BCRD", data$SUB_OPERACION1)

suboperaciones <- unique(data$SUB_OPERACION1)

#sub <- count(data, SUB_OPERACION)
#sub2 <- count(data, SUB_OPERACION1)

#data$DATE_LIQUIDACION_REAL1  <- as.Date(data$DATE_LIQUIDACION_REAL1)

#write.xlsx(data, "data.xlsx")

#  titlePanel("Consulta el Precio Limpio de los Valores que te Interesan"),
#  fluidRow(
#style = "padding: 20px; margin-bottom: 20px;",
#h1("Consulta el Precio Limpio de los Valores que te Interesan")
#),

# --------------------------------------------------------
# --------- Textbox para buscar por ISIN ----------
# --------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(
      "label { font-size:140%; }"
    ))
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("locInput",
                  "Filtra por emisor", 
                  choices = c("TODOS", unique(data_vigentes$EMISOR1)), 
                  options = FALSE,
                  multiple = FALSE,
                  selected = "TODOS"),  
      
      selectInput(inputId = "ISIN_select",
                  label = "Selecciona un ISIN",
                  choices = unique(data_vigentes$ISIN),
                  selected = "DO1005241125"),
      
      textInput(inputId = "ISIN_text",
                label = "¿Buscas un ISIN específico?", 
                placeholder = "Ingrésalo aquí"),
      
      checkboxGroupInput("tipo", 
                         h3("Tipo de Operación"), 
                         choices = unique(data$SUB_OPERACION1),
                         selected = unique(data$SUB_OPERACION1)),
      
      checkboxGroupInput("checkGroup", 
                         h3("Puesto de Bolsa"), 
                         choices = vendedor[match(vendedor_fct, vendedor)],
                         selected = vendedor[match(vendedor_fct, vendedor)]),
      
      width = 3  # Width of the sidebar to 3 (default is 4)
    ),
    mainPanel(
      plotOutput(outputId = "scatterplot", height = "710px"),  # Height of plot - Default is 400px
      dataTableOutput("summary_table"),  # Add tableOutput to display the summary table
      width = 9  # Width of the main panel to 9 (default is 8)
    )
  )
)




server <- function(input, output, session) {
  # Reactive expression to filter isines based on the selected emisor
  filtered_isines <- reactive({
    selected_emisor <- input$locInput
    if (identical(selected_emisor, "TODOS")) {
      return(list(All = unique(data_vigentes$ISIN)))
    } else {
      return(unique(data_vigentes[data_vigentes$EMISOR1 %in% selected_emisor, "ISIN"]))
    }
  })
  
  # Observe the changes in the pickerInput and update the selectInput choices accordingly
  observeEvent(input$locInput, {
    updateSelectInput(session, "ISIN_select", choices = filtered_isines())
    
  })
  
  # ----- Ajustes para que leyenda y colores funcionen bien -----
  selected_categories <- reactive({
    # If no categories are selected, return an empty vector with correct levels
    if (is.null(input$tipo)) {
      return(factor(character(), levels = c("CONTADO RF", "VENTA FOWARD RF", "OP. ESTRUCTURADAS", "REPO A PLAZO BCRD", "REPORTO RF")))
    }
    # If categories are selected, return a factor vector with correct levels
    return(factor(input$tipo, levels = c("CONTADO RF", "VENTA FOWARD RF", "OP. ESTRUCTURADAS", "REPO A PLAZO BCRD", "REPORTO RF")))
  })
  
  colors_named <- c("CONTADO RF" = "#3C5488FF", 
                    "VENTA FOWARD RF" = "#F39B7FFF", 
                    "OP. ESTRUCTURADAS" = "#00a087ff", 
                    "REPO A PLAZO BCRD" = "#DC0000FF", 
                    "REPORTO RF" = "#B09C85FF")
  
  output$scatterplot <- renderPlot({
    # If the text input for ISIN is not empty, use it; otherwise, use the selectInput value
    chosen_ISIN <- ifelse(input$ISIN_text != "", input$ISIN_text, input$ISIN_select)
    
    filtered <- data[data$ISIN == chosen_ISIN & data$vendedor %in% input$checkGroup & data$SUB_OPERACION1 %in% input$tipo,]
    
    # Datos para gráfico
    isines_vigentes <- unique(data_vigentes$ISIN)
    emisores <- unique(data_vigentes$EMISOR1)
    selected_emisor <- unique(filtered$EMISOR1)
    vencimiento_isin <- unique(filtered$VENCIMIENTO2)
    years_vencimiento <- unique(filtered$yrs_vencimiento)
    cupon_isin <- unique(filtered$TASA_INT_EMISION)
    moneda_isin <- unique(filtered$MONEDA.x)
    tipo_valor <- unique(filtered$NOMBRE_INSTRUMENTO2)
    tipo_tasa <- unique(filtered$TIPO_TASA)
    periodicidad <- unique(filtered$NOMBRE_PERIODO)
    
    filtered$SUB_OPERACION1 <- factor(filtered$SUB_OPERACION1, levels = c("CONTADO RF", "VENTA FOWARD RF", "OP. ESTRUCTURADAS", "REPO A PLAZO BCRD", "REPORTO RF"))
    
    subtitle_third_line <- if_else(
      unique(filtered$TIPO_TASA) == "Tasa Cero Cupon",
      "Este instrumento no paga intereses, sólo su valor facial en la fecha de vencimiento.",
      if (unique(filtered$TIPO_TASA) == "Tasa Variable") {
        "Este instrumento tiene un cupón Tasa Variable. Consulta con el emisor cuál es el cupón actual."
      } else {
        paste0("Tiene un cupón ", tipo_tasa, " de ", cupon_isin, "% anual, con pagos ", periodicidad, ".")
      }
    )
    ggplot(filtered, aes(DATE_LIQUIDACION_REAL1, PRECIO_LIMPIO)) + 
      geom_point(aes(color = SUB_OPERACION1), size = 5, alpha = 0.7) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
      geom_text(aes(label = paste("Liquidaciones:", format(nrow(filtered), big.mark = ","))),
                x = Inf, y = Inf, hjust = 1, vjust = 1, nudge_x = -0.1, nudge_y = -0.1,
                size = 7.5, color = "#44546A") +
      scale_color_manual(
        values = colors_named,
        breaks = selected_categories(),
        labels = selected_categories()
      ) + 
      labs(title = paste("Precios limpios de", chosen_ISIN, "-", selected_emisor),
           subtitle = paste0(
             tipo_valor, ", denominado en ", moneda_isin, ".\n",
             "Faltan ", years_vencimiento, " años para su vencimiento, en ", vencimiento_isin,".\n",
             subtitle_third_line),
           x = "", y = "") +  
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0, size = 23, face = "bold", color = "#44546A", margin = margin(9,0,6,0)),
            legend.title = element_blank(),
            plot.subtitle = element_text(hjust = 0, size = 20, margin = margin(3,0,20,0)),
            legend.position = "top",
            legend.text = element_text(size = 17, face = "bold"),
            legend.box.spacing = margin(0,0,10,0),
            axis.text = element_text(size = 20),
            plot.margin = margin(12, 5, 5, -12))  # Set the plot margins (top, right, bottom, left) 
    
  })
  
  output$summary_table <- renderDataTable({
    chosen_ISIN <- ifelse(input$ISIN_text != "", input$ISIN_text, input$ISIN_select)
    
    filtered_data <- data[data$ISIN == chosen_ISIN & data$vendedor %in% input$checkGroup & data$SUB_OPERACION1 %in% input$tipo,]
    
    # Calculate the count of observations for each combination of "vendedor" and "suboperaciones"
    vendor_subop_count <- aggregate(filtered_data$ISIN, by = list(filtered_data$vendedor, filtered_data$SUB_OPERACION1), FUN = length)
    colnames(vendor_subop_count) <- c("Vendedor", "Suboperaciones", "Count")
    
    # Calculate the summary table for "vendedor" counts
    vendor_count_summary <- aggregate(filtered_data$ISIN, by = list(filtered_data$vendedor), FUN = length)
    colnames(vendor_count_summary) <- c("Vendedor", "Liquidaciones")
    
    # Merge both summary tables 
    comprehensive_table <- left_join(vendor_count_summary, vendor_subop_count, by = "Vendedor")
    
    # Pivot the table to have a column for each "suboperaciones"
    comprehensive_table <- pivot_wider(comprehensive_table, names_from = "Suboperaciones", values_from = "Count", values_fill = 0)
    
    comprehensive_table <- arrange(comprehensive_table, desc(`Liquidaciones`))  # Sort the data frame in descending order based on Total Count
    
    # Format the numbers with commas
    comprehensive_table[, -1] <- lapply(comprehensive_table[, -1], function(x) format(x, big.mark = ","))
    
    # Return the comprehensive_table as a DataTable
    datatable(comprehensive_table, 
              options = list(
                pageLength = 10, 
                dom = 't', 
                scrollX = TRUE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'font-size': '130%'});",
                  "}")), 
              class = 'hover', 
              rownames = FALSE) %>%
      formatStyle(
        names(comprehensive_table),    # Apply styles to all columns
        fontSize = '20px'                  # Set the font size to 18 pixels
      )
  })
}



shinyApp(ui, server)


# --------------------------------------------------------
# --------- weighted average line... it should exclude JMMB ----------
# --------------------------------------------------------
