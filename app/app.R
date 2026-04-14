library(shiny)

ARMS <- c("Control", "Treatment")

AGE_GROUPS <- c("21-34", "35-54", "55+")
SEX_LEVELS <- c("Male", "Female")
DRINK_LEVELS <- c("Moderate", "Heavy")

# -------------------------------
# Urn randomization function
# -------------------------------
urn_randomize <- function(previous_assignments,
                          arms = ARMS,
                          bias_strength = 0.75) {
    if (length(previous_assignments) == 0) {
        return(sample(arms, 1))
    }
    
    tab <- table(factor(previous_assignments, levels = arms))
    
    if (tab[1] == tab[2]) {
        probs <- rep(1 / length(arms), length(arms))
    } else {
        underrep <- which.min(tab)
        probs <- rep((1 - bias_strength), length(arms))
        probs[underrep] <- bias_strength
        probs <- probs / sum(probs)
    }
    
    sample(arms, 1, prob = probs)
}

# -------------------------------
# Helper: derive age group
# -------------------------------
age_to_group <- function(age) {
    if (age >= 21 && age <= 34) return("21-34")
    if (age >= 35 && age <= 54) return("35-54")
    if (age >= 55) return("55+")
    NA_character_
}

# -------------------------------
# UI
# -------------------------------
ui <- fluidPage(
    titlePanel("Stratified Urn Randomization"),
    
    tags$div(
        style = "color: #b30000; font-weight: bold;",
        "WARNING: This app relies on a user-uploaded CSV as the single source of truth.
     Only one person should control enrollment."
    ),
    
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "csv",
                "Upload previous enrollment CSV (optional)",
                accept = ".csv"
            ),
            downloadButton("download", "Download current CSV"),
            hr(),
            numericInput("age", "Age:", min = 21, max = 120, value = 30),
            selectInput("sex", "Sex:", choices = SEX_LEVELS),
            selectInput("drink", "Baseline drinking level:",
                        choices = DRINK_LEVELS),
            actionButton("rand", "Randomize", class = "btn-primary"),
            hr(),
            textOutput("assignment")
        ),
        
        mainPanel(
            h4("Enrollment Table"),
            tableOutput("enroll_table"),
            hr(),
            h4("Balance by Stratum"),
            tableOutput("balance_table")
        )
    )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
    
    empty_df <- data.frame(
        id = integer(),
        age = integer(),
        age_group = character(),
        sex = character(),
        drinking_level = character(),
        treatment = character(),
        stringsAsFactors = FALSE
    )
    
    enrollments <- reactiveVal(empty_df)
    
    # -------------------------------
    # CSV upload + validation
    # -------------------------------
    observeEvent(input$csv, {
        req(input$csv)
        
        df <- read.csv(input$csv$datapath, stringsAsFactors = FALSE)
        
        required <- c("age", "age_group", "sex", "drinking_level", "treatment")
        
        validate(
            need(all(required %in% names(df)),
                 paste("CSV must contain:", paste(required, collapse = ", "))),
            need(all(df$age_group %in% AGE_GROUPS),
                 "Invalid age_group values"),
            need(all(df$sex %in% SEX_LEVELS),
                 "Invalid sex values"),
            need(all(df$drinking_level %in% DRINK_LEVELS),
                 "Invalid drinking_level values"),
            need(all(df$treatment %in% ARMS),
                 "Invalid treatment values")
        )
        
        if (!"id" %in% names(df)) {
            df$id <- seq_len(nrow(df))
        }
        
        df <- df[, c("id", required)]
        enrollments(df)
    })
    
    # -------------------------------
    # Randomization
    # -------------------------------
    observeEvent(input$rand, {
        dat <- enrollments()
        
        age_group <- age_to_group(input$age)
        
        validate(
            need(!is.na(age_group),
                 "Age must be ≥21")
        )
        
        stratum_idx <- which(
            dat$age_group == age_group &
                dat$sex == input$sex &
                dat$drinking_level == input$drink
        )
        
        prev_assignments <- dat$treatment[stratum_idx]
        
        trt <- urn_randomize(prev_assignments)
        
        new_row <- data.frame(
            id = nrow(dat) + 1,
            age = input$age,
            age_group = age_group,
            sex = input$sex,
            drinking_level = input$drink,
            treatment = trt,
            stringsAsFactors = FALSE
        )
        
        enrollments(rbind(dat, new_row))
    })
    
    # -------------------------------
    # Outputs
    # -------------------------------
    output$assignment <- renderText({
        dat <- enrollments()
        if (nrow(dat) == 0) return("")
        paste("Assigned to:", dat$treatment[nrow(dat)])
    })
    
    output$enroll_table <- renderTable({
        enrollments()
    }, rownames = FALSE)
    
    output$balance_table <- renderTable({
        dat <- enrollments()
        if (nrow(dat) == 0) return(NULL)
        
        as.data.frame(
            with(dat, table(age_group, sex, drinking_level, treatment))
        )
    })
    
    # -------------------------------
    # CSV download
    # -------------------------------
    output$download <- downloadHandler(
        filename = function() {
            paste0("urn_randomization_state_", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(enrollments(), file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)