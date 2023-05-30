# load packages

library(shiny)
library(tidyverse)
library(tools)
library(shinythemes)


### bia_table function (could write a package for this)

p_case_detection <- c(0.05,0.10,0.15,0.20,0.25)

bia_table <- function(results,base_scenario,alt_scenario){
  
  base_table <- results %>%
    filter(scenario==base_scenario) %>%
    select(year,scenario,criteria,CD_method,cost_total:cost_maint) %>% 
    mutate(cost_other=cost_total-cost_case_detection-cost_hosp-cost_treat)
  
  alt_table <- results %>%
    filter(scenario==alt_scenario) %>%
    select(year,scenario,criteria,CD_method,cost_total:cost_maint) %>% 
    mutate(cost_other=cost_total-cost_case_detection-cost_hosp-cost_treat)
  
  bia_table <- data.frame(year=0:length(p_case_detection),
                          scenario=alt_scenario,
                          criteria=alt_table$criteria[1],
                          CD_method=alt_table$CD_method[1],
                          bia_total=base_table$cost_total-alt_table$cost_total,
                          bia_case_detection=base_table$cost_case_detection-alt_table$cost_case_detection,
                          bia_treat=base_table$cost_treat-alt_table$cost_treat,
                          bia_hosp=base_table$cost_hosp-alt_table$cost_hosp,
                          bia_maint=base_table$cost_maint-alt_table$cost_maint,
                          bia_other=base_table$cost_other-alt_table$cost_other)
  
  table <- bind_rows(
    base_table %>%
      pivot_longer(cols=cost_total:cost_other,names_to="cost_group",values_to="CAD") %>%
      pivot_wider(names_from=year,names_prefix="year_",values_from=CAD)
    ,
    alt_table %>%
      pivot_longer(cols=cost_total:cost_other,names_to="cost_group",values_to="CAD") %>%
      pivot_wider(names_from=year,names_prefix="year_",values_from=CAD)
    ,
    bia_table %>%
      pivot_longer(cols=bia_total:bia_other,names_to="cost_group",values_to="CAD") %>%
      pivot_wider(names_from=year,names_prefix="year_",values_from=CAD)
  )
  
  return(table)
  
}

# load and wrangle data ### edit this so that I only have to load in 3 dfs to the app. No point having all of the filtering here. 
# 0.05 to 0.13 
all_overall_10000_005_013 <- readRDS("data_10000/all_overall_10000_005_013.RDa")
all_results_10000_005_013 <- readRDS("data_10000/all_results_10000_005_013.RDa")
bia_all_long_10000_005_013 <- readRDS("data_10000/bia_all_long_10000_005_013.RDa")

# 0.05 to 0.25
all_overall_10000_005_025 <- readRDS("data_10000/all_overall_10000_005_025.RDa")
all_results_10000_005_025 <- readRDS("data_10000/all_results_10000_005_025.RDa")
bia_all_long_10000_005_025 <- readRDS("data_10000/bia_all_long_10000_005_025.RDa")

# rbind dfs 
all_overall_rbind <- rbind(all_overall_10000_005_013, all_overall_10000_005_025)
all_results_rbind <- rbind(all_results_10000_005_013, all_results_10000_005_025)
all_results_rbind_noedit <- all_results_rbind
bia_all_long_rbind <- rbind(bia_all_long_10000_005_013, bia_all_long_10000_005_025)

# figure caption
fig_cap <- "Figure 2: Annual total (top), case detection (middle left), treatment (middle right), hospitalisation (bottom left), 
           and outpatient care (bottom right) additional costs (million $) compared to no case detection baseline scenario. 
           Negative additional costs indicate cost savings. 
           S1a CDQ ≥ 17 points for all patients; S1b flow meter (with bronchodilator) all patients; S1c CDQ ≥ 17 points + flow meter (with bronchodilator) all patients; 
           S2a flow meter (without bronchodilator) among symptomatic patients; S3a CDQ ≥ 19.5 points among patients aged ≥50 years with a smoking history; 
           S3b CDQ ≥ 16.5 points among patients aged ≥50 years with a smoking history; S3c flow meter (without bronchodilator) among patients aged ≥50 years with a smoking history, 
           S3d CDQ ≥ 17 points + flow meter (with bronchodilator) among patients aged ≥50 years with a smoking history. Corresponding results tables can be found in Appendix 2."

# Define UI 
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("BIA EpicR"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "uptake_step", 
        label = "Yearly increase in uptake (initial value is 0.05, or 5%)", 
        choices = c(unique(bia_all_long_rbind$uptake_step)), 
        selected = bia_all_long_rbind$uptake_step[1]
      ),
      # unit costs 
      numericInput(
        inputId = "UC_cd", 
        label = "Unit cost of case detection", 
        value = all_results_rbind_noedit$UC_case_detection[1]
      ),
      numericInput(
        inputId = "UC_OPd",
        label = "Unit cost of OP diagnosis", 
        value = all_results_rbind_noedit$UC_OP_diagnosis[1]
      ),
      numericInput(
        inputId = "saba_UC",
        label = "Unit cost of SABA", 
        value = all_results_rbind_noedit$UC_SABA[1]
      ),
      numericInput(
        inputId = "lama_UC",
        label = "Unit cost of LAMA", 
        value = all_results_rbind_noedit$UC_LAMA[1]
      ),
      numericInput(
        inputId = "lama_laba_UC",
        label = "Unit cost of LAMA+LABA", 
        value = all_results_rbind_noedit$UC_LAMA_LABA[1]
      ),
      numericInput(
        inputId = "ics_lama_laba_UC",
        label = "Unit cost of ICS+LAMA+LABA", 
        value = all_results_rbind_noedit$UC_ICS_LAMA_LABA[1]
      ),
      numericInput(
        inputId = "nrt_UC",
        label = "Unit cost of NRT", 
        value = all_results_rbind_noedit$UC_NRT[1]
      ),
      numericInput(
        inputId = "exac_sev_UC",
        label = "Unit cost of severe exacerbation", 
        value = all_results_rbind_noedit$UC_exac_sev[1]
      ),
      numericInput(
        inputId = "exac_vsev_UC",
        label = "Unit cost of very-severe exacerbation", 
        value = all_results_rbind_noedit$UC_exac_vsev[1]
      ),
      numericInput(
        inputId = "gold1_UC",
        label = "Unit cost of GOLD1", 
        value = all_results_rbind_noedit$UC_GOLD1[1]
      ),
      numericInput(
        inputId = "gold2_UC",
        label = "Unit cost of GOLD2", 
        value = all_results_rbind_noedit$UC_GOLD2[1]
      ),
      numericInput(
        inputId = "gold3_UC",
        label = "Unit cost of GOLD3", 
        value = all_results_rbind_noedit$UC_GOLD3[1]
      ),
      numericInput(
        inputId = "gold4_UC",
        label = "Unit cost of GOLD4", 
        value = all_results_rbind_noedit$UC_GOLD4[1]
      ),
      actionButton(
        inputId = "update_ucs",
        label = "Update unit costs"
      )
    ),
    
    ### must update the first line of each legend, create a single str object 
    mainPanel(
      tabsetPanel(
        tabPanel("Total costs",
           plotOutput("p1"),
           p(em(fig_cap))
        ),
        tabPanel("Case detection costs",
           plotOutput("p2"),
           p(em(fig_cap))
        ),
        tabPanel("Treatment costs",
           plotOutput("p3"),
           p(em(fig_cap))
        ),
        tabPanel("Hospitalization costs",
           plotOutput("p4"),
           p(em(fig_cap))
        ),
        tabPanel("Outpatient care costs",
           plotOutput("p5"),
           p(em(fig_cap))
        ),
        tabPanel("Total costs (table)",
          dataTableOutput("bia_total"),
          dataTableOutput("dataTable"))
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  reactiveData <- eventReactive(input$update_ucs, {
    
    #update all_results_rbind with new ucs
    all_results_rbind <- all_results_rbind %>%
      filter(round(uptake_step, 2)==input$uptake_step) #filter all_results_rbind by uptake parameter
    
    all_results_rbind$UC_case_detection <- input$UC_cd
    all_results_rbind$UC_OP_diagnosis <- input$UC_OPd
    all_results_rbind$UC_SABA <- input$saba_UC
    all_results_rbind$UC_LAMA <- input$lama_UC
    all_results_rbind$UC_LAMA_LABA <- input$lama_laba_UC
    all_results_rbind$UC_ICS_LAMA_LABA <- input$ics_lama_laba_UC
    all_results_rbind$UC_NRT <- input$nrt_UC
    all_results_rbind$UC_exac_sev <- input$exac_sev_UC
    all_results_rbind$UC_exac_vsev <- input$exac_vsev_UC
    all_results_rbind$UC_GOLD1 <- input$gold1_UC
    all_results_rbind$UC_GOLD2 <- input$gold2_UC
    all_results_rbind$UC_GOLD3 <- input$gold3_UC
    all_results_rbind$UC_GOLD4 <- input$gold4_UC
    
    # recalculate yearly costs based on new uc 
    # cd
    CD <- cbind(all_results_rbind$case_detection, all_results_rbind$CD_true_pos+all_results_rbind$CD_false_pos) #case detections
    CD_uc <- c(all_results_rbind$UC_case_detection[1], all_results_rbind$UC_OP_diagnosis[1]) 
    
    # tx
    treat <- cbind(all_results_rbind$SABA, all_results_rbind$LAMA, all_results_rbind$LAMA_LABA, all_results_rbind$ICS_LAMA_LABA, all_results_rbind$NRT)
    treat_uc <- c(all_results_rbind$UC_SABA[1], all_results_rbind$UC_LAMA[1], all_results_rbind$UC_LAMA_LABA[1], all_results_rbind$UC_ICS_LAMA_LABA[1], all_results_rbind$UC_NRT[1])
    
    # hosps
    hosps <- cbind(all_results_rbind$exacs_sev, all_results_rbind$exacs_vsev)
    hosps_uc <- c(all_results_rbind$UC_exac_sev[1], all_results_rbind$UC_exac_vsev[1])
    
    # background
    copd <- cbind(all_results_rbind$copd_GOLD1, all_results_rbind$copd_GOLD2, all_results_rbind$copd_GOLD3, all_results_rbind$copd_GOLD4)
    bg_uc <- c(all_results_rbind$UC_GOLD1[1], all_results_rbind$UC_GOLD2[1], all_results_rbind$UC_GOLD3[1], all_results_rbind$UC_GOLD4[1])
    
    # recalculate
    all_results_rbind <- all_results_rbind %>% 
      mutate(cost_case_detection = rowSums(t(c(CD_uc)*t(CD)))) %>% 
      mutate(cost_treat = rowSums(t(c(treat_uc)*t(treat)))) %>% 
      mutate(cost_hosp = rowSums(t(c(hosps_uc)*t(hosps)))) %>%
      mutate(cost_maint = rowSums(t(c(bg_uc)*t(copd))))
    
    # rerun bia_table
    bia_s10_s1a <- bia_table(all_results_rbind,"s10","s1a")
    bia_s10_s1b <- bia_table(all_results_rbind,"s10","s1b")
    bia_s10_s1c <- bia_table(all_results_rbind,"s10","s1c")
    bia_s10_s2a <- bia_table(all_results_rbind,"s10","s2a")
    bia_s10_s3a <- bia_table(all_results_rbind,"s10","s3a")
    bia_s10_s3b <- bia_table(all_results_rbind,"s10","s3b")
    bia_s10_s3c <- bia_table(all_results_rbind,"s10","s3c")
    bia_s10_s3d <- bia_table(all_results_rbind,"s10","s3d")
    
    bia_all_long_rbind <- bind_rows(bia_s10_s1a,
                              bia_s10_s1b,
                              bia_s10_s1c,
                              bia_s10_s2a,
                              bia_s10_s3a,
                              bia_s10_s3b,
                              bia_s10_s3c,
                              bia_s10_s3d) %>% 
      distinct() %>% 
      pivot_longer(cols=year_0:year_5,names_to="year",values_to="CAD") %>% 
      mutate(year=fct_recode(year,"2021"="year_0","2022"="year_1","2023"="year_2","2024"="year_3","2025"="year_4","2026"="year_5")) %>% 
      mutate(scenario=fct_recode(scenario,"S10"="s10","S1a"="s1a","S1b"="s1b","S1c"="s1c","S2a"="s2a","S3a"="s3a","S3b"="s3b","S3c"="s3c","S3d"="s3d")) %>% 
      mutate(year=as.numeric(year)+2020) %>%
      mutate(uptake_step = input$uptake_step)
    return(bia_all_long_rbind)
  })
  
  output$bia_total <- renderDataTable({
    bia_all_long_rbind <- reactiveData() # access output of eventReactive
    bia_all_long_rbind %>%
      filter(year!="2021") %>%
      group_by(scenario,cost_group) %>%
      summarize(total_CAD=sum(CAD)) %>%
      pivot_wider(names_from=scenario,values_from=total_CAD)
  })
  
  output$dataTable <- renderDataTable({
    value <- reactiveData()
    value
  })
  
  output$p1 <- renderPlot({
    bia_all_long_rbind <- reactiveData()
    ggplot(bia_all_long_rbind %>% filter(cost_group=="bia_total" & scenario!="s10"), 
           #had to round, but could create a round values column in original .RDa file
           aes(x=year,y=CAD*(-1)/1000000,group=scenario,col=scenario)) +
      scale_color_manual(values=c("#F5594E", "#E7861B", "#95A900", "#00B81F", "#00C0B8", "#00A5FF", "#BF80FF", "#FF61C9"), name="Scenario") +
      scale_x_continuous(expand=c(0.01,0.01)) +
      scale_y_continuous(breaks=seq(0,140,25),expand=c(0.02,0.02),limits=c(0,131)) +
      ylab("Additional total costs (million $)") + xlab("") +
      geom_point() +
      geom_line() +
      theme_bw() +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14),
            legend.text=element_text(size=12),legend.title=element_text(size=12),
            plot.margin = margin(2, 10, 0, 2),
            legend.position = "none") 
  })
  
  output$p2 <- renderPlot({
    bia_all_long_rbind <- reactiveData()
    ggplot(bia_all_long_rbind %>% filter(cost_group=="bia_case_detection" & scenario!="s10"),
                 aes(x=year,y=CAD*(-1)/1000000,group=scenario,col=scenario)) +
      scale_color_manual(values=c("#F5594E", "#E7861B", "#95A900", "#00B81F", "#00C0B8", "#00A5FF", "#BF80FF", "#FF61C9"), name="Scenario") +
      scale_x_continuous(expand=c(0.01,0.01)) +
      scale_y_continuous(breaks=seq(0,140,25),expand=c(0.02,0.02),limits=c(0,131)) +
      ylab("Case detection costs (million $)") + xlab("") +
      geom_point() +
      geom_line() +
      theme_bw() + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14),
            legend.text=element_text(size=12),legend.title=element_text(size=12),
            plot.margin = margin(2, 10, 0, 2)) + 
      guides(colour = guide_legend(nrow = 1)) 
  })
  
  output$p3 <- renderPlot({
    bia_all_long_rbind <- reactiveData()
    ggplot(bia_all_long_rbind %>% filter(cost_group=="bia_treat" & scenario!="s10"),
           aes(x=year,y=CAD*(-1)/1000000,group=scenario,col=scenario)) +
      scale_color_manual(values=c("#F5594E", "#E7861B", "#95A900", "#00B81F", "#00C0B8", "#00A5FF", "#BF80FF", "#FF61C9"), name="Scenario") +
      scale_x_continuous(expand=c(0.01,0.01)) +
      scale_y_continuous(expand=c(0.02,0.02)) +
      ylab("Treatment costs (million $)") + xlab("") +
      geom_point() +
      geom_line() +
      theme_bw() +
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14),
            legend.text=element_text(size=12),legend.title=element_text(size=12),
            plot.margin = margin(2, 10, 0, 2)) + 
      guides(colour = guide_legend(nrow = 1)) 
  })
  
  output$p4 <- renderPlot({
    bia_all_long_rbind <- reactiveData()
    ggplot(bia_all_long_rbind %>% filter(cost_group=="bia_hosp" & scenario!="s10"),
           aes(x=year,y=CAD*(-1)/1000000,group=scenario,col=scenario)) +
      scale_color_manual(values=c("#F5594E", "#E7861B", "#95A900", "#00B81F", "#00C0B8", "#00A5FF", "#BF80FF", "#FF61C9"), name="Scenario") +
      scale_x_continuous(expand=c(0.01,0.01)) +
      scale_y_continuous(expand=c(0.02,0.02)) +
      ylab("Hospitalisation costs (million $)") + xlab("") +
      geom_point() +
      geom_line() +
      geom_hline(yintercept=0, linetype="dashed", color = "darkgrey") +
      theme_bw() + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14),
            legend.text=element_text(size=12),legend.title=element_text(size=12),
            plot.margin = margin(2, 10, 0, 2)) + 
      guides(colour = guide_legend(nrow = 1)) 
  })
  
  output$p5 <- renderPlot({
    bia_all_long_rbind <- reactiveData()
    ggplot(bia_all_long_rbind %>% filter(cost_group=="bia_other" & scenario!="s10"),
           aes(x=year,y=CAD*(-1)/1000000,group=scenario,col=scenario)) +
      scale_color_manual(values=c("#F5594E", "#E7861B", "#95A900", "#00B81F", "#00C0B8", "#00A5FF", "#BF80FF", "#FF61C9"), name="Scenario") +
      scale_x_continuous(expand=c(0.01,0.01)) +
      scale_y_continuous(expand=c(0.02,0.02)) +
      ylab("Outpatient care costs (million $)") + xlab("") +
      geom_point() +
      geom_line() +
      theme_bw() + 
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14),
            legend.text=element_text(size=12),legend.title=element_text(size=12),
            plot.margin = margin(2, 10, 0, 2)) + 
      guides(colour = guide_legend(nrow = 1))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
