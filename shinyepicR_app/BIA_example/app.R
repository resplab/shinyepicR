# load packages

library(shiny)
library(shinyBS)
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

### load and wrangle data ### edit this so that I only have to load in 3 dfs to the app. No point having all of the filtering here. 
# 0.01
all_overall_18mio_001_005 <- readRDS("data_18mio/all_overall_18mio_001_005.RDa")
all_results_18mio_001_005 <- readRDS("data_18mio/all_results_18mio_001_005.RDa")
bia_all_long_18mio_001_005 <- readRDS("data_18mio/bia_all_long_18mio_001_005.RDa")

# 0.02
all_overall_18mio_002_010 <- readRDS("data_18mio/all_overall_18mio_002_010.RDa")
all_results_18mio_002_010 <- readRDS("data_18mio/all_results_18mio_002_010.RDa")
bia_all_long_18mio_002_010 <- readRDS("data_18mio/bia_all_long_18mio_002_010.RDa")

# 0.05
all_overall_18mio_005_025 <- readRDS("data_18mio/all_overall_18mio_005_025.RDa")
all_results_18mio_005_025 <- readRDS("data_18mio/all_results_18mio_005_025.RDa")
bia_all_long_18mio_005_025 <- readRDS("data_18mio/bia_all_long_18mio_005_025.RDa")

# 0.08
all_overall_18mio_008_040 <- readRDS("data_18mio/all_overall_18mio_008_040.RDa")
all_results_18mio_008_040 <- readRDS("data_18mio/all_results_18mio_008_040.RDa")
bia_all_long_18mio_008_040 <- readRDS("data_18mio/bia_all_long_18mio_008_040.RDa")

# 0.10
all_overall_18mio_010_050 <- readRDS("data_18mio/all_overall_18mio_010_050.RDa")
all_results_18mio_010_050 <- readRDS("data_18mio/all_results_18mio_010_050.RDa")
bia_all_long_18mio_010_050 <- readRDS("data_18mio/bia_all_long_18mio_010_050.RDa")

# rbind dfs 
all_overall_rbind <- rbind(all_overall_18mio_001_005, 
                           all_overall_18mio_002_010,
                           all_overall_18mio_005_025,
                           all_overall_18mio_008_040,
                           all_overall_18mio_010_050)
all_results_rbind <- rbind(all_results_18mio_001_005,
                           all_results_18mio_002_010,
                           all_results_18mio_005_025,
                           all_results_18mio_008_040,
                           all_results_18mio_010_050)
bia_all_long_rbind <- rbind(bia_all_long_18mio_001_005, 
                            bia_all_long_18mio_002_010,
                            bia_all_long_18mio_005_025,
                            bia_all_long_18mio_008_040,
                            bia_all_long_18mio_010_050)

# figure legend
scen_leg <- "S1a CDQ ≥ 17 points for all patients; S1b flow meter (with bronchodilator) all patients; S1c CDQ ≥ 17 points + flow meter (with bronchodilator) all patients; 
           S2a flow meter (without bronchodilator) among symptomatic patients; S3a CDQ ≥ 19.5 points among patients aged ≥50 years with a smoking history; 
           S3b CDQ ≥ 16.5 points among patients aged ≥50 years with a smoking history; S3c flow meter (without bronchodilator) among patients aged ≥50 years with a smoking history, 
           S3d CDQ ≥ 17 points + flow meter (with bronchodilator) among patients aged ≥50 years with a smoking history."

# Define UI 
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Budget impact analysis of adopting primary care-based COPD case detection in the Canadian general population"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "uptake_step", 
        label = "Physician uptake (Starting value and yearly increase are the same)", 
        choices = c(unique(bia_all_long_rbind$uptake_step)),
        selected = unique(bia_all_long_rbind$uptake_step)[3]
      ),
      # unit costs (values taken from Table 2)
      numericInput(
        inputId = "UC_cd_time", 
        label = "Cost of administration of COPD Diagnostic Questionnaire", 
        value = 11.91
      ),
      numericInput(
        inputId = "UC_cd_fm_bronch", 
        label = "Cost of flow-meter with bronchodilator", 
        value = 30.81
      ),
      numericInput(
        inputId = "UC_cd_fm", 
        label = "Cost of flow-meter (without bronchodilator)", 
        value = 24.68
      ),
      numericInput(
        inputId = "UC_OPd",
        label = "Outpatient diagnosis cost", 
        value = round(all_results_rbind$UC_OP_diagnosis[1], 2)
      ),
      actionButton(
        inputId = "update_ucs",
        label = "Run"
      ),
      bsTooltip(
        id = "uptake_step", 
        title = "Physician uptake as a decimal. This number specifies the starting uptake and yearly increase. 
                For ex, 0.05 means 5% uptake in 2021, and 5% increase in uptake each year (25% uptake in 2026)",
        placement = "right", 
        options = list(container = "body")
      ),
      bsTooltip(
        id = "UC_cd_time", 
        title = "Administering case detection was costed at 34% of a 15-minute routine primary care visit",
        placement = "right", 
        options = list(container = "body")
      ),
      bsTooltip(
        id = "UC_OPd", 
        title = "Outpatient diagnosis includes the cost of diagnostic spirometry plus a primary care visit to interpret the results",
        placement = "right", 
        options = list(container = "body")
      )

    ),
    
    ### must update the first line of each legend, create a single str object 
    mainPanel(
      tabsetPanel(
        tabPanel("Total costs",
           plotOutput("p1"),
           p(em(paste("Figure: Annual total costs (million $) compared to no case detection baseline scenario. Negative additional costs indicate cost savings.", scen_leg)))
        ),
        tabPanel("Case detection costs",
           plotOutput("p2"),
           p(em(paste("Figure: Annual case detection costs (million $) compared to no case detection baseline scenario. Negative additional costs indicate cost savings.", scen_leg)))
        ),
        tabPanel("Treatment costs",
           plotOutput("p3"),
           p(em(paste("Figure: Annual treatment costs (million $) compared to no case detection baseline scenario. Negative additional costs indicate cost savings.", scen_leg)))
        ),
        tabPanel("Hospitalization costs",
           plotOutput("p4"),
           p(em(paste("Figure: Annual hospitalization costs (million $) compared to no case detection baseline scenario. Negative additional costs indicate cost savings.", scen_leg)))
        ),
        tabPanel("Outpatient care costs",
           plotOutput("p5"),
           p(em(paste("Figure: Annual outpatient care costs (million $) compared to no case detection baseline scenario. Negative additional costs indicate cost savings.", scen_leg)))
        ),
        tabPanel("Total costs (table)",
          dataTableOutput("bia_total"),
          p(em(paste("Table: Total budget impact (no case detection – case detection) results. Negative budget impact indicates budget expansion.", scen_leg)))
        ),
        tabPanel("About",
          h1("About"),
          p("This web application is for analyzing budget impact of adopting primary care-based COPD case detection in the Canadian general population. 
            The analysis was performed using the Evaluation Platform in COPD (EPIC) which is a microsimulation model of COPD in Canadians aged 40 years and above. 
            Please refer to the manuscript for details on the model."), 
          h1("Citation"), 
          p(a("Rachael Mountain, MSC, ", href = "https://www.lancaster.ac.uk/health-and-medicine/about-us/people/rachael-mountain"), 
            a("Kate M. Johnson, PhD", href = "https://resp.core.ubc.ca/team/Kate_Johnson")),
          h1("Credits"),
          p("This web app was created by Dexter Kim at the University of British Columbia, Faculty of Medicine")
          )
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  #update unit costs (uc)
  reactiveData <- eventReactive(input$update_ucs, {
    
    #update all_results_rbind with new ucs
    all_results_rbind <- all_results_rbind %>%
      filter(round(uptake_step, 2)==input$uptake_step) #filter all_results_rbind by uptake parameter
    
    all_results_rbind <- all_results_rbind %>%
      mutate(CD_uc = case_when(
        scenario=="s10" ~ 0,
        scenario=="s1a" ~ input$UC_cd_time,
        scenario=="s1b" ~ input$UC_cd_time + input$UC_cd_fm_bronch,
        scenario=="s1c" ~ 2*input$UC_cd_time + input$UC_cd_fm_bronch,
        scenario=="s2a" ~ input$UC_cd_time + input$UC_cd_fm,
        scenario=="s3a" ~ input$UC_cd_time,
        scenario=="s3b" ~ input$UC_cd_time,
        scenario=="s3c" ~ input$UC_cd_time + input$UC_cd_fm,
        scenario=="s3d" ~ 2*input$UC_cd_time + input$UC_cd_fm_bronch
      )) %>%
      mutate(OPd_uc = input$UC_OPd)
    
    # recalculate cost_total and cost_case_detection 
    all_results_rbind <- all_results_rbind %>%
      mutate(cost_total = cost_total - cost_case_detection) %>%
      mutate(cost_case_detection = case_detection*CD_uc + (CD_true_pos+CD_false_pos)*OPd_uc) %>%
      mutate(cost_total = cost_total + cost_case_detection)
    
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
  
  # total costs table
  output$bia_total <- renderDataTable({
    bia_all_long_rbind <- reactiveData() # access output of eventReactive
    bia_all_long_rbind %>%
      filter(year!="2021") %>%
      group_by(scenario,cost_group) %>%
      summarize(total_CAD=sum(CAD)) %>%
      pivot_wider(names_from=scenario,values_from=total_CAD) %>%
      mutate(cost_group = c("Total cost of case detection", 
                            "Total cost of hospitalizations",
                            "Total cost of maintenance",
                            "Total cost of outpatient care", #cost_other
                            "Total cost",
                            "Total cost of treatment",
                            "Budget impact of case detection",
                            "Budget impact of hospitalizations",
                            "Budget impact of maintenance",
                            "Budget impact of outpatient care",
                            "Budget impact (total)",
                            "Budget impact of treatment"))
  })
  
  # plots
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
            legend.text=element_text(size=12),legend.title=element_text(size=12), legend.position = "bottom",
            plot.margin = margin(2, 10, 0, 2)) +
            guides(colour = guide_legend(nrow = 1)) 
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
            legend.text=element_text(size=12),legend.title=element_text(size=12), legend.position = "bottom",
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
            legend.text=element_text(size=12),legend.title=element_text(size=12), legend.position = "bottom",
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
            legend.text=element_text(size=12),legend.title=element_text(size=12), legend.position = "bottom",
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
            legend.text=element_text(size=12),legend.title=element_text(size=12), legend.position = "bottom",
            plot.margin = margin(2, 10, 0, 2)) + 
      guides(colour = guide_legend(nrow = 1))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
