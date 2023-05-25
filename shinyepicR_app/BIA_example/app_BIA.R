# load packages

library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)

# load and wrangle data ### edit this so that I only have to load in 3 dfs to the app. No point having all of the filtering here. 
# 0.05 to 0.13 
all_overall_10000_005_013 <- readRDS("data_10000/all_overall_10000_005_013.RDa")
all_results_10000_005_013 <- readRDS("data_10000/all_results_10000_005_013.RDa")
bia_all_long_10000_005_013 <- readRDS("data_10000/bia_all_long_10000_005_013.RDa")

# 0.05 to 0.25
all_overall_10000_005_025 <- readRDS("data_10000/all_overall_10000_005_025.RDa")
all_results_10000_005_025 <- readRDS("data_10000/all_results_10000_005_025.RDa")
bia_all_long_10000_005_025 <- readRDS("data_10000/bia_all_long_10000_005_025.RDa")

#rbind dfs 
all_overall_rbind <- rbind(all_overall_10000_005_013, all_overall_10000_005_025)
all_results_rbind <- rbind(all_results_10000_005_013, all_results_10000_005_025)
bia_all_long_rbind <- rbind(bia_all_long_10000_005_013, bia_all_long_10000_005_025)


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
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
           plotOutput("p1"),
           plotOutput("p2"), 
           plotOutput("p3"), 
           plotOutput("p4"), 
           plotOutput("p5")
         ),
        tabPanel("Total Costs",
             dataTableOutput("bia_total"))
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$bia_total <- renderDataTable({
    bia_all_long_rbind %>% 
      filter(round(uptake_step, 2)==input$uptake_step & year!="2021") %>%
      group_by(scenario,cost_group) %>% 
      summarize(total_CAD=sum(CAD)) %>% 
      pivot_wider(names_from=scenario,values_from=total_CAD) 
  })
  
  output$p1 <- renderPlot({
    ggplot(bia_all_long_rbind %>% filter(round(uptake_step, 2)==input$uptake_step & cost_group=="bia_total" & scenario!="s10"), 
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
    ggplot(bia_all_long_rbind %>% filter(round(uptake_step, 2)==input$uptake_step & cost_group=="bia_case_detection" & scenario!="s10"),
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
    ggplot(bia_all_long_rbind %>% filter(round(uptake_step, 2)==input$uptake_step & cost_group=="bia_treat" & scenario!="s10"),
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
    ggplot(bia_all_long_rbind %>% filter(round(uptake_step, 2)==input$uptake_step & cost_group=="bia_hosp" & scenario!="s10"),
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
    ggplot(bia_all_long_rbind %>% filter(round(uptake_step, 2)==input$uptake_step & cost_group=="bia_other" & scenario!="s10"),
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
