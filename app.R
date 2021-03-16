library(ggplot2)
library(shiny)
library(shinydashboard)
library(readxl)
library(scales)
library(flexdashboard)
library(plotly)
library(shinyjs)
library(DT)
library(utc)
library(shinyBS)
library(shinyWidgets)



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(menuItem("Vue d'ensemble", icon = icon('dashboard'), tabName = 'dashboard'),
                               menuItem("Modélisation IPMVP", icon = icon('chart-bar'), tabName = 'ipmvp'))
  ),
  dashboardBody(
    
    useShinyjs(),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    
    
    tags$style(HTML("
                    .info-box {
                    min-height: 117px;
                      }
                    .info-box-icon {
                    height: 117px;
                    line-height: 117px;
                    }
                    .info-box-content {
                    padding-top: 0px;padding-bottom: 0px;
                    }
                    


                    .box.box-solid.box-primary>.box-header {
                    color:#000000;
                    background:#ECF0F5
                    }

                    .box.box-solid.box-primary{
                    
                    border-bottom-color:#d7d8d2;
                    border-left-color:#d7d8d2;
                    border-right-color:#d7d8d2;
                    border-top-color:#DCDCDC;
                    background:#ffffff
                    }
                    
                    .box.box-solid.box-primary>.box-body{
                      margin: 0 !important;
                      padding: 0 !important;}


                    /* tabBox background */                    
                              .nav-tabs-custom>.nav-tabs {
                              background-color: #ECF0F5;
                              color:#000000;
                              border-bottom-color:#d7d8d2;
                    border-left-color:#d7d8d2;
                    border-right-color:#d7d8d2;
                    border-top-color:#DCDCDC
                              }


                    .box.box-solid.box-warning>.box-header {
                    color:#000000;
                    background:#fbbb04
                    }

                    .box.box-solid.box-warning{
                    
                    border-bottom-color:#d7d8d2;
                    border-left-color:#d7d8d2;
                    border-right-color:#d7d8d2;
                    border-top-color:#DCDCDC;
                    background:#f9ea8c
                    }
                    
                    
                    .dygraph-annotation{
                        font-size: 14;
                    }


                    .modal-body {padding: 10px}
                                         .modal-content  {;border-radius: 16px !important;}
                                         .modal { text-align: center}
                                         .modal-header
                     {
                         padding:9px 15px;
                         border-bottom:1px solid #eee;
                         background-color: #0480be;
                     }
                     .modal-dialog{ width:1000px}
                     .modal-body{ min-height:230px}
                     
                    #parent {
    list-style: none;
    width: 110%;
    height: 0px;
    margin: 0;
    padding: 0;

}

#parent > li {
    display: inline-block;
    width: 23%;
    height: 50%;
} 


                    #parent_2 {
    list-style: none;
    width: 187%;
    height: 0px;
    margin: 0;
    padding: 0;

}

#parent_2 > li {
    display: inline-block;
    width: 35%;
    height: 50%;
}
                                                  
.margeaaAfficherAction{
  
  margin-left: 0em;
  margin-right: 0em;
  font-family: 'Trebuchet MS', Helvetica, sans-serif;
  font-size: 0.9em;
}


.w-h-t-circle{
    content: '';
    width: 14px;
    height: 14px;
    background-color: #1d61a2;
    position: absolute;
    top: 13px;
    left: 15px;
    border-radius: 50%;
}




                                                  
                    ")),
    tabItems(
      tabItem(tabName = "dashboard",
              fillPage(fillRow(flex=c(1,1,0.03,0.9,1.04),
                               
                               fillCol(box(title = HTML("<b style='font-size: 3.7em; color: #1D60A1; line-height: 0.0001;'>.</b>  <b style='font-size: 1em; line-height: 1; margin-left: 0.4em;' > Informations sur le sites </b> "), status = "primary",solidHeader = T,br(),
                                           includeHTML("informations_sites.html"),br(), img(src="brulefer.jpg",width="100.2%",height=360), width = 12,
                                           height = 751,align = "left"),height = "400"),
                               
                               
                               
                               fillCol(box(title = HTML("<b style='font-size: 3.7em; color: #1D60A1; line-height: 0.0001;'>.</b>  <b style='font-size: 1em; line-height: 1; margin-left: 0.4em;' > Température extérieure actuelle </b> "), status = "primary",solidHeader = T,br(),
                                           imageOutput("plot1",height = 230),width = NULL,height = 385, uiOutput("Temp"),
                                           align = "center"),height = "400"),
                               
                               fillCol(),
                               
                               
                               fillCol(box(title = HTML("<b style='font-size: 3.7em; color: #1D60A1; line-height: 0.0001;'>.</b>  <b style='font-size: 1em; line-height: 1; margin-left: 0.4em;' > Economie d'énergie depuis 2019 </b> "), status = "primary",solidHeader = T,br(),
                                           width = NULL,height = 385,br(),br(),br(),flexdashboard::gaugeOutput("eco",height = "150px"),br(),
                                           HTML('<font face = "Verdana" size = "8" color = "#54C45E"> + 2011 </font> <sup>
                              <font face = "Verdana" size = "5" color = "grey" > MWh </font></sup>'),
                                           align = "center")),
                               
                               
                               fillCol(infoBox(title="Température max" , value = "15 °C", subtitle =HTML('<span style="font-family:monospace;
                    font-size: 0.9em;font-weight: bold; line-height: 1.55;" > Température extérieure maximale enregsitrée pendant
                    la journée </span>'), icon = icon("temperature-high"), width = 12,color = "red"),
                                       
                                       infoBox(title="Température min" , value = "7 °C", subtitle = HTML('<span style="font-family:monospace;
                    font-size: 0.9em;font-weight: bold; line-height: 1.55;" > Température extérieure minimale enregsitrée pendant
                            la journée </span>'), icon = icon("temperature-low"), width = 12,color = "blue"),
                                       
                                       infoBox(title="Température ressentie actuelle" , value = "12 °C", subtitle =HTML('<span style="font-family:monospace;
                    font-size: 0.9em;font-weight: bold; line-height: 1.55;" > Température extérieure ressentie, calculée en fonction de la vitesse du vent </span>'),
                                               icon = icon("temperature-high"),width = 12, color = "olive"), 
                                       
                                       height = "400"),
                               height = "400"
              ),
              fillRow(flex=c(0.92,1.921,0.96), height = "400",
                      fillCol(),
                      fillCol(box(title = HTML("<b style='font-size: 3.7em; color: #1D60A1; line-height: 0.0001;'>.</b>  <b style='font-size: 1em; line-height: 1; margin-left: 0.4em;' > Consommation de l'énergie par rapport à l'année dernière (SJ) </b> "), status = "primary",solidHeader = T,
                                  width = 12,height = 350,plotlyOutput("conso",height = 299,width = "98%"),
                                  align = "center")),
                      fillCol(box(title = tags$b("•   Notes"),
                                  status = "warning",solidHeader = T,
                                  width = NULL,height = 350,br(),HTML('<span style="font-family:monospace;
                    font-size: 1em;font-weight: bold; line-height: 1.55;" > - Penser à rappeler Clément au sujet de la fuite d\'eau dans la cuisine.  </span>'),
                                  align = "center"))
              )
              )),
      
      tabItem(tabName = "ipmvp",
              
              fillPage(
                fillRow(flex=c(1,2.5,1), height = "420",
                        fillCol(),
                        fillCol(box(title = tags$ul(id = "parent_2", tags$li(HTML("<b style='font-size: 3.7em; color: #1D60A1; line-height: 0.0001;'>.</b>  <b style='font-size: 1em; line-height: 1; margin-left: 0.4em;' > Théorique vs. réalisé </b> ")),
                                                    tags$li(tags$ul(id = "parent",tags$li(prettySwitch(inputId = "afficheractions",label = HTML("<b style='font-size: 0.9em; line-height: 1; margin-left: 0em;'> Afficher les actions </b>")
                                                                                                       ,value = TRUE,slim = F,status = "success")),
                                                                    tags$li(HTML(" <p style='font-size: 1.8em; line-height: 1; margin-left: 2.65em; margin-right: 3.8em; color: #BDC3C7;'> | </p>"))
                                                                    , tags$li(actionBttn(inputId = "los",
                                                                                         icon = icon("search-plus"),style = "unite",
                                                                                         color = "primary"))))),
                                    status = "primary",solidHeader = T,br(),
                                    width = 12,height = 415,
                                    br(),br(),
                                    includeHTML("ChargerHighcharts.html"), uiOutput("Barplot"),
                                    align = "center",tags$script(HTML("var largeur = '187%';
    var w = window.innerWidth;
    $('#parent_2').css('width',w*0.87);
$('.sidebar-toggle').bind('click', function() {
  largeur = largeur === w/1.32 ? w*0.87 : w/1.32;
	$('#parent_2').css('width',largeur);
                     
                     });")))
                        ),
                        fillCol()
                ),
                fillRow(flex=c(1,2.5,1), height = "400",
                        fillCol(),
                        fillCol(tabBox(width = 12,height = 330, id = "tabs",
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       tabPanel(HTML("<b style='font-size: 3.7em; color: #1D60A1; line-height: 0.0001;'>.</b>  <b style='font-size: 1.2em; line-height: 1; margin-left: 0.4em;' > Actions réalisées </b> "), uiOutput("marq"),value = "panel1"),
                                       tabPanel(HTML("<b style='font-size: 1.35em; color: #1D60A1; line-height: 0.0001;'>&#9998</b>  <b style='font-size: 1.2em; line-height: 1; margin-left: 0.4em;' > Modifier des actions </b> "),value = "panel2",
                                                DTOutput('shiny_table'),br(),
                                                
                                                tipify(actionButton("add_btn", " Ajouter", icon("plus"), 
                                                                    style="color: white; background-color: green; border-color: black; 
                                                                                   font-weight: bold"),"Ajouter des actions allant à 7 actions max, modifier en double cliquant les cellules", placement = "top"),
                                                tipify(actionButton("delete_btn", " Supprimer", icon("remove"), 
                                                                    style="color: white; background-color: red; border-color: black; 
                                                                                   font-weight: bold; margin-left: 1.2em"),"Supprimer des actions en sélectionnant la ligne en question", placement = "top"),
                                                tipify(actionButton("sauv", " Sauvgarder", icon("save"),
                                                                    style="color: black; background-color: #ECF0F5; border-color: grey; 
                                                                                   font-weight: bold; margin-left: 25em"),"Sauvgarder les modifications", placement = "top"),
                                                align = "center")
                        )),
                        fillCol()
                )
              )
              
      )
      
    ),
    
    
    skin = "blue"
  )
)

server <- function(input, output,session) {
  
  
  
  # bien lire : on ajoute 3 class sur ce truc : skin, sidebare-mini et sidebare collapse bbach tkon collapsé hadi zdtiha nta 
  # en se servant de inspect element :)
  runjs('
        var el2 = document.querySelector(".skin-blue  ");
        el2.className = "skin-blue sidebar-mini sidebar-collapse";
        ')
  
  data = readRDS("data/data.Rds")
  Temp = data$Temperature[length(data$Temperature)]
  
  ### datatable traitement
  # Attention, la colonne des dates est en caractère !
  # Attention2 , C'est le DF this_table qu'il faut utiliser pour le plot desueurs et non pas marqueur.rds
  
  
  this_table = readRDS("data/marqueur.rds")
  this_table <- reactiveVal(this_table)
  
  observeEvent(input$add_btn, {
    t = rbind(this_table(), data.frame(Date = ("XXX"), Type = "XXX", Description = "XXXX", Marqueur = "XXXX"))
    this_table(t)
  })
  
  observeEvent(input$delete_btn, {
    t = this_table()
    print(nrow(t))
    if (!is.null(input$shiny_table_rows_selected)) {
      t <- t[-as.numeric(input$shiny_table_rows_selected),]
    }
    this_table(t)
  })
  
  output$shiny_table <- renderDT({
    datatable(this_table(), selection = 'single', options = list(dom = 't',
                                                                 initComplete = JS(
                                                                   "function(settings, json) {",
                                                                   "$(this.api().table().header()).css({'line-height': '0.5'});",
                                                                   "}")),
              editable = T,rownames = F)%>%
      formatStyle( 0, target= 'row',  lineHeight='65%',backgroundColor = "#ECF0F5")
  })
  
  observeEvent(input$shiny_table_cell_edit, {
    info <- input$shiny_table_cell_edit
    edit_row <-  info$row
    edit_col <-  info$col + 1 
    edit_value <-  info$value
    
    t = this_table()
    t[edit_row,edit_col] <- edit_value
    this_table(t)
  })
  ### data table traitement fin
  
  ### Save Data actions to rds
  observeEvent(input$sauv, {
    setwd("D:/projets_R/Serveur-AWS-main/dash/data/")
    saveRDS(this_table(),"marqueur.Rds")
    setwd("D:/projets_R/Serveur-AWS-main/dash/")
    return(paste("Tableau_marqueurs.html"))
  })
  ### Save Data actions to rds fin
  
  ### mise a jour du html des marqueurs
  RemplacementHtmlMarqueur <- eventReactive(input$sauv, {
    
    data = as.data.frame(this_table())
    latin = readLines("Tableau_marqueurs.html", encoding = "UTF-8")
    data$Date = as.Date(data$Date)
    data = data[rev(order(data$Date)),]
    # Changement de date
    latin[62] = ifelse(is.na(data$Date[1]),"",as.character(data$Date[1]) )
    latin[66] = ifelse(is.na(data$Date[2]),"",as.character(data$Date[2]) )
    latin[70] = ifelse(is.na(data$Date[3]),"",as.character(data$Date[3]) )
    latin[74] = ifelse(is.na(data$Date[4]),"",as.character(data$Date[4]) )
    latin[78] = ifelse(is.na(data$Date[5]),"",as.character(data$Date[5]) )
    latin[82] = ifelse(is.na(data$Date[6]),"",as.character(data$Date[6]) )
    latin[86] = ifelse(is.na(data$Date[7]),"",as.character(data$Date[7]) )
    # Changement de type
    latin[95] = ifelse(is.na(data$Type[1]),"",data$Type[1] )
    latin[99] = ifelse(is.na(data$Type[2]),"",data$Type[2] )
    latin[103] = ifelse(is.na(data$Type[3]),"",data$Type[3] )
    latin[107] = ifelse(is.na(data$Type[4]),"",data$Type[4] )
    latin[111] = ifelse(is.na(data$Type[5]),"",data$Type[5] )
    latin[115] = ifelse(is.na(data$Type[6]),"",data$Type[6] )
    latin[119] = ifelse(is.na(data$Type[7]),"",data$Type[7] )
    # Changement de Description
    latin[131] = ifelse(is.na(data$Description[1]),"",data$Description[1] )
    latin[135] = ifelse(is.na(data$Description[2]),"",data$Description[2] )
    latin[139] = ifelse(is.na(data$Description[3]),"",data$Description[3] )
    latin[143] = ifelse(is.na(data$Description[4]),"",data$Description[4] )
    latin[147] = ifelse(is.na(data$Description[5]),"",data$Description[5] )
    latin[151] = ifelse(is.na(data$Description[6]),"",data$Description[6] )
    latin[155] = ifelse(is.na(data$Description[7]),"",data$Description[7] )
    # Changement de Marqueur
    latin[167] = ifelse(is.na(data$Marqueur[1]),"",data$Marqueur[1] )
    latin[171] = ifelse(is.na(data$Marqueur[2]),"",data$Marqueur[2] )
    latin[175] = ifelse(is.na(data$Marqueur[3]),"",data$Marqueur[3] )
    latin[179] = ifelse(is.na(data$Marqueur[4]),"",data$Marqueur[4] )
    latin[183] = ifelse(is.na(data$Marqueur[5]),"",data$Marqueur[5] )
    latin[187] = ifelse(is.na(data$Marqueur[6]),"",data$Marqueur[6] )
    latin[191] = ifelse(is.na(data$Marqueur[7]),"",data$Marqueur[7] )
    
    
    con <- file("Tableau_marqueurs.html", open="w", encoding = "UTF-8")
    writeLines(latin, con)
    close(con,type = "w")
    
    setwd("D:/projets_R/Serveur-AWS-main/dash")
    return(paste("Tableau_marqueurs.html"))
  } , ignoreNULL = F)
  
  output$marq <-renderUI({includeHTML(RemplacementHtmlMarqueur())}) 
  ### mise a jour du html des marqueurs fin
  
  ## Zoom ##
  observeEvent((input$los), {
    showModal(modalDialog(
      renderUI({
        fillPage(includeHTML("modelisation_ipmvp_agrandi.html"))
      }),
      easyClose = TRUE,
      size = "m",
      footer = NULL
    ))
  })
  
  ## Zoom ##
  
  
  
  ####### Diriger vers le premier panel si "Sauvgarder" est cliquée #########
  
  observeEvent(input$sauv, {
    updateTabItems(session, "tabs",
                   selected = "panel1")
  })
  
  ####### Diriger vers le premier panel si "Sauvgarder" est cliquée ######### fin
  
  
  
  ####### Annotation : Mise à jours du plot modélisation #########
  RemplacementHtmlBarplot <- eventReactive(
    {input$sauv
      input$afficheractions},
    
    {
      
      data = as.data.frame(this_table())
      
      #### modelisation_ipmvp.html
      latin = readLines("modelisation_ipmvp.html", encoding = "UTF-8")
      data$Date <- as.Date(data$Date)
      if(input$afficheractions){
        # Changement d'annotation
        latin[171] = ifelse(is.na(data$Date[1]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[1]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[1],"' },"  ) )
        latin[172] = ifelse(is.na(data$Date[2]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[2]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[2],"' },"  ) )
        latin[173] = ifelse(is.na(data$Date[3]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[3]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[3],"' },"  ) )
        latin[174] = ifelse(is.na(data$Date[4]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[4]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[4],"' },"  ) )
        latin[175] = ifelse(is.na(data$Date[5]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[5]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[5],"' },"  ) )
        latin[176] = ifelse(is.na(data$Date[6]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[6]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[6],"' },"  ) )
        latin[177] = ifelse(is.na(data$Date[7]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[7]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[7],"' },"  ) )
        # Droite verticale
        latin[151] = ifelse(is.na(data$Date[1]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[1]))), ",zIndex: 4}," ) )
        latin[152] = ifelse(is.na(data$Date[2]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[2]))), ",zIndex: 4}," ) )
        latin[153] = ifelse(is.na(data$Date[3]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[3]))), ",zIndex: 4}," ) )
        latin[154] = ifelse(is.na(data$Date[4]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[4]))), ",zIndex: 4}," ) )
        latin[155] = ifelse(is.na(data$Date[5]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[5]))), ",zIndex: 4}," ) )
        latin[156] = ifelse(is.na(data$Date[6]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[6]))), ",zIndex: 4}," ) )
        latin[157] = ifelse(is.na(data$Date[7]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[7]))), ",zIndex: 4}," ) )
        con <- file("modelisation_ipmvp.html", open="w", encoding = "UTF-8")
        writeLines(latin, con)
        close(con,type = "w")
        #### modelisation_ipmvp.html
        
        
        
        
        #### modelisation_ipmvp_agrandi.html
        latin = readLines("modelisation_ipmvp_agrandi.html", encoding = "UTF-8")
        data$Date <- as.Date(data$Date)
        # Changement d'annotation
        latin[171] = ifelse(is.na(data$Date[1]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[1]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[1],"' },"  ) )
        latin[172] = ifelse(is.na(data$Date[2]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[2]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[2],"' },"  ) )
        latin[173] = ifelse(is.na(data$Date[3]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[3]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[3],"' },"  ) )
        latin[174] = ifelse(is.na(data$Date[4]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[4]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[4],"' },"  ) )
        latin[175] = ifelse(is.na(data$Date[5]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[5]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[5],"' },"  ) )
        latin[176] = ifelse(is.na(data$Date[6]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[6]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[6],"' },"  ) )
        latin[177] = ifelse(is.na(data$Date[7]),"", paste0( "{point: {x : ", as.character(1000*as.integer(toUTC(data$Date[7]))), ",y: 15,xAxis: 0,},shape: 'circle',style: {fontSize: '15px',fontWeight: 'bold',fontFamily : 'Lucida Sans'},align:'center',text: '", data$Marqueur[7],"' },"  ) )
        # Droite verticale
        latin[151] = ifelse(is.na(data$Date[1]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[1]))), ",zIndex: 4}," ) )
        latin[152] = ifelse(is.na(data$Date[2]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[2]))), ",zIndex: 4}," ) )
        latin[153] = ifelse(is.na(data$Date[3]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[3]))), ",zIndex: 4}," ) )
        latin[154] = ifelse(is.na(data$Date[4]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[4]))), ",zIndex: 4}," ) )
        latin[155] = ifelse(is.na(data$Date[5]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[5]))), ",zIndex: 4}," ) )
        latin[156] = ifelse(is.na(data$Date[6]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[6]))), ",zIndex: 4}," ) )
        latin[157] = ifelse(is.na(data$Date[7]),"", paste0("{color: '#1D60A1',width: 1.45,value: ", as.character(1000*as.integer(toUTC(data$Date[7]))), ",zIndex: 4}," ) )
        con <- file("modelisation_ipmvp_agrandi.html", open="w", encoding = "UTF-8")
        writeLines(latin, con)
        close(con,type = "w")} else {
          
          # Changement d'annotation
          latin[171] = ""
          latin[172] = ""
          latin[173] = ""
          latin[174] = ""
          latin[175] = ""
          latin[176] = ""
          latin[177] = ""
          # Droite verticale
          latin[151] = ""
          latin[152] = ""
          latin[153] = ""
          latin[154] = ""
          latin[155] = ""
          latin[156] = ""
          latin[157] = ""
          con <- file("modelisation_ipmvp.html", open="w", encoding = "UTF-8")
          writeLines(latin, con)
          close(con,type = "w")
          #### modelisation_ipmvp.html
          
          
          
          
          #### modelisation_ipmvp_agrandi.html
          latin = readLines("modelisation_ipmvp_agrandi.html", encoding = "UTF-8")
          data$Date <- as.Date(data$Date)
          # Changement d'annotation
          latin[171] = ""
          latin[172] = ""
          latin[173] = ""
          latin[174] = ""
          latin[175] = ""
          latin[176] = ""
          latin[177] = ""
          # Droite verticale
          latin[151] = ""
          latin[152] = ""
          latin[153] = ""
          latin[154] = ""
          latin[155] = ""
          latin[156] = ""
          latin[157] = ""
          con <- file("modelisation_ipmvp_agrandi.html", open="w", encoding = "UTF-8")
          writeLines(latin, con)
          close(con,type = "w")
          
        }
      
      
      
      
      
      setwd("D:/projets_R/Serveur-AWS-main/dash")
      return(paste("modelisation_ipmvp.html"))
    } , ignoreNULL = FALSE)
  
  output$Barplot <-renderUI({includeHTML(RemplacementHtmlBarplot())}) 
  
  ####### Annotation : Mise à jours du plot modélisation ######### fin
  
  
  
  
  output$brulefer <- renderImage({
    
    image_file <- paste0("www/brulefer.png")
    return(list(
      src = image_file,
      filetype = "image/png",
      height = "369",
      width = "100%"
    ))
    
  }, deleteFile = FALSE)
  
  ###
  
  output$plot1 <- renderImage({
    
    image_file <- paste0("www/thermo.jpg")
    
    return(list(
      src = image_file,
      filetype = "image/jpeg",
      height = 230,
      width = 230
    ))
    
  }, deleteFile = FALSE)
  
  
  
  output$Temp <- renderUI({
    if(Temp>0 & Temp<=15){
      HTML(paste0('<font face = "Verdana" size = "8" color = "#87c2ea">+', Temp ,'</font>',
                  '<sup><font face = "Verdana" size = "5" color = "grey" >', " °C" ,'</font></sup>'))
    } else if(Temp<=0){
      HTML(paste0('<font face = "Verdana" size = "8" color = "#0000FF">', Temp ,'</font>',
                  '<sup><font face = "Verdana" size = "5" color = "grey" >', " °C" ,'</font></sup>'))
    } else if(Temp>15 & Temp<=27.5){
      HTML(paste0('<font face = "Verdana" size = "8" color = "#000000">+', Temp ,'</font>',
                  '<sup><font face = "Verdana" size = "5" color = "grey" >', " °C" ,'</font></sup>'))
    } else if(Temp>27.5 & Temp<=35){
      HTML(paste0('<font face = "Verdana" size = "8" color = "#FF9900">+', Temp ,'</font>',
                  '<sup><font face = "Verdana" size = "5" color = "grey" >', " °C" ,'</font></sup>'))
    } else if(Temp>35){
      HTML(paste0('<font face = "Verdana" size = "8" color = "#DC3912">+', Temp ,'</font>',
                  '<sup><font face = "Verdana" size = "5" color = "grey" >', " °C" ,'</font></sup>'))
    }
  })
  
  
  output$conso <- renderPlotly({
    df <- as.data.frame(read_excel("saint-james-test-ipmvb.xlsx"))
    souf_df = df[,c("Periode.de.suivi","Consommation après APE")]
    names(souf_df) = c(c("Periode.de.suivi","Consommatiavant APE"))
    data <- rbind(souf_df[,c("Periode.de.suivi","Consommatiavant APE")],
                  df[,c("Periode.de.suivi","Consommatiavant APE")])
    names(data) = c("Date","Conso en (MWh)")
    data$`Conso en (MWh)` = data$`Conso en (MWh)`/1000
    data$Date = as.Date(data$Date)
    data$alphayr = c(rep(c(0.8,1),12))
    data$Type = c(rep("Période N",12),rep("Période N-1",12))
    df2 = data
    
    gg = ggplot(data=df2)+
      geom_bar(aes(x=Date, y=`Conso en (MWh)`, fill=Type),stat="identity", position=position_dodge2(),alpha=df2$alphayr,width = 20)+
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", breaks = c()) +
      theme_minimal()+
      theme(
        axis.title.x=element_blank(),
        axis.text.x = element_text(colour = "darkgrey", size = 10,face = "bold"),
        axis.title.y = element_text( colour = "darkgrey",size = 12),
        axis.text.y = element_text( colour = "darkgrey",size = 9,face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )+
      scale_fill_manual(name = "",values=c('#C0E6D2','#3CAE68'))+
      ylab("MWh")
    
    ggplotly(gg, tooltip = "Conso en (MWh)") %>%
      layout(legend = list(orientation = "h", x = 0.31, y =-0.15))
    
    
  })
  
  
  
  
  output$eco <- flexdashboard::renderGauge({
    gauge(31,0,100,sectors = gaugeSectors(colors = c("#54C45E","#54C45E","#54C45E")),symbol = " %",label = "")
  })
}

shinyApp(ui, server)

