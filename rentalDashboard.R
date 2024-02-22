# Install packages for Dashboard
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus")
# install.packages("shinyjqui")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("shinyWidgets")
# install.packages("leaflet")
# install.packages("devtools")
# install.packages("geojsonio")
# install.packages("tidyverse")
# install.packages("rlist")

#Load packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjqui)
library(httr)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(leaflet) 
library(leaflet.providers)
library(devtools)
devtools::install_github('wleepang/shiny-pager-ui', quiet = TRUE)
library(shinyPagerUI)
library(tidyverse)
library(rgdal)
library(geojsonio)
library(pillar)
library(sf)
library(waiter)
library(rlist)

#######################
# LOAD DATA FILES     #
#######################

# Change home path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set current file path as working dir
# setwd("/Users/declanwatson/Documents/Info_vis/info_vis_ass_3/")
home= paste0(getwd())

#######################
# SERVER DATA PARSING #
#######################

# Loading and removing duplicates from Places of Interest data
poi_categories = c("Cafe", "Food & Drinks", "Gym", "Health", "Places of Worship", "Shops & Supermarkets", "Tourism & Entertainment")
poi_cafe = fromJSON('data/Points_of_interest/categorised_data/Cafe.json')
poi_cafe <- poi_cafe[!duplicated(poi_cafe$place_id), ] # Remove duplicates
poi_food = fromJSON('data/Points_of_interest/categorised_data/Food & Drinks.json')
poi_food <- poi_food[!duplicated(poi_food$place_id), ] # Remove duplicates
poi_gym = fromJSON('data/Points_of_interest/categorised_data/Gym.json')
poi_gym <- poi_gym[!duplicated(poi_gym$place_id), ] # Remove duplicates
poi_health = fromJSON('data/Points_of_interest/categorised_data/Health.json')
poi_health <- poi_health[!duplicated(poi_health$place_id), ] # Remove duplicates
poi_shops = fromJSON('data/Points_of_interest/categorised_data/Shops & Supermarkets.json')
poi_shops <- poi_shops[!duplicated(poi_shops$place_id), ] # Remove duplicates
poi_tourism = fromJSON('data/Points_of_interest/categorised_data/Tourism & Entertainment.json')
poi_tourism <- poi_tourism[!duplicated(poi_tourism$place_id), ] # Remove duplicates
poi_rating_labels = c("Poor", "Average", "High") # Used for scale 

#######################
# HELPER FUNCTIONS #
#######################

# Add helper functions here

##################
# USER INTERFACE #
##################

sidebar <- dashboardSidebar(
  # Setting width of the sidebar
  width = 300,
  minified = FALSE,
  sidebarMenu(
    # Assigning id to tabs
    id = "tabs",
    menuItem("Home", tabName = "home", icon = icon("home", verify_fa = FALSE)),
    menuItem("Apartment Hunting", tabName = "searchapt", icon = icon("building", verify_fa = FALSE)),
    conditionalPanel('input.tabs == "searchapt"',
                     sliderInput("maxPrice",label = "Maximum Price",min = 500,max = 2000,value = 2000,ticks = FALSE)),
    conditionalPanel('input.tabs == "searchapt"',
                     sliderInput("bedrooms",label = "Maximum bedrooms",min = 1,max = 4,value = 2, ticks = FALSE)),
    conditionalPanel('input.tabs == "searchapt"',
                     sliderInput("bathrooms",label = "Maximum bathrooms",min = 1,max = 4,value = 2, ticks = FALSE)),
    conditionalPanel('input.tabs == "searchapt"',
                     selectInput("sortby",label = "Sort By",choices = c('Default', 'Price'),selected = 'Default')),
    conditionalPanel('input.tabs == "searchapt"',
                     selectInput("direction",label = "Sort Direction", choices = c('Ascending', 'Descending'),selected = 'Ascending')),
    menuItem("Places of Interest", tabName = "places", icon=icon("fa-solid fa-landmark", verify_fa = FALSE)),
    menuItem("Transport", tabName = "transport", icon = icon("fa-regular fa-train-tram", verify_fa = FALSE)), 
    menuItem("Environment", tabName = "environment", icon = icon("fa-regular fa-tree", verify_fa = FALSE))
  )
)

body <- dashboardBody( 
  tags$head( 
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "searchapt",
            uiOutput("boxes"),
            fluidRow(column(width=12,offset = 4,pageruiInput('pager', page_current = 1, pages_total = 10))),
            br(),
            fluidRow(column(width = 12,box(id='locationmap',title = NULL,solidHeader = FALSE,width = NULL, closable = TRUE,
                                           leafletOutput('rentMap', height = 500)))),
            fluidRow(column(width = 6, offset = 4,
                            actionGroupButtons(inputIds = c("show_map","hide_map"),
                                               labels = list("Show map of current houses", "Hide map of current houses")))),
    ),
    tabItem(tabName = "transport",
            leafletOutput("transport", height = "800px"),
            br(),
            fluidRow(column(width = 6, offset = 5,
                            actionGroupButtons(inputIds = c('transportSMap', 'transportHMap'),
                                               labels = list("Show saved houses", "Remove saved houses"))
                            ))
    ),
    tabItem(tabName = "environment",
            leafletOutput("environment", height = "800px"),
            br(),
            fluidRow(column(width = 6, offset = 5,
                            actionGroupButtons(inputIds = c('envSMap', 'envHMap'),
                                               labels = list("Show saved houses", "Remove saved houses"))
            ))
    ),
    tabItem(tabName= "places", 
            leafletOutput("places", height = "800px"),
            br(),
            fluidRow(column(width = 6, offset = 5,
                            actionGroupButtons(inputIds = c('placesSMap', 'placesHMap'),
                                               labels = list("Show saved houses", "Remove saved houses"))
            ))
    ),
    tabItem(tabName = "home",
             htmlOutput("homePage")
    )
  )

)

# Initializing dashboard page UI 
ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
  # Adding header title
  dashboardHeader(title = "Student Living Portal",
                  titleWidth = 300),
  sidebar,
  body,
  footer = dashboardFooter(left = "Developed by: Abhijeet, Declan, Madhavi, Mehrnoosh")
)



################
# SHINY SERVER #
################

server <- function(input, output, session) {
  
  ##########################################################################################
  # Reactive values for the server
  rvs <- reactiveValues(aptRows = list(), 
                        filterLat = -37.840935, filterLong = 144.946457, 
                        starHouses = list(), starDfHouses = NULL)
  ##########################################################################################
  
  # Reactive function for calling the domain API on changing filters or page.
  reactResponse <- reactive({
    # Request for the domain API call
    requestForApartments <- list(listingType = "Rent",
                                 propertyTypes = c("House","NewApartments","ApartmentUnitFlat","Studio"),
                                 propertyFeatures = list(),
                                 listingAttributes = c("HasPhotos","HasPrice"),
                                 propertyEstablishedType = "Any",
                                 minBedrooms = 1,
                                 maxBedrooms = input$bedrooms,
                                 minBathrooms = 1,
                                 maxBathrooms = input$bathrooms,
                                 minCarspaces = 0,
                                 maxCarspaces = 4,
                                 minPrice = 300,
                                 maxPrice = input$maxPrice,
                                 locations = list(list(state = "VIC",
                                                       region = "",
                                                       area = "",
                                                       suburb = "Melbourne",
                                                       postCode = "",
                                                       includeSurroundingSuburbs = FALSE)),
                                 sort = list(sortKey = input$sortby,
                                             direction = input$direction),
                                 pageNumber = input$pager$page_current,
                                 pageSize = 6
    )
    
    # Calling the API and storing the response
    response <- POST(url = "https://api.domain.com.au/v1/listings/residential/_search",
                     add_headers(.headers = c('X-Api-Key' = 'key_71860e867195148b05d3b927fef00538')),
                     body = toJSON(requestForApartments, auto_unbox = TRUE),
                     encode = "raw")
    
    # Fetching the total records available for displaying
    totalRecords <- as.integer(headers(response)$`x-total-count`)
    
    # Getting the page count by dividing the total records by page size(i.e 6)
    pageCount <- floor(totalRecords/6)
    
    # Default max page count is 10, but if records are less update the total pages in pager ui input
    if(pageCount < 10) {
      if(totalRecords %% 2 != 0) {
        count <- count - 7
        pageCount <- (count/6)
      }
      updatePageruiInput(session, 'pager', pages_total = pageCount)
    }
    
    # Returning the JSON response in variable
    apartmentResponse <- fromJSON(content(response, "text"), flatten = TRUE)
  })
  
  ##########################################################################################
  
  # Creating an observer for responses from filters for apartment hunting tab and dynamically 
  # updating the UI using render UI.
  observe({
    # Assigning a local variable for reactive function response
    aptReactResponse <- reactResponse()
    
    # For the number of results in the response
    for (row in 1:nrow(aptReactResponse)) {
      # Creating a social box for each listing
      rvs$aptRows[[row]] <- socialBox(
        id = paste0("socialBox", row),
        collapsible = FALSE,
        width = NULL,
        # Good looking title using the user block
        title = userBlock(
          image = aptReactResponse[row, "listing.advertiser.logoUrl"],
          title = aptReactResponse[row, "listing.advertiser.name"],
          subtitle = aptReactResponse[row, "listing.advertiser.type"]),
        # Displaying the headline and price details with a save button
        fluidRow(column(width = 9,HTML(paste0('<b>',aptReactResponse[row, "listing.headline"],'</b><br>',
                                              '<b>',aptReactResponse[row, "listing.priceDetails.displayPrice"],'</b>'))),
                 # Save button UI changes if the apartment has been saved earlier
                 column(width = 3,actionButton(inputId = paste0('star',row),class= "btn-default",
                                               label = case_when(list.any(rvs$starHouses, aptReactResponse[row, "listing.id"] 
                                                                          %in% listing.id) == TRUE ~ 'Saved',TRUE ~ 'Save'),
                                               icon = icon(case_when(list.any(rvs$starHouses, 
                                                                              aptReactResponse[row, "listing.id"] 
                                                                              %in% listing.id) == TRUE  ~ "check",TRUE ~ "star"))
                                               )
                        )),
        br(),
        # Adding image carousel to display different images in response
        fluidRow(column(width = 12,carousel(id = paste0("imageCarousel", row),width = NULL,.list = Map(function(i) {
          carouselItem(tags$img(src = paste0(aptReactResponse[row, "listing.media"][[1]]$url[[i]]))
                                   )}, 1:length(aptReactResponse[row, "listing.media"][[1]]$url)))
        )),
        br(),
        # Displaying the address 
        fluidRow(column(width = 12,
                        HTML(paste0('<b>',aptReactResponse[row, "listing.propertyDetails.displayableAddress"],'</b><br>')))),
        # Adding the features of house with respective icons
        fluidRow(column(width = 12,icon("bed"),aptReactResponse[row, "listing.propertyDetails.bedrooms"],
                        icon("bath"),aptReactResponse[row, "listing.propertyDetails.bathrooms"],
                        icon("car"),aptReactResponse[row, "listing.propertyDetails.carspaces"])),
        br(),
        # Creating a collapsed details box 
        fluidRow(column(width = 12,box(title = "Details", width = 12,collapsible = TRUE, collapsed = TRUE,
                            HTML(aptReactResponse[row, "listing.summaryDescription"]),
                            br(),
                            # Adding a user box for the contacts of the listing
                            fluidRow(column(width = 6,box(title = "Contacts",width = NULL,collapsible = TRUE,collapsed = TRUE,
                                                    lapply(X = 1:length(aptReactResponse[row, "listing.advertiser.contacts"][[1]]$name), 
                                                           FUN = function(k){
                                                      userBlock(
                                                        title = aptReactResponse[row, "listing.advertiser.contacts"][[1]]$name[k],
                                                        image = aptReactResponse[row, "listing.advertiser.contacts"][[1]]$photoUrl[k]
                                                      ) 
                                                    }) 
                                                )),
                                     #Adding a to do list for facilities using lapply for each element in facilities list
                                         column(width = 6,
                                                box(title = "Facilities",width = NULL,collapsible = TRUE,collapsed = TRUE,
                                                    todoList(sortable = FALSE,
                                                             lapply(X = 1:length(aptReactResponse[row, "listing.propertyDetails.features"][[1]]), FUN = function(j){
                                                               todoListItem(label = aptReactResponse[row, "listing.propertyDetails.features"][[1]][j]) 
                                                             }) 
                                                    ) 
                                                ))
                                       )
        ))), # Main Fluid Row End
        # Adding the discover features button to navigate to different tabs with the selected house
        fluidRow(column(width = 4, HTML(paste0('<center><b>Discover features !!!</b></center>'))),
                 column(width = 8,
                        actionGroupButtons(
                          inputIds = c(paste0('switch',row), paste0('tsptswitch',row), paste0('envswitch',row)),
                          labels = list("Places of Interest", "Transportation", "Environment"),
                          status = "default"
                        ))),
        footer = HTML(paste0('<a href="',"https://www.domain.com.au/",aptReactResponse[row, "listing.listingSlug"],'"target="_blank">Book Apartment here!</a>'))
      ) # End Social Box
    } # End For Loop
  }) # End Observe
  
  ##########################################################################################
  
  # Render the dynamically created listing social boxes in the apartment hunting tab.
  output$boxes <- renderUI({
    lapply(X = 1:length(rvs$aptRows), FUN = function(j){
      if(j %% 2 != 0) {
        fluidRow(
          column(width = 6,solidHeader = FALSE,height = 400,rvs$aptRows[[j]]),
          column(width = 6,solidHeader = FALSE,height = 400,rvs$aptRows[[j+1]])
        )
      }
    })
  })
  
  ##########################################################################################
  
  # When user clicks on save button for any element on the screen. 
  # Combined so that we can use it for single observe event.
  toListenStarClicks <- reactive({
    list(input$star1,input$star2,input$star3,input$star4,input$star5,input$star6)
  })
  
  ##########################################################################################
  
  # Observing the save button clicks on apartment hunting tab
  observeEvent(ignoreInit = TRUE, toListenStarClicks(), {
    # Use this to initialize the start when no button is clicked.
    if(input$star1==0 && input$star2==0 && input$star3==0 && input$star4==0 && input$star5==0 && input$star6==0) {
      return()
    }
    
    # Creating the index variable
    index <- NULL
    
    # If save button for first listing is clicked
    if(input$star1==1) {
      # If the listing is already present then remove it from the list and update button
      if(list.any(rvs$starHouses, reactResponse()[1,"listing.id"] %in% listing.id)) {
        index <- list.which(rvs$starHouses, reactResponse()[1,"listing.id"] %in% listing.id)
        rvs$starHouses <- list.remove(rvs$starHouses, c(index))
        updateActionButton(inputId = 'star1', label = 'Save', icon = icon("star"))
      } else {
        # If the listing is not present then add to the list and update button to saved.
        rvs$starHouses <- append(rvs$starHouses, list(as.list(reactResponse()[1,])))
        updateActionButton(inputId = 'star1', label = 'Saved', icon = icon("check"))
      }
    }
    
    # If save button for second listing is clicked
    if(input$star2==1) {
      # If the listing is already present then remove it from the list and update button
      if(list.any(rvs$starHouses, reactResponse()[2,"listing.id"] %in% listing.id)) {
        index <- list.which(rvs$starHouses, reactResponse()[2,"listing.id"] %in% listing.id)
        rvs$starHouses <- list.remove(rvs$starHouses, c(index))
        updateActionButton(inputId = 'star2', label = 'Save', icon = icon("star"))
      } else {
        # If the listing is not present then add to the list and update button to saved.
        rvs$starHouses <- append(rvs$starHouses, list(as.list(reactResponse()[2,])))
        updateActionButton(inputId = 'star2', label = 'Saved', icon = icon("check"))
      }
    }
    
    # If save button for third listing is clicked
    if(input$star3==1) {
      # If the listing is already present then remove it from the list and update button
      if(list.any(rvs$starHouses, reactResponse()[3,"listing.id"] %in% listing.id)) {
        index <- list.which(rvs$starHouses, reactResponse()[3,"listing.id"] %in% listing.id)
        rvs$starHouses <- list.remove(rvs$starHouses, c(index))
        updateActionButton(inputId = 'star3', label = 'Save', icon = icon("star"))
      } else {
        # If the listing is not present then add to the list and update button to saved.
        rvs$starHouses <- append(rvs$starHouses, list(as.list(reactResponse()[3,])))
        updateActionButton(inputId = 'star3', label = 'Saved', icon = icon("check"))
      }
    }
    
    # If save button for fourth listing is clicked
    if(input$star4==1) {
      # If the listing is already present then remove it from the list and update button
      if(list.any(rvs$starHouses, reactResponse()[4,"listing.id"] %in% listing.id)) {
        index <- list.which(rvs$starHouses, reactResponse()[4,"listing.id"] %in% listing.id)
        rvs$starHouses <- list.remove(rvs$starHouses, c(index))
        updateActionButton(inputId = 'star4', label = 'Save', icon = icon("star"))
      } else {
        # If the listing is not present then add to the list and update button to saved.
        rvs$starHouses <- append(rvs$starHouses, list(as.list(reactResponse()[4,])))
        updateActionButton(inputId = 'star4', label = 'Saved', icon = icon("check"))
      }
    }
    
    # If save button for fifth listing is clicked
    if(input$star5==1) {
      # If the listing is already present then remove it from the list and update button
      if(list.any(rvs$starHouses, reactResponse()[5,"listing.id"] %in% listing.id)) {
        index <- list.which(rvs$starHouses, reactResponse()[5,"listing.id"] %in% listing.id)
        rvs$starHouses <- list.remove(rvs$starHouses, c(index))
        updateActionButton(inputId = 'star5', label = 'Save', icon = icon("star"))
      } else {
        # If the listing is not present then add to the list and update button to saved.
        rvs$starHouses <- append(rvs$starHouses, list(as.list(reactResponse()[5,])))
        updateActionButton(inputId = 'star5', label = 'Saved', icon = icon("check"))
      }
    }
    
    # If save button for sixth listing is clicked
    if(input$star6==1) {
      # If the listing is already present then remove it from the list and update button
      if(list.any(rvs$starHouses, reactResponse()[6,"listing.id"] %in% listing.id)) {
        index <- list.which(rvs$starHouses, reactResponse()[6,"listing.id"] %in% listing.id)
        rvs$starHouses <- list.remove(rvs$starHouses, c(index))
        updateActionButton(inputId = 'star6', label = 'Save', icon = icon("star"))
      } else {
        # If the listing is not present then add to the list and update button to saved.
        rvs$starHouses <- append(rvs$starHouses, list(as.list(reactResponse()[6,])))
        updateActionButton(inputId = 'star6', label = 'Saved', icon = icon("check"))
      }
    }
    
    # Convert the list of lists back to data frame for plotting on leaflet
    rvs$starDfHouses <- list.stack(rvs$starHouses)
  })
  
  ##########################################################################################
  
  # When a click on the discover places of interest button is clicked for 
  # a listing. Combining for all the listings displayed.
  toListenRentClicks <- reactive({
    list(input$switch1, input$switch2, input$switch3, input$switch4, input$switch5, input$switch6)
  })
  
  ##########################################################################################
  
  # Observing the places of interest clicks on each listing. 
  observeEvent(ignoreInit = TRUE, toListenRentClicks(), {
    
    # Not doing anything when no button is clicked and then returning
    if(input$switch1==0 && input$switch2==0 && input$switch3==0 && input$switch4==0 && input$switch5==0 && input$switch6==0){
      return()
    }
    
    # If places of interest button is clicked for first listing. Assigning the 
    # variables values for that particular listing.
    if(input$switch1==1) {
      rvs$filteredApt <- reactResponse()[1,]
      rvs$filterLat <- reactResponse()[1, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[1, "listing.propertyDetails.longitude"]
    }
    
    # If places of interest button is clicked for second listing. Assigning the 
    # variables values for that particular listing.
    if(input$switch2==1) {
      rvs$filteredApt <- reactResponse()[2,]
      rvs$filterLat <- reactResponse()[2, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[2, "listing.propertyDetails.longitude"]
    }
    
    # If places of interest button is clicked for third listing. Assigning the 
    # variables values for that particular listing.
    if(input$switch3==1) {
      rvs$filteredApt <- reactResponse()[3,]
      rvs$filterLat <- reactResponse()[3, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[3, "listing.propertyDetails.longitude"]
    }
    
    # If places of interest button is clicked for fourth listing. Assigning the 
    # variables values for that particular listing.
    if(input$switch4==1) {
      rvs$filteredApt <- reactResponse()[4,]
      rvs$filterLat <- reactResponse()[4, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[4, "listing.propertyDetails.longitude"]
    }
    
    # If places of interest button is clicked for fifth listing. Assigning the 
    # variables values for that particular listing.
    if(input$switch5==1) {
      rvs$filteredApt <- reactResponse()[5,]
      rvs$filterLat <- reactResponse()[5, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[5, "listing.propertyDetails.longitude"]
    }
    
    # If places of interest button is clicked for sixth listing. Assigning the 
    # variables values for that particular listing.
    if(input$switch6==1) {
      rvs$filteredApt <- reactResponse()[6,]
      rvs$filterLat <- reactResponse()[6, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[6, "listing.propertyDetails.longitude"]
    }
    
    # Switching the tab from apartment hunting to places of interest tab
    newtab <- switch(input$tabs, "searchapt" = "places","places" = "searchapt")
    updateTabItems(session, "tabs", newtab)
    
    # Using leaflet proxy for updating the maps after they have been rendered.
    # Adding the 1km radius around the map with the apartment
    leafletProxy("places", session = session) %>% 
      removeShape("rentSpace") %>% 
      removeMarker('homeMarker') %>% 
      setView(lat = rvs$filterLat, lng = rvs$filterLong, zoom = 15) %>% 
      addCircles(rvs$filterLong, rvs$filterLat, weight = 1, radius = 1000, color =  "black", layerId = "rentSpace") %>% 
      addAwesomeMarkers(data = rvs$filteredApt,
                        lng = ~listing.propertyDetails.longitude,
                        lat = ~listing.propertyDetails.latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = rvs$filteredApt$listing.advertiser.name,
                        layerId = 'homeMarker')
  })
  
  ##########################################################################################
  
  # When a click on the discover transport button is clicked for 
  # a listing. Combining for all the listings displayed.
  toListenTransportClicks <- reactive({
    list(input$tsptswitch1,input$tsptswitch2,input$tsptswitch3,input$tsptswitch4,input$tsptswitch5,input$tsptswitch6)
  })
  
  ##########################################################################################
  
  # Observing the transport discover clicks on each listing.
  observeEvent(ignoreInit = TRUE, toListenTransportClicks(), {
    
    # Not doing anything when no button is clicked and then returning
    if(input$tsptswitch1==0 && input$tsptswitch2==0 && input$tsptswitch3==0 && input$tsptswitch4==0 && input$tsptswitch5==0 && 
       input$tsptswitch6==0) {
      return()
    }
    
    # When the transport discover button is clicked for the first listing. 
    # Saving the values for that listing in reactive variables. 
    if(input$tsptswitch1==1) {
      rvs$filteredApt <- reactResponse()[1,]
      rvs$filterLat <- reactResponse()[1, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[1, "listing.propertyDetails.longitude"]
    }
    
    # When the transport discover button is clicked for the second listing. 
    # Saving the values for that listing in reactive variables. 
    if(input$tsptswitch2==1) {
      rvs$filteredApt <- reactResponse()[2,]
      rvs$filterLat <- reactResponse()[2, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[2, "listing.propertyDetails.longitude"]
    }
    
    # When the transport discover button is clicked for the third listing. 
    # Saving the values for that listing in reactive variables. 
    if(input$tsptswitch3==1) {
      rvs$filteredApt <- reactResponse()[3,]
      rvs$filterLat <- reactResponse()[3, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[3, "listing.propertyDetails.longitude"]
    }
    
    # When the transport discover button is clicked for the fourth listing. 
    # Saving the values for that listing in reactive variables. 
    if(input$tsptswitch4==1) {
      rvs$filteredApt <- reactResponse()[4,]
      rvs$filterLat <- reactResponse()[4, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[4, "listing.propertyDetails.longitude"]
    }
    
    # When the transport discover button is clicked for the fifth listing. 
    # Saving the values for that listing in reactive variables. 
    if(input$tsptswitch5==1) {
      rvs$filteredApt <- reactResponse()[5,]
      rvs$filterLat <- reactResponse()[5, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[5, "listing.propertyDetails.longitude"]
    }
    
    # When the transport discover button is clicked for the sixth listing. 
    # Saving the values for that listing in reactive variables. 
    if(input$tsptswitch6==1) {
      rvs$filteredApt <- reactResponse()[6,]
      rvs$filterLat <- reactResponse()[6, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[6, "listing.propertyDetails.longitude"]
    }
    
    # Switching between the apartment hunting tab and the transport tab.
    newtab <- switch(input$tabs, "searchapt" = "transport","transport" = "searchapt")
    updateTabItems(session, "tabs", newtab)
    
    # Using the leaflet proxy for the transport leaflet map with the selected data 
    leafletProxy("transport", session = session) %>% 
      removeShape("rentSpace") %>% 
      removeMarker('homeMarker') %>% 
      setView(lat = rvs$filterLat, lng = rvs$filterLong, zoom = 15) %>% 
      addCircles(rvs$filterLong, rvs$filterLat, weight = 1, radius = 1000, color =  "black", layerId = "rentSpace") %>% 
      addAwesomeMarkers(data = rvs$filteredApt,
                        lng = ~listing.propertyDetails.longitude,
                        lat = ~listing.propertyDetails.latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = rvs$filteredApt$listing.advertiser.name,
                        layerId = 'homeMarker')
  })
  
  ##########################################################################################
  
  # When a click on the discover environment button is clicked for 
  # a listing. Combining for all the listings displayed.
  toListenEnvClicks <- reactive({
    list(input$envswitch1,input$envswitch2,input$envswitch3,input$envswitch4,input$envswitch5,input$envswitch6)
  })
  
  ##########################################################################################
  
  # Observing the environment discover clicks on each listing.
  observeEvent(ignoreInit = TRUE, toListenEnvClicks(), {
    
    # Not doing anything when no button is clicked and then returning
    if(input$envswitch1==0 && input$envswitch2==0 && input$envswitch3==0 && input$envswitch4==0 && input$envswitch5==0 && input$envswitch6==0) {
      return()
    }
    
    # If environment button is clicked for the first listing.
    # Saving the values for that listing in reactive variables. 
    if(input$envswitch1==1) {
      rvs$filteredApt <- reactResponse()[1,]
      rvs$filterLat <- reactResponse()[1, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[1, "listing.propertyDetails.longitude"]
    }
    
    # If environment button is clicked for the second listing.
    # Saving the values for that listing in reactive variables. 
    if(input$envswitch2==1) {
      rvs$filteredApt <- reactResponse()[2,]
      rvs$filterLat <- reactResponse()[2, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[2, "listing.propertyDetails.longitude"]
    }
    
    # If environment button is clicked for the third listing.
    # Saving the values for that listing in reactive variables. 
    if(input$envswitch3==1) {
      rvs$filteredApt <- reactResponse()[3,]
      rvs$filterLat <- reactResponse()[3, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[3, "listing.propertyDetails.longitude"]
    }
    
    # If environment button is clicked for the fourth listing.
    # Saving the values for that listing in reactive variables. 
    if(input$envswitch4==1) {
      rvs$filteredApt <- reactResponse()[4,]
      rvs$filterLat <- reactResponse()[4, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[4, "listing.propertyDetails.longitude"]
    }
    
    # If environment button is clicked for the fifth listing.
    # Saving the values for that listing in reactive variables. 
    if(input$envswitch5==1) {
      rvs$filteredApt <- reactResponse()[5,]
      rvs$filterLat <- reactResponse()[5, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[5, "listing.propertyDetails.longitude"]
    }
    
    # If environment button is clicked for the sixth listing.
    # Saving the values for that listing in reactive variables. 
    if(input$envswitch6==1) {
      rvs$filteredApt <- reactResponse()[6,]
      rvs$filterLat <- reactResponse()[6, "listing.propertyDetails.latitude"]
      rvs$filterLong <- reactResponse()[6, "listing.propertyDetails.longitude"]
    }
    
    # Switching between the apartment hunting tab and environment tab
    newtab <- switch(input$tabs, "searchapt" = "environment","environment" = "searchapt")
    updateTabItems(session, "tabs", newtab)
    
    # Updating the environment leaflet map using leaflet proxy to add info.
    leafletProxy("environment", session = session) %>% 
      removeShape("rentSpace") %>% 
      removeMarker('homeMarker') %>% 
      setView(lat = rvs$filterLat, lng = rvs$filterLong, zoom = 15) %>% 
      addCircles(rvs$filterLong, rvs$filterLat, weight = 1, radius = 1000, color =  "black", layerId = "rentSpace") %>% 
      addAwesomeMarkers(data = rvs$filteredApt,
                        lng = ~listing.propertyDetails.longitude,
                        lat = ~listing.propertyDetails.latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = rvs$filteredApt$listing.advertiser.name,
                        layerId = 'homeMarker')
  })
  
  ##########################################################################################

  # Observing the show location button and updating the map box to restore.
  observeEvent(input$show_map, {
    updateBox("locationmap", action = "restore")
  })
  
  ##########################################################################################
  
  # Observing the hide location button and remove the box from the view.
  observeEvent(input$hide_map, {
    updateBox("locationmap", action = "remove")
  })
  
  ##########################################################################################
  
  # Adding the leaflet map for displaying houses in the current display.
  output$rentMap <- renderLeaflet({
    aptReactResponse <- reactResponse()
    leaflet() %>% 
      addProviderTiles("CartoDB.Voyager") %>% 
      addAwesomeMarkers(data = aptReactResponse,
                        lng = ~listing.propertyDetails.longitude,
                        lat = ~listing.propertyDetails.latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = aptReactResponse$listing.advertiser.name,
                        popup = paste0('<center><b>',aptReactResponse$listing.headline,'</b></center>',
                                       '<hr>',
                                       '<b>Price:</b> ',aptReactResponse$listing.priceDetails.displayPrice,'<br>',
                                       '<b>Address:</b> ',aptReactResponse$listing.propertyDetails.displayableAddress, '<br>',
                                       '<b>Number of bedrooms:</b> ',aptReactResponse$listing.propertyDetails.bedrooms,'<br>',
                                       '<b>Number of bathrooms:</b> ',aptReactResponse$listing.propertyDetails.bathrooms,'<br>',
                                       '<b>Number of carspaces:</b> ',aptReactResponse$listing.propertyDetails.carspaces, '<br>',
                                       '<b>Date Available:</b> ',aptReactResponse$listing.dateAvailable, '<br>'))
  })
  
  ##########################################################################################
  
  # Observing the show saved houses on the transport map button
  observeEvent(input$transportSMap, {
    if(list.count(rvs$starHouses) == 0) {
      # Show notification on the screen.
      showNotification("No saved houses to display on the map", type = "warning", duration = 4, id='transportNotification')
      return()
    }
    
    # Getting the data for the currently selected saved houses.
    # Adding the markers for saved houses and removing the previous ones.
    dfData <- rvs$starDfHouses
    leafletProxy("transport", session = session) %>%
      clearGroup('savedHouses') %>% 
      addAwesomeMarkers(data = dfData,
                        lng = dfData$listing.propertyDetails.longitude,
                        lat = dfData$listing.propertyDetails.latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = dfData$listing.advertiser.name,
                        popup = paste0('<center><b>',dfData$listing.headline,'</b></center>',
                                       '<hr>',
                                       '<b>Price:</b> ',dfData$listing.priceDetails.displayPrice,'<br>',
                                       '<b>Address:</b> ',dfData$listing.propertyDetails.displayableAddress, '<br>',
                                       '<b>Number of bedrooms:</b> ',dfData$listing.propertyDetails.bedrooms,'<br>',
                                       '<b>Number of bathrooms:</b> ',dfData$listing.propertyDetails.bathrooms,'<br>',
                                       '<b>Number of carspaces:</b> ',dfData$listing.propertyDetails.carspaces, '<br>',
                                       '<b>Date Available:</b> ',dfData$listing.dateAvailable, '<br>'),
                        group = 'savedHouses')
  })
  
  ##########################################################################################
  
  # Observing the hide saved houses from the transport map button
  observeEvent(input$transportHMap, {
    # Clearing the group from the transport map
    leafletProxy("transport", session = session) %>% 
      clearGroup('savedHouses')
  })
  
  ##########################################################################################
  
  # Rendering the transport leaflet map with different layers groups.
output$transport = 
    map = renderLeaflet({
    # Load PTV data and create tooltips/labels
    path = paste(home, "/data/Transport/ptv_combined_city_of_melbourne.csv", sep="")
    data_transport_PTV = read.csv(path, stringsAsFactors = FALSE)
    data_transport_PTV$Mode = paste(substring(data_transport_PTV$Mode,1,1), tolower(substring(data_transport_PTV$Mode,2)), sep="")
    data_transport_PTV$tooltip = paste("<b>", data_transport_PTV$Mode,"</b>", " <b>Stop: </b>", data_transport_PTV$STOP_ID, '<br>',
                                      "<b>Stop name:</b> ", data_transport_PTV$STOP_NAME,'<br>',
                                      "<b>Routes using stop:</b> ", data_transport_PTV$ROUTES_USING_STOP, '<br>',
                                      "<b>Zone:</b> ", data_transport_PTV$TICKETZONE
                                       )
    data_transport_PTV$label = paste(data_transport_PTV$Mode, " Stop: ", data_transport_PTV$STOP_NAME
                                    )
    
    # Load street parking
    path = paste(home, "/data/Transport/street_parking.csv", sep="")
    data_street_parking = read.csv(path, stringsAsFactors = FALSE)
    
    
    ## Tram Routes
    # Reading the tram routes file
    tramRoutes <- readOGR(dsn = paste0(home,"/data/Transport/Tram_Routes/ll_gda2020/esrishape/whole_of_dataset/victoria/PTV"), "PTV_METRO_TRAM_ROUTE")
    
    # Creating factors for each tram route number
    tramRouteNumbers <- c("1", "3/3a", "5", "6", "11", "12", "16", "19", "30", "35", "48", "57", "58", "59", "64", "67", "70", "72", "75", "78", "82", "86", "96", "109")
    routeFactor <- factor(tramRouteNumbers, levels = tramRouteNumbers)
    
    # Assigning tram route colors based on real visual identity from PTV
    customPal <- colorFactor(palette = c("#B5BD00", "#8DC8E8", "#D50032", "#01426A", "#6ECEB2", "#007E92", "#FBD872", "#8A1B61", "#534F96", "#6B3529", "#333434", "#00C1D5", "#969696", "#00653A", "#00AB8E", "#956C58", "#F59BBB", "#9ABEAA", "#00A9E0", "#A0A0D6", "#D2D755", "#FFB500", "#C6007E", "#E87722"), levels = routeFactor)
    
    ## Bus Routes
    # Reading the bus routes shapefile
    busRoutes <- readOGR(dsn = paste0(home,"/data/Transport/Bus_Routes/ll_gda2020/esrishape/whole_of_dataset/victoria/PTV"),"PTV_METRO_BUS_ROUTE")
    # Removing the routes not used.
    busRoutes <- subset(busRoutes, !grepl('TeleBus', busRoutes$ROUTESHTNM))
    # Changing route numbers as integers for sorting
    busRoutes$ROUTESHTNM <- as.integer(busRoutes$ROUTESHTNM)
    # Reading as simple features file
    busSf <- st_as_sf(busRoutes)
    # Adding the ranges for bus routes
    busSf <- busSf %>% mutate(Range = case_when(ROUTESHTNM <= 199 ~ "100-199",
                                                ROUTESHTNM >= 200 & ROUTESHTNM <=299 ~ "200-299",
                                                ROUTESHTNM >= 300 & ROUTESHTNM <=399 ~ "300-399",
                                                ROUTESHTNM >= 400 & ROUTESHTNM <=499 ~ "400-499",
                                                ROUTESHTNM >= 500 & ROUTESHTNM <=599 ~ "500-599",
                                                ROUTESHTNM >= 600 & ROUTESHTNM <=699 ~ "600-699",
                                                ROUTESHTNM >= 700 & ROUTESHTNM <=799 ~ "700-799",
                                                ROUTESHTNM >= 800 & ROUTESHTNM <=899 ~ "800-899",
                                                TRUE ~ "900-999"))
    # Creating factors for ranges 
    busRouteFactor <- factor(busSf$Range, levels = c("100-199","200-299","300-399","400-499","500-599",
                                                     "600-699","700-799","800-899","900-999"))
    
    # Assigning colors from PTV visual toolkit to each range
    busPal <- colorFactor(palette = c("#F47920","#204394","#0CAE5D","#D92B26","#00B9F2","#046737","#F1B80E","#8B5FA7","#7EC2BA"), levels = c("100-199","200-299","300-399","400-499","500-599","600-699","700-799","800-899","900-999"))
    
    # Creating the transport map
    leaflet() %>% 
      setView(lng = 144.946457, lat = -37.840935, zoom = 12) %>% 
      addProviderTiles(providers$CartoDB) %>%
      
      # Add custom markers for tram stops and cluster options
      addAwesomeMarkers(data = filter(data_transport_PTV, Mode=="Tram"), lng=~LONGITUDE, lat=~LATITUDE,
                        icon=~awesomeIcons(icon=case_when(Mode == 'Tram' ~ 'fa-regular fa-train-tram'),
                                           library='fa',
                                           markerColor=case_when(Mode == 'Tram' ~ 'green'),
                                           iconColor='#fff'),
                        label=~label,
                        popup= ~tooltip,  
                        layerId=~STOP_ID,
                        clusterOptions = markerClusterOptions(maxClusterRadius=15), 
                        group = "Tram Stops") %>%
      
      # Add custom markers for bus stops and cluster options
      addAwesomeMarkers(data = filter(data_transport_PTV, Mode=="Bus"), lng=~LONGITUDE, lat=~LATITUDE,
                        icon=~awesomeIcons(icon=case_when(Mode == 'Bus' ~ 'fa-solid fa-bus'),
                        library='fa',
                        markerColor=case_when(Mode == 'Bus' ~ 'orange'),
                        iconColor='#fff'),
                        label=~label,
                        popup= ~tooltip,  
                        layerId=~STOP_ID,
                        clusterOptions = markerClusterOptions(maxClusterRadius=15), 
                        group = "Bus Stops") %>%
      
      # Add custom markers for train stops and cluster options
      addAwesomeMarkers(data = filter(data_transport_PTV, Mode=="Train"), lng=~LONGITUDE, lat=~LATITUDE,
                        icon=~awesomeIcons(icon=case_when(Mode == 'Train' ~ 'fa-solid fa-train'),
                        library='fa',
                        markerColor=case_when(Mode == 'Train' ~ 'blue'), #0072CE
                        iconColor='#fff'),
                        label=~label,
                        popup= ~tooltip,  
                        layerId=~STOP_ID,
                        clusterOptions = markerClusterOptions(maxClusterRadius=15), 
                        group = "Train Stops") %>%
      
      # Add circles for parking places
      addCircles(data = data_street_parking, lng=~longitude, lat=~latitude,
                        radius = 10,
                        stroke = FALSE,
                        color = "#4876FF",
                        fillOpacity = 0.8,
                        group = "Street Parking") %>%
      
      # Adding the polyline for tram routes with popups and custom label
      addPolylines(data = tramRoutes,weight = 3,color = ~customPal(ROUTESHTNM),fill = FALSE,stroke = TRUE,label = ~ROUTESHTNM,
                   labelOptions = labelOptions(interactive = TRUE, opacity = 1, 
                                               style = c("color" = "black","box-shadow" = "3px 3px rgba(0,0,0,0.25)", 
                                                         "border-color" = "rgba(0,0,0,0.5)", "font-size" = "12px")),
                   popup = paste0('<center><b>',tramRoutes$OPERATOR, '</b></center>',
                                  '<hr>',
                                  '<b>Route:</b> ', tramRoutes$ROUTESHTNM,' (',tramRoutes$TRIPHDSIGN,')', '<br>',
                                  '<b>First Stop:</b> ', tramRoutes$FIRSTSTNAM, '<br>',
                                  '<b>Last Stop:</b> ', tramRoutes$LASTSPNAME, '<br>',
                                  '<b>Number of Stops:</b> ', tramRoutes$NUMOFSTOPS, '<br>'),
                   highlight = highlightOptions(weight = 10,fillOpacity = 0.2,bringToFront = TRUE),
                   group = "Tram Routes",
                   layerId = paste0('TRAMROUTE-',tramRoutes$SHAPE_ID,'(',tramRoutes$ROUTESHTNM,')')
                   ) %>%
      
      # Adding legend for tram routes.
      addLegend(position = "bottomleft", pal = customPal, values = routeFactor, 
                title = "Tram Routes", opacity = 1, group = "Tram Routes") %>% 
      
      # Adding the polylines for bus routes with popups and labels.
      addPolylines(data = busSf,
                   color = ~busPal(busSf$Range),
                   weight = 2,
                   fill = FALSE,
                   stroke = TRUE,
                   label = ~ROUTESHTNM,
                   labelOptions = labelOptions(interactive = TRUE, opacity = 1,style = c("color" = "black","box-shadow" = "3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)", "font-size" = "12px")),
                   popup = paste0('<center><b>',busSf$OPERATOR, '</b></center>',
                                  '<hr>',
                                  '<b>Route:</b> ', busSf$ROUTESHTNM,' (',busSf$ROUTELONGN,')', '<br>',
                                  '<b>First Stop:</b> ', busSf$FIRSTSTNAM, '<br>',
                                  '<b>Last Stop:</b> ', busSf$LASTSPNAME, '<br>',
                                  '<b>Number of Stops:</b> ', busSf$NUMOFSTOPS, '<br>'),
                   highlight = highlightOptions(weight = 10,
                                                fillOpacity = 0.2,
                                                bringToFront = TRUE),
                   group = "Bus Routes") %>% 
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      
      # Custom legend position and values for bus routes
      addLegend(position = "bottomleft", pal = busPal, values = busRouteFactor, title = "Bus Routes", opacity = 1, group = "Bus Routes") %>% 
      
      # Create layers for each data-set
      addLayersControl(overlayGroups = c("Tram Routes","Tram Stops","Bus Stops", "Bus Routes", "Train Stops", "Street Parking"),position = "topright",
                       options = layersControlOptions(collapsed = FALSE)) %>% 
     
      # Hide initial layers to not clutter map
      hideGroup("Tram Stops") %>%
      hideGroup("Bus Stops") %>%
      hideGroup("Bus Routes") %>% 
      hideGroup("Train Stops") %>%
      hideGroup("Street Parking")
  })
  map
  
  ##########################################################################################
  
  # Observing the show saved houses on environment tab button
  observeEvent(input$envSMap, {
    # Not doing anything if the saved houses are none 
    if(list.count(rvs$starHouses) == 0) {
      # Show notification on the screen.
      showNotification("No saved houses to display on the map", type = "warning", duration = 4, id='envNotification')
      return()
    }
    
    # Getting the saved houses
    dfData <- rvs$starDfHouses
    
    # Displaying the saved houses on the environment tab using leaflet proxy
    leafletProxy("environment", session = session) %>%
      clearGroup('savedHouses') %>% 
      addAwesomeMarkers(data = dfData,
                        lng = dfData$listing.propertyDetails.longitude,
                        lat = dfData$listing.propertyDetails.latitude,
                        icon = ~awesomeIcons("fas fa-home",
                                             library = "fa",
                                             iconColor = "black",
                                             markerColor = "red"),
                        label = dfData$listing.advertiser.name,
                        popup = paste0('<center><b>',dfData$listing.headline,'</b></center>',
                                       '<hr>',
                                       '<b>Price:</b> ',dfData$listing.priceDetails.displayPrice,'<br>',
                                       '<b>Address:</b> ',dfData$listing.propertyDetails.displayableAddress, '<br>',
                                       '<b>Number of bedrooms:</b> ',dfData$listing.propertyDetails.bedrooms,'<br>',
                                       '<b>Number of bathrooms:</b> ',dfData$listing.propertyDetails.bathrooms,'<br>',
                                       '<b>Number of carspaces:</b> ',dfData$listing.propertyDetails.carspaces, '<br>',
                                       '<b>Date Available:</b> ',dfData$listing.dateAvailable, '<br>'),
                        group = 'savedHouses')
  })
  
  ##########################################################################################
  
  # Observing the remove saved houses from the environment map
  observeEvent(input$envHMap, {
    # Clearing the saved houses group from the map
    leafletProxy("environment", session = session) %>% 
      clearGroup('savedHouses')
  })
  
  ##########################################################################################
  
  # Rendering the environment tab
output$environment = 
  map = renderLeaflet({
  
  # Load sensor data
  path = paste(home, "/data/Environment/Microclimate_Sensor_Readings_mean.csv", sep="")
  data_environment_sensor = read.csv(path, stringsAsFactors = FALSE)
  data_environment_sensor$popup = paste("<b>Sensor Readings:</b> ", '<br>', 
                                        "<b>Mean temperature:</b> ", data_environment_sensor$temp_mean, '<br>', 
                                        "<b>Mean wind speed:</b> ", data_environment_sensor$ws_mean, '<br>',
                                        "<b>Mean humidity:</b> ", data_environment_sensor$humidity_mean
                                        )

  # Load urban forest data and create custom tooltips and labels
  path = paste(home, "/data/Environment/Trees__with_species_and_dimensions__Urban_Forest_.csv", sep="")
  data_environment_urban_forest = read.csv(path, stringsAsFactors = FALSE)
  data_environment_urban_forest$popup = paste("<b>Common Name:</b> ", data_environment_urban_forest$Common.Name,'<br>', 
                                              "<b>Scientific Name:</b> ", data_environment_urban_forest$Scientific.Name, '<br>',
                                              "<b>Year planted:</b> ", data_environment_urban_forest$Year.Planted)
  data_environment_urban_forest$label = paste("Tree: ", data_environment_urban_forest$Common.Name)
  
  tree_icon_path = paste(home,"/data/icons/tree_evergreen_icon.png", sep="")
  
  set.seed(42)
  subset_indcs = sample.int(dim(data_environment_urban_forest)[1], dim(data_environment_urban_forest)[1]/2) #half the data
  data_environment_urban_forest = data_environment_urban_forest[subset_indcs, ]
  

  leaflet() %>% 
    
    addProviderTiles(providers$CartoDB) %>%
    # Circles for sensor Temperature Data
    addCircles(data=data_environment_sensor, 
               lng=~longitude, lat=~latitude, 
               radius=80, stroke = FALSE, 
               fillOpacity = 0.8,
               popup = ~popup,
               group = "Sensor readings") %>%
    
    # Circles for each tree in urban forest
    addCircles(data = data_environment_urban_forest, lng=~Longitude, lat=~Latitude,
               radius = 15,
               stroke = FALSE,
               popup = ~popup,
               label = ~label,
               color = "#00FF00",
               group = "Urban Forest") %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    
    # Layer control
    addLayersControl(
      baseGroups = c("Urban Forest", "Sensor readings"),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    )
  })
map

##########################################################################################

# Observing the show saved houses on the places of interest tab
observeEvent(input$placesSMap, {
  if(list.count(rvs$starHouses) == 0) {
    # Show notification on the screen.
    showNotification("No saved houses to display on the map", type = "warning", duration = 4, id='placesNotification')
    return()
  }
  
  # Displaying the saved houses on the places leaflet map using leaflet proxy
  dfData <- rvs$starDfHouses
  leafletProxy("places", session = session) %>%
    clearGroup('savedHouses') %>% 
    addAwesomeMarkers(data = dfData,
                      lng = dfData$listing.propertyDetails.longitude,
                      lat = dfData$listing.propertyDetails.latitude,
                      icon = ~awesomeIcons("fas fa-home",
                                           library = "fa",
                                           iconColor = "black",
                                           markerColor = "red"),
                      label = dfData$listing.advertiser.name,
                      popup = paste0('<center><b>',dfData$listing.headline,'</b></center>',
                                     '<hr>',
                                     '<b>Price:</b> ',dfData$listing.priceDetails.displayPrice,'<br>',
                                     '<b>Address:</b> ',dfData$listing.propertyDetails.displayableAddress, '<br>',
                                     '<b>Number of bedrooms:</b> ',dfData$listing.propertyDetails.bedrooms,'<br>',
                                     '<b>Number of bathrooms:</b> ',dfData$listing.propertyDetails.bathrooms,'<br>',
                                     '<b>Number of carspaces:</b> ',dfData$listing.propertyDetails.carspaces, '<br>',
                                     '<b>Date Available:</b> ',dfData$listing.dateAvailable, '<br>'),
                      group = 'savedHouses')
})

##########################################################################################

# Observing the hide saved houses from the places of interest tab
observeEvent(input$placesHMap, {
  # Clearing the saved houses group from the map
  leafletProxy("places", session = session) %>% 
    clearGroup('savedHouses')
})

##########################################################################################

# Places of interest
output$places =
  map = renderLeaflet({

    path = paste(home, "/data/Places_Of_Interest/Coworking_Spaces.csv", sep="")
    data_cowork = read.csv(path, stringsAsFactors = FALSE)
    data_cowork$popup = paste("<h4><b><u><a href='", data_cowork$Website, "'>", data_cowork$Organisation, "</a></u></b></h4>",
                              data_cowork$Address, '<br><br>&#x1F50D;<i> coworking space</i>')
    data_cowork$label = paste("Organisation: ", data_cowork$Organisation)

    # Loading and removing duplicates from Places of Interest data
    poi_categories = c("Cafe", "Food & Drinks", "Gym", "Health", "Places of Worship", "Shops & Supermarkets", "Tourism & Entertainment")
    poi_cafe = fromJSON('data/Points_of_interest/categorised_data/Cafe.json')
    poi_cafe <- poi_cafe[!duplicated(poi_cafe$place_id), ] # Remove duplicates
    poi_food = fromJSON('data/Points_of_interest/categorised_data/Food & Drinks.json')
    poi_food <- poi_food[!duplicated(poi_food$place_id), ] # Remove duplicates
    poi_gym = fromJSON('data/Points_of_interest/categorised_data/Gym.json')
    poi_gym <- poi_gym[!duplicated(poi_gym$place_id), ] # Remove duplicates
    poi_health = fromJSON('data/Points_of_interest/categorised_data/Health.json')
    poi_health <- poi_health[!duplicated(poi_health$place_id), ] # Remove duplicates
    poi_shops = fromJSON('data/Points_of_interest/categorised_data/Shops & Supermarkets.json')
    poi_shops <- poi_shops[!duplicated(poi_shops$place_id), ] # Remove duplicates
    poi_tourism = fromJSON('data/Points_of_interest/categorised_data/Tourism & Entertainment.json')
    poi_tourism <- poi_tourism[!duplicated(poi_tourism$place_id), ] # Remove duplicates
    poi_rating_labels = c("Poor", "Average", "High") # Used for scale 
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB) %>%
  
      # co-working spaces
      addMarkers(data = data_cowork, lng=~longitude, lat=~latitude,
                 clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     spiderifyOnMaxZoom=FALSE,
                                                     disableClusteringAtZoom=20,
                                                     maxClusterRadius=30),
               popup = ~popup,
               label = ~label,
               icon = makeIcon(iconUrl=~"img/work.png", iconWidth = 25, iconHeight = 25),
               group = "Cowork spaces") %>%             

      # Cafe
      addMarkers(data=poi_cafe, lng=~long, lat=~lat, popup=~popup, group="Cafe", 
             label = paste0(poi_cafe$name, case_when(!is.na(poi_cafe$rating) ~ paste0(' (', poi_cafe$rating, "★)"), TRUE ~ '')),
             icon=makeIcon(iconUrl=~iconpath, iconWidth = 20, iconHeight = 20),
             clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                   spiderifyOnMaxZoom=FALSE,
                                                   disableClusteringAtZoom=20,
                                                   maxClusterRadius=30)) %>%
    
      # Food & Drink
      addMarkers(data=poi_food, lng=~long, lat=~lat, popup=~popup, group="Food & Drink",
              label = paste0(poi_cafe$name, case_when(!is.na(poi_cafe$rating) ~ paste0(' (', poi_cafe$rating, "★)"), TRUE ~ '')),
              icon=makeIcon(iconUrl=~iconpath, iconWidth = 20, iconHeight = 20),
              clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     spiderifyOnMaxZoom=FALSE,
                                                     disableClusteringAtZoom=20,
                                                     maxClusterRadius=30)) %>%
      # Gym
      addMarkers(data=poi_gym, lng=~long, lat=~lat, popup=~popup, group="Gym",
               label = paste0(poi_gym$name, case_when(!is.na(poi_gym$rating) ~ paste0(' (', poi_gym$rating, "★)"), TRUE ~ '')),
               icon=makeIcon(iconUrl=~iconpath, iconWidth = 20, iconHeight = 20),
               clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     spiderifyOnMaxZoom=FALSE,
                                                     disableClusteringAtZoom=20,
                                                     maxClusterRadius=30)) %>%
      # Health
      addMarkers(data=poi_health, lng=~long, lat=~lat, popup=~popup, group="Health",
               label = paste0(poi_health$name, case_when(!is.na(poi_health$rating) ~ paste0(' (', poi_health$rating, "★)"), TRUE ~ '')),
               icon=makeIcon(iconUrl=~iconpath, iconWidth = 20, iconHeight = 20),
               clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     spiderifyOnMaxZoom=FALSE,
                                                     disableClusteringAtZoom=20,
                                                     maxClusterRadius=30)) %>%
      # Shops & Supermarkets
      addMarkers(data=poi_shops, lng=~long, lat=~lat, popup=~popup, group="Shops & Supermarkets",
               label = paste0(poi_shops$name, case_when(!is.na(poi_shops$rating) ~ paste0(' (', poi_shops$rating, "★)"), TRUE ~ '')),
               icon=makeIcon(iconUrl=~iconpath, iconWidth = 20, iconHeight = 20),
               clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                     spiderifyOnMaxZoom=FALSE,
                                                     disableClusteringAtZoom=20,
                                                     maxClusterRadius=30)) %>%
      # Tourism & Entertainment
      addMarkers(data=poi_tourism, lng=~long, lat=~lat, popup=~popup, group="Tourism & Entertainment",
               label = paste0(poi_tourism$name, case_when(!is.na(poi_tourism$rating) ~ paste0(' (', poi_tourism$rating, "★)"), TRUE ~ '')),
              icon=makeIcon(iconUrl=~iconpath, iconWidth = 20, iconHeight = 20),
              clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                    spiderifyOnMaxZoom=FALSE,
                                                    disableClusteringAtZoom=20,
                                                    maxClusterRadius=30)) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      # Add legend for rating
      # addLegend(position = "bottomright", pal = colorFactor(palette = c("#D3212C", "#FF980E", "#006B3D"), domain=poi_rating_labels, ordered=TRUE), values=poi_rating_labels,
      #           title = "Ratings", opacity = 1 , group = "Cafe") %>%
    
      # Layers arrangement
      addLayersControl(
        overlayGroups = c("Cafe", "Food & Drink", "Shops & Supermarkets", "Gym", "Health", "Tourism & Entertainment", "Cowork spaces"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
map

##########################################################################################

# Adding the home page HTML as Iframe
output$homePage <- renderUI({
  tags$iframe(seamless="seamless", 
              src= "home.html",
              style='width:80vw;height:100vh;')
              # width=100, 
              # height=1000)
})

##########################################################################################
  
# Rendering all the maps even when the tab is hidden so that leaflet proxy can work.
outputOptions(output, "places", suspendWhenHidden = FALSE)
outputOptions(output, "rentMap", suspendWhenHidden = FALSE)
outputOptions(output, "boxes", suspendWhenHidden = FALSE)
outputOptions(output, "environment", suspendWhenHidden = FALSE)
outputOptions(output, "transport", suspendWhenHidden = FALSE)

}



#############
# RUN SHINY #
#############

shinyApp(ui, server, options=list(launch.browser=TRUE))
