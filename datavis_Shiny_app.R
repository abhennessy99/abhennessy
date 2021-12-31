# Shiny Combination
# libraries and data cleaning ----
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(shiny)
library(leaflet)
library(leafpop)
library(gstat)
library(geogrid)
library(magrittr)
library(purrr)
library(patchwork)
library(tidyr)
library(htmltools)
library(plotly)
library(scales)
library(shinythemes)
library(rsconnect)

bug <- import("C:/Users/abhen/OneDrive - wesleyan.edu/AdvancedGIS/skill_share/example data/Data Vis Final Project/dataRaw/NorwayButterfliesandBees.csv")
bug %>% dplyr::select(order,family,genus,species,scientificName,countryCode,locality,
                      stateProvince,occurrenceStatus,individualCount,decimalLatitude,
                      decimalLongitude,coordinateUncertaintyInMeters,day,month,year
) -> df.raw
df.raw %>% filter(year>2012) -> df

`%!in%` = Negate(`%in%`)
df <- filter(df,occurrenceStatus=="PRESENT",
             year>2012,
             family %!in% c("Zygaenidae","Papilionidae"))

# cleaning Bee NAs
df[, 4][is.na(df[, 4])] <- "Bombus sp."

df <- df %>% 
  mutate(month=factor(month,levels=c(4,5,6,7,8,9),labels=c("April","May","June","July","August","September")),
         family = factor(family,levels=c("Apidae","Nymphalidae","Pieridae",
                                         "Lycaenidae","Hesperiidae"),
                         ordered = T))

# turning data into point objects
st_as_sf(df,coords=c("decimalLongitude","decimalLatitude"),
         crs=4273) -> pt
# creating absence points WIP ----
df.raw %>% filter(year>2012,family %!in% c("Zygaenidae","Papilionidae")) %>% 
  group_by(locality,year,decimalLatitude,decimalLongitude) %>% 
  summarize(individualCount=sum(individualCount)) %>% filter(individualCount==0) %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),
           crs=4273) -> pt.absence

# combining absence and presence transect points
df %>%  
  group_by(family,decimalLatitude,decimalLongitude) %>% 
  summarise(individualCount = sum(individualCount),
            locality=locality,
            year=year) %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),
           crs=4273) -> pres
pt.absence %>% mutate(family="Absent") -> abs

# getting norway basemap
world <- map_data("world")
world %>% 
  filter(region=="Norway") -> norway


# filter for southern norway
norway_filter <- norway %>% 
  filter(lat<65)





# Sampling Intensity ----
bug$eventDate <- as.factor(bug$eventDate)

bug %>% group_by(eventDate,locality) %>% count() -> x

x %>% 
  mutate(eventDate=as.character(eventDate),
         eventDate=str_sub(eventDate,start=1,end=4),
         eventDate=as.factor(eventDate)) -> x
x %>% 
  group_by(eventDate) %>% 
  count()-> y

ggplot(y)+
  geom_col(aes(x=eventDate,y=n))+
  labs(x="Year",y="Sampling Events")

bug %>% group_by(family) %>% summarize(sum=sum(individualCount)) -> z

ggplot(z)+
  geom_col(aes(x=reorder(family,desc(sum)),y=sum))+
  labs(x="Insect Family",y="Total Insect Count")

# Vis 1 ----
# Bar Graph and Line Graph showing time series of pollinators in Norway
df %>%  
  group_by(year,family) %>% 
  summarise(sum=sum(individualCount)) -> yr_fam 

ts1 <- ggplot(yr_fam)+
  geom_col(aes(x=year,y=sum,fill=family))+
  scale_fill_brewer("Insect Family",palette="Set1")+
  labs(x="Year",
       y="Insect Count",
       title="Total Sampled Pollinators Over Time")+
  scale_x_continuous(breaks=unique(yr_fam$year))+
  scale_y_continuous(minor_breaks = seq(500,5000,1000),
                     breaks=seq(0,6000,1000))+
  theme_bw()

ts2 <- ggplot(yr_fam)+
  geom_line(aes(x=year,y=sum,color=family),size=1.25)+
  scale_color_brewer("Insect Family",palette="Set1")+
  labs(x="Year",
       y="Insect Count",
       title="Pollinator Family Trends Over Time")+
  scale_x_continuous(breaks=unique(yr_fam$year))+
  scale_y_continuous(breaks=seq(0,3000,500))+
  theme_bw()

ts1
ts2
# Should I patchwork them together?
# Do the points help on the line graph?


# Vis 2 ----
# GeoFacet and Shiny app to compare time series of pollinators by state  

# average lat and lon for each state
df %>% group_by(stateProvince) %>% 
  summarize(lat = mean(decimalLatitude),
            lon = mean(decimalLongitude)) -> state_means
# create sf from averaged points
st_as_sf(state_means,coords=c("lon","lat"),
         crs=4273) -> state_means.sf

# group by year,family,stateProvince and sum insect count
df %>%  
  group_by(year,family,stateProvince) %>% 
  summarise(sum=sum(individualCount)) -> state_yr_fam 


states <- unique(state_yr_fam$stateProvince)

lines = list()
for (i in 1:length(states)) {
  state_yr_fam %>% filter(stateProvince==states[[i]]) %>%     
    ggplot()+geom_line(aes(x=year,y=sum,color=family),size=1.5)+
    labs(title=paste0(states[i]," Pollinator Time Series"),x="Year",
         y="Insect Count")+
    scale_color_brewer("Insect Family",palette="Set1")+
    scale_x_continuous(breaks=unique(yr_fam$year))+
    theme_bw() -> lines[[i]]
}

bars = list()
for (i in 1:length(states)) {
  state_yr_fam %>% filter(stateProvince==states[[i]]) %>%     
    ggplot()+geom_col(aes(x=year,y=sum,fill=family))+
    labs(title=paste0(states[i]," Pollinator Time Series"),x="Year",
         y="Insect Count")+
    scale_fill_brewer("Insect Family",palette="Set1")+
    scale_x_continuous(breaks=unique(yr_fam$year))+
    theme_bw() -> bars[[i]]
}


leaflet() %>% 
  addTiles() %>% 
  addCircles(data=state_means.sf,
             popup=~popupGraph(bars))
leaflet() %>% 
  addTiles() %>% 
  addCircles(data=state_means.sf,
             popup=~popupGraph(lines))


# maybe add radio button to switch between state-level and site-level

# average lat and lon for each site

df %>% group_by(locality) %>% 
  summarize(lat = mean(decimalLatitude),
            lon = mean(decimalLongitude)) -> site_means
# create sf from averaged points
st_as_sf(site_means,coords=c("lon","lat"),
         crs=4273) -> site_means.sf

# group by year,family,stateProvince and sum insect count
df %>%  
  group_by(year,family,locality) %>% 
  summarise(sum=sum(individualCount)) -> site_yr_fam 

sites <- unique(site_yr_fam$locality)

sites.lines = list()
for (i in 1:length(sites)) {
  site_yr_fam %>% filter(locality==sites[[i]]) %>%     
    ggplot()+geom_line(aes(x=year,y=sum,color=family),size=1.5)+
    labs(title=paste0(sites[i]," Pollinator Time Series"),x="Year",
         y="Insect Count")+
    scale_color_brewer("Insect Family",palette="Set1")+
    scale_x_continuous(breaks=unique(yr_fam$year))+
    theme_bw() -> sites.lines[[i]]
}

sites.bars = list()
for (i in 1:length(sites)) {
  site_yr_fam %>% filter(locality==sites[[i]]) %>%     
    ggplot()+geom_col(aes(x=year,y=sum,fill=family))+
    labs(title=paste0(sites[i]," Pollinator Time Series"),x="Year",
         y="Insect Count")+
    scale_fill_brewer("Insect Family",palette="Set1")+
    scale_x_continuous(breaks=unique(yr_fam$year))+
    theme_bw() -> sites.bars[[i]]
}


leaflet() %>% 
  addTiles() %>% 
  addCircles(data=site_means.sf,
             popup=~popupGraph(sites.lines))

leaflet() %>% 
  addTiles() %>% 
  addCircles(data=site_means.sf,
             popup=~popupGraph(sites.bars))
# Vis 3 ----
# Leaflet whole of Norway map, filter by year / family



# pop-up should give count, species

# leaflet filtered by year, maybe family too
# creating site centers
df %>% group_by(locality) %>% 
  summarize(individualCount=sum(individualCount),
            lat=mean(decimalLatitude),
            lon=mean(decimalLongitude)) -> locality.centers
# maybe choose 2 years and have 2 leaflets side by side

df %>% filter(year==2015) %>% group_by(locality,family) %>% 
  summarize(individualCount=sum(individualCount)) %>% 
  pivot_wider(id_cols=locality,names_from=family,values_from=individualCount) -> locality.family 
locality.family[is.na(locality.family)] <- 0

locality.family %>% right_join(locality.centers) %>% 
  st_as_sf(coords=c("lon","lat"),
           crs=4273) -> pt.locality_global


pt.locality_global$popup <- paste0("<b><h3>",pt.locality_global$locality," - ",year,"</b></h3>",
                            "<b>Total Pollinators: </b>",pt.locality_global$individualCount,
                            "</br><b>Bees: </b></br>","Apidae - ",pt.locality_global$Apidae,
                            "</br><b>Butterflies: </b></br>",
                            "Lycaenidae - ",pt.locality_global$Lycaenidae,
                            "</br>Nymphalidae - ",pt.locality_global$Nymphalidae,
                            "</br>Pieridae - ",pt.locality_global$Pieridae,
                            "</br>Hesperiidae - ",pt.locality_global$Hesperiidae)


pal3 <- colorNumeric(c("#ea698b","#d55d92","#c05299","#ac46a1","#973aa8","#822faf","#6d23b6","#6411ad","#571089","#47126b"),domain=NULL,
                     reverse = F)
# Allow people to choose what to size/color dots by
leaflet(pt.locality_global) %>% 
  addTiles() %>% 
  addCircles(radius =~ individualCount*8,
             popup =~ popup,
             color =~ pal3(individualCount)) %>% 
  addLegend("bottomright",pal=pal3,values=~individualCount,
            title="Total Pollinator Count",
            opacity=1)




# Vis 4 ----
# transect maps/leaflet
# leaflet filtered by site and year from above
# jittered slightly to allow clicking
locality = "Rinnan"
pt %>% filter(year==2015,
              locality=="Rinnan") %>% 
  st_jitter(factor=0.01) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircles(radius=~individualCount+1.5,
             popup =~ paste0(individualCount))

# Vis 5 ----
# Interpolation map
# interpolate the three areas separately

# finer grids from: 
# https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/norway-shapefile
st_read("dataRaw/no_1km.shp") %>% st_transform(4273) -> no1km
st_read("dataRaw/no_10km.shp") %>% st_transform(4273) -> no10km


df.raw %>% 
  filter(individualCount > 0) %>% 
  group_by(decimalLatitude,decimalLongitude,stateProvince) %>% 
  summarize(count=sum(individualCount)) -> df.idw.state

df.idw.state %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),
           crs=4273) -> sf.idw.state

a.sf <- sf.idw.state %>% filter(stateProvince == "Agder")
r.sf <- sf.idw.state %>% filter(stateProvince == "Rogaland")
ve.sf <- sf.idw.state %>% filter(stateProvince == "Vestfold og Telemark")
vi.sf <- sf.idw.state %>% filter(stateProvince == "Viken")
t.sf <- sf.idw.state %>% filter(stateProvince == "Tr?ndelag")

list(a.sf,r.sf,vi.sf,ve.sf,t.sf) -> state_list

# grids <- map(state_list,function(x) st_crop(no1km,x))


# map2(state_list,grids,function(x,y) idw(formula = count~1,locations = x,newdata=y)) -> interpolated

saveRDS(interpolated,"interpolated.rds")

interpolated[[1]]$stateProvince=unique(state_list[[1]]$stateProvince)
interpolated[[2]]$stateProvince=unique(state_list[[2]]$stateProvince)
interpolated[[3]]$stateProvince=unique(state_list[[3]]$stateProvince)
interpolated[[4]]$stateProvince=unique(state_list[[4]]$stateProvince)
interpolated[[5]]$stateProvince=unique(state_list[[5]]$stateProvince)


i <- rbind(interpolated[[1]],interpolated[[2]],interpolated[[3]],interpolated[[4]],interpolated[[5]]) 
s <- rbind(state_list[[1]],state_list[[2]],state_list[[3]],state_list[[4]],state_list[[5]])




 
# Shiny Combination ----
# constants for UI
vis3.labels <- c(names(pt.locality[2:5]))

pt.locality_global %>% rename(Total = individualCount) -> pt.locality.clean

pal3 <- colorNumeric(c("#ea698b","#d55d92","#c05299","#ac46a1","#973aa8","#822faf","#6d23b6","#6411ad","#571089","#47126b"),domain=NULL,
                     reverse = F)

ui <- fluidPage(
  theme = shinytheme("united"),
  
  # App title
  titlePanel("Norway Pollinators"),
  
 
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset ----
      tabsetPanel(type = "tabs",
                  tabPanel("Static Time Series",
                           plotOutput(outputId = "static_bar"),
                           plotOutput(outputId = "static_line")),
                  tabPanel("Interactive Time Series",
                           radioButtons(inputId = "ts_level",
                                        label ="Select a spatial scale to display, then click on a point!",
                                        choices = c("Site","State"),
                                        selected = "State"),
                           radioButtons(inputId = "bar_line",
                                        label ="Choose the type of summary graph you would like to see in the pop-up",
                                        choices=c("Bar","Line"),
                                        selected = "Bar"),
                           leafletOutput(outputId = "leaflet_ts")),
                  tabPanel("Leaflet", 
                           selectInput(inputId = "color_size",
                                       label="Choose a variable to color and size the points with",
                                       choices=c(names(pt.locality.clean[-1][-8][-7] %>% st_set_geometry(NULL))),
                                       selected="Total"),
                           selectInput(inputId = "year1",
                                       label="Select a first year to compare",
                                       choices=unique(df$year),
                                       selected=2019),
                           leafletOutput(outputId = "leaflet1"),
                           selectInput(inputId = "year2",
                                       label="Select a second year to compare",
                                       choices=unique(df$year),
                                       selected=2013),
                           leafletOutput(outputId = "leaflet2")),
                  tabPanel("Transects",
                           selectInput(inputId="site_tran",
                                       label="Select a site to view",
                                       choices = unique(df$locality),
                                       selected="Odden"),
                           plotOutput(outputId = "label"),
                           selectInput(inputId = "tran_year",
                                       label="Select years to facet wrap with",
                                       choices=unique(df$year),
                                       selected=2013,
                                       multiple=T),
                           plotlyOutput(outputId = "tran")),
                  tabPanel("Interpolation",
                           selectInput(inputId = "state",
                                       label="Select a State to zoom in on",
                                       choices = unique(df.raw$stateProvince),
                                       selected = "Agder"),
                           plotOutput(outputId = "inter"),
                           plotOutput(outputId = "inter_all")
                           )
                  )
      )
    )


server <- function(input,output){
  output$static_bar <- renderPlot({
    ts1
  })
  output$static_line <- renderPlot({
    ts2
  })
  output$leaflet_ts <- renderLeaflet({
    if (input$ts_level == "State" & input$bar_line == "Bar"){
             leaflet() %>% 
               addTiles() %>% 
               addCircles(data=state_means.sf,
                          popup=~popupGraph(bars))
    } else if (input$ts_level == "Site" & input$bar_line == "Bar") {
             leaflet() %>% 
               addTiles() %>% 
               addCircles(data=site_means.sf,
                          popup=~popupGraph(sites.bars))
    } else if (input$ts_level == "State"& input$bar_line == "Line"){
      leaflet() %>% 
        addTiles() %>% 
        addCircles(data=state_means.sf,
                   popup=~popupGraph(lines))
    } else if (input$ts_level == "Site"& input$bar_line == "Line"){
      leaflet() %>% 
        addTiles() %>% 
        addCircles(data=site_means.sf,
                   popup=~popupGraph(sites.lines))
    }
  })
  output$leaflet1 <- renderLeaflet({
    
    # Allow people to choose what to size/color dots by
    # coloring / sizing doesnt change with the year
    
    df %>% group_by(locality,family,year) %>% 
      summarize(individualCount=sum(individualCount)) %>% 
      pivot_wider(id_cols=c(locality,year),names_from=family,values_from=individualCount) -> locality.family 
    locality.family[is.na(locality.family)] <- 0
    
    z = rowSums(locality.family[3:7])
    
    locality.family %>% right_join(locality.centers[-2]) %>% 
      st_as_sf(coords=c("lon","lat"),
               crs=4273) -> pt.locality
    
    pt.locality$Total = z
    
    pt.locality %<>% filter(year==input$year1)
    pt.locality$popup <- paste0("<b><h3>",pt.locality$locality," - ",pt.locality$year,"</b></h3>",
                                "<b>Total Pollinators: </b>",pt.locality$Total,
                                "</br><b>Bees: </b></br>","Apidae - ",pt.locality$Apidae,
                                "</br><b>Butterflies: </b></br>",
                                "Lycaenidae - ",pt.locality$Lycaenidae,
                                "</br>Nymphalidae - ",pt.locality$Nymphalidae,
                                "</br>Pieridae - ",pt.locality$Pieridae,
                                "</br>Hesperiidae - ",pt.locality$Hesperiidae)
    
    pt.locality %>% dplyr::select(locality,input$color_size,geometry) -> x
    
    leaflet(pt.locality) %>% 
      addTiles() %>% 
      addCircles(radius =~ as.numeric(unlist(x[2] %>% st_set_geometry(NULL)))*40,
                 popup =~ popup,
                 color =~ pal3(unlist(x[2] %>% st_set_geometry(NULL)))) %>% 
      addLegend("bottomright",pal=pal3,values=~unlist(x[2] %>% st_set_geometry(NULL)),
                title=paste0(input$color_size," Count"),
                opacity=1)
  })
  output$leaflet2 <- renderLeaflet({
    
    # Allow people to choose what to size/color dots by
    # coloring / sizing doesnt change with the year
    
    df %>% group_by(locality,family,year) %>% 
      summarize(individualCount=sum(individualCount)) %>% 
      pivot_wider(id_cols=c(locality,year),names_from=family,values_from=individualCount) -> locality.family 
    locality.family[is.na(locality.family)] <- 0
    
    z = rowSums(locality.family[3:7])
    
    locality.family %>% right_join(locality.centers[-2]) %>% 
      st_as_sf(coords=c("lon","lat"),
               crs=4273) -> pt.locality
    
    pt.locality$Total = z
    
    pt.locality %<>% filter(year==input$year2)
    pt.locality$popup <- paste0("<b><h3>",pt.locality$locality," - ",pt.locality$year,"</b></h3>",
                                "<b>Total Pollinators: </b>",pt.locality$Total,
                                "</br><b>Bees: </b></br>","Apidae - ",pt.locality$Apidae,
                                "</br><b>Butterflies: </b></br>",
                                "Lycaenidae - ",pt.locality$Lycaenidae,
                                "</br>Nymphalidae - ",pt.locality$Nymphalidae,
                                "</br>Pieridae - ",pt.locality$Pieridae,
                                "</br>Hesperiidae - ",pt.locality$Hesperiidae)
    
    pt.locality %>% dplyr::select(locality,input$color_size,geometry) -> x
    
    leaflet(pt.locality) %>% 
      addTiles() %>% 
      addCircles(radius =~ as.numeric(unlist(x[2] %>% st_set_geometry(NULL)))*40,
                 popup =~ popup,
                 color =~ pal3(unlist(x[2] %>% st_set_geometry(NULL)))) %>% 
      addLegend("bottomright",pal=pal3,values=~unlist(x[2] %>% st_set_geometry(NULL)),
                title=paste0(input$color_size," Count"),
                opacity=1)
  })
  output$label <- renderPlot({
    pt %>% filter(locality==input$site_tran) -> loc
    
    ggplot()+
      geom_map(data=norway_filter, aes(map_id=region),
               map=norway_filter,color="grey")+
      expand_limits(x = norway_filter$long, y = norway_filter$lat)+
      geom_sf(data=loc,color="red")+
      geom_sf_text(data=loc[1,],aes(label=locality),color="red",nudge_y = .3)+
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
  })
  output$tran <- renderPlotly({
    df %>%  
      filter(locality==input$site_tran,
             year %in% input$tran_year) %>% 
      group_by(family,decimalLatitude,decimalLongitude) %>% 
      summarise(individualCount = sum(individualCount),
                locality=locality,
                year=year) %>% 
      st_as_sf(coords=c("decimalLongitude","decimalLatitude"),
               crs=4273)  %>% 
      rbind(pt.absence %>% filter(locality==input$site_tran,
                                  year %in% input$tran_year) %>% 
              mutate(family="Absent")) -> t
    
    
    g <- ggplot()+
      geom_sf(data=t,
              aes(color=family,size=individualCount),alpha=0.4)+
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid=element_blank())+
      facet_wrap(~year)+
      scale_size_continuous("Count")+
      scale_color_brewer("Family",palette = "Set1")+
      ggtitle("Pollinator Transects")
    
    ggplotly(g)
    
  })
  output$inter <- renderPlot({
    i %<>% filter(stateProvince == input$state)
    df.raw %>% filter(stateProvince == input$state) %>% 
      group_by(decimalLatitude,decimalLongitude) %>% 
      summarise(individualCount=sum(individualCount)) -> pt
    
    ggplot()+
      geom_sf(data=i,aes(fill=var1.pred),color=NA)+
      scale_fill_distiller("Estimated Pollinator Count",palette = "YlGn",direction=1)+
      geom_map(data=norway_filter, aes(map_id=region),alpha=0.1,
               map=norway_filter,color="black")+
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
      
  })
  output$inter_all <- renderPlot({
    ggplot()+
      geom_sf(data=i,aes(fill=var1.pred),color=NA)+
      scale_fill_distiller("Estimated Pollinator Count",palette="YlGn",direction=1)+
      geom_map(data=norway_filter, aes(map_id=region),alpha=0.1,
               map=norway_filter,color="black")+
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
  })
}
# shinyrun ----
shinyApp(ui, server)





# testing ----
output$leaflet1 <- renderLeaflet({
  
  # Allow people to choose what to size/color dots by
  # coloring / sizing doesnt change with the year
  
  df %>% group_by(locality,family,year) %>% 
    summarize(individualCount=sum(individualCount)) %>% 
    pivot_wider(id_cols=c(locality,year),names_from=family,values_from=individualCount) -> locality.family 
  locality.family[is.na(locality.family)] <- 0
  
  z = rowSums(locality.family[3:7])
  
  locality.family %>% right_join(locality.centers[-2]) %>% 
    st_as_sf(coords=c("lon","lat"),
             crs=4273) -> pt.locality
  
  pt.locality$total_pol = z
  
  pt.locality %<>% filter(input$year1)
  pt.locality$popup <- paste0("<b><h3>",pt.locality$locality," - ",pt.locality$year,"</b></h3>",
                              "<b>Total Pollinators: </b>",pt.locality$total_pol,
                              "</br><b>Bees: </b></br>","Apidae - ",pt.locality$Apidae,
                              "</br><b>Butterflies: </b></br>",
                              "Lycaenidae - ",pt.locality$Lycaenidae,
                              "</br>Nymphalidae - ",pt.locality$Nymphalidae,
                              "</br>Pieridae - ",pt.locality$Pieridae,
                              "</br>Hesperiidae - ",pt.locality$Hesperiidae)
  
  pt.locality %>% dplyr::select(locality,input$color_size,geometry) -> x
  
  leaflet(pt.locality) %>% 
    addTiles() %>% 
    addCircles(radius =~ as.numeric(unlist(x[2] %>% st_set_geometry(NULL)))*10,
               popup =~ popup,
               color =~ pal3(unlist(x[2] %>% st_set_geometry(NULL)))) %>% 
    addLegend("bottomright",pal=pal3,values=~unlist(x[2] %>% st_set_geometry(NULL)),
              title=paste0(input$color_size," Count"),
              opacity=1)
})


df %>% group_by(locality,family,year) %>% 
  summarize(individualCount=sum(individualCount)) %>% 
  pivot_wider(id_cols=c(locality,year),names_from=family,values_from=individualCount) -> locality.family 
locality.family[is.na(locality.family)] <- 0

z = rowSums(locality.family[3:7])

locality.family %>% right_join(locality.centers[-2]) %>% 
  st_as_sf(coords=c("lon","lat"),
           crs=4273) -> pt.locality

pt.locality$total_pol = z

pt.locality %<>% filter(year==2013)
pt.locality$popup <- paste0("<b><h3>",pt.locality$locality," - ",pt.locality$year,"</b></h3>",
                            "<b>Total Pollinators: </b>",pt.locality$total_pol,
                            "</br><b>Bees: </b></br>","Apidae - ",pt.locality$Apidae,
                            "</br><b>Butterflies: </b></br>",
                            "Lycaenidae - ",pt.locality$Lycaenidae,
                            "</br>Nymphalidae - ",pt.locality$Nymphalidae,
                            "</br>Pieridae - ",pt.locality$Pieridae,
                            "</br>Hesperiidae - ",pt.locality$Hesperiidae)

pt.locality %>% dplyr::select(locality,Apidae,geometry) -> x

leaflet(pt.locality) %>% 
  addTiles() %>% 
  addCircles(radius =~ as.numeric(unlist(x[2] %>% st_set_geometry(NULL)))*10,
             popup =~ popup,
             color =~ pal3(unlist(x[2] %>% st_set_geometry(NULL)))) %>% 
  addLegend("bottomright",pal=pal3,values=~unlist(x[2] %>% st_set_geometry(NULL)),
            title=paste0("Apidae"," Count"),
            opacity=1)












# old testing ----
st_as_sf(df,coords=c("decimalLongitude","decimalLatitude"),
         crs=4273) -> pt

df %>%  
  filter(locality=="Agle",
         year %in% c(2013,2015)) %>% 
  group_by(family,decimalLatitude,decimalLongitude) %>% 
  summarise(individualCount = sum(individualCount),
            locality=locality,
            year=year) %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),
           crs=4273)  %>% 
  rbind(pt.absence %>% filter(locality=='Agle',
                              year %in% c(2013,2015)) %>% 
          mutate(family="Absent")) -> test

gtest <- ggplot()+
  geom_sf(data=test,
          aes(color=Family,size=Count),alpha=0.4)+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid=element_blank())+
  facet_wrap(~year)+
  scale_color_brewer("Family",palette = "Set1")+
  scale_size_continuous("&  Count")+
  ggtitle("Pollinator Transects")

ggplotly(gtest)



output$tran <- renderPlotly({
  pt %>%  
    filter(locality==input$site_tran,
           year %in% input$tran_year) %>% 
    dplyr::select(locality,individualCount,year,family) %>% 
    rbind(pt.absence %>% filter(locality==input$site_tran,
                                year %in% input$tran_year) %>% 
            mutate(family="Absent")) -> loc
  df %>%  
    filter(locality==input$site_tran,
           year %in% input$tran_year) %>% 
    group_by(family,decimalLatitude,decimalLongitude) %>% 
    summarise(individualCount = sum(individualCount),
              locality=locality,
              year=year) %>% 
    st_as_sf(coords=c("decimalLongitude","decimalLatitude"),
             crs=4273)  %>% 
    rbind(pt.absence %>% filter(locality==input$site_tran,
                                year %in% input$tran_year) %>% 
            mutate(family="Absent")) -> t
  
  
  g <- ggplot()+
    geom_sf(data=t,
            aes(color=family,size=individualCount),alpha=0.4)+
    theme_bw()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid=element_blank())+
    facet_wrap(~year)+
    scale_color_brewer("Family",palette = "Set1")+
    scale_size_continuous("&  Count")+
    ggtitle("Pollinator Transects")
  
  ggplotly(g)
  
})

