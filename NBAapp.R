library(shiny)
library(leaflet)
library(openxlsx)
library(sp)
library(plyr)

df = read.xlsx("NBAbirthplaces.xlsx")

df$PlayerTeams = paste0(df$Player, " (", df$Team, ")")

chd = substr(df$Latitude, 3, 3)[1]
chm = substr(df$Latitude, 6, 6)[1]
chs = substr(df$Latitude, 9, 9)[1]

df$lats = as.numeric(char2dms(df$Latitude,chd=chd,chm=chm,chs=chs))

chd = substr(df$Longitutde, 3, 3)[1]
chm = substr(df$Longitutde, 6, 6)[1]
chs = substr(df$Longitutde, 9, 9)[1]

df$longs = as.numeric(char2dms(df$Longitutde,chd=chd,chm=chm,chs=chs))

df2 <- ddply(df, .(lats, longs, Birthplace), summarize,
             PlayerTeams=paste(PlayerTeams,collapse=" <br> "))




ui <- fluidPage(title = "Birthplaces of NBA players",
                titlePanel(h1("Birthplaces of NBA players",
                              style='text-align: center')),
                leafletOutput("mymap", height = "90vh")
)

server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        leaflet(df2)%>%
            addCircleMarkers(lng = ~longs, lat = ~lats,
                             radius = ~sqrt(nchar(PlayerTeams)),
                             popup = ~as.character(paste0("<h3>", Birthplace, "</h3>",
                                                          "<h4>Players: </h4>", PlayerTeams)))%>%
            setView(lng = -99, lat = 40, zoom = 5)%>%
            addProviderTiles(providers$OpenStreetMap)
        
    })
}

shinyApp(ui = ui, server = server)