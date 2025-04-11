# data downloaded from the FAA National Wildlife Strike Database
  # wildlife.faa.gov

# SETUP

  setwd("/Volumes/SANDISK 1TB/code/wildlife strikes")
  
  # install.packages("Rmisc")
  # install.packages("tidyverse")
  # install.packages("usmap")
  # install.packages ("sf")
  
  library(tidyverse)
  library(usmap)
  library(sf)
  library(dplyr)
  
  # creating data frame for all incidents on record
  
  NWSDall <- read_csv("NWSD.csv") %>%
    select(INCIDENT_DATE, INCIDENT_MONTH, INCIDENT_YEAR, TIME_OF_DAY, STATE, HEIGHT, AIRPORT_ID,
           AIRPORT, AIRPORT_LATITUDE, AIRPORT_LONGITUDE, AIRCRAFT, OPERATOR, SPECIES, OUT_OF_RANGE_SPECIES,
           COST_REPAIRS_INFL_ADJ, COST_OTHER_INFL_ADJ, NR_INJURIES, NR_FATALITIES, BIRD_BAND_NUMBER) %>%
    dplyr::rename(LATITUDE = AIRPORT_LATITUDE, LONGITUDE = AIRPORT_LONGITUDE,
                  DATE = INCIDENT_DATE, MONTH = INCIDENT_MONTH, YEAR = INCIDENT_YEAR) %>%
    arrange(YEAR, MONTH)

  # creating data frame for incidents in 2024 in the 50 U.S. states and DC
  
    NWSD24 <- NWSDall %>%
      select(DATE, MONTH, YEAR, TIME_OF_DAY, STATE, HEIGHT, AIRPORT_ID, AIRPORT, AIRCRAFT, OPERATOR,
             LATITUDE, LONGITUDE, SPECIES) %>%
      filter(YEAR == 2024,
             STATE %in% c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL",
             "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
             "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
             "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")) %>%
      arrange(YEAR, MONTH)
    
  # creating data frame for total flights
    
    APM <- read_csv("APM.csv") %>%
      dplyr::rename(AIRPORT_ID = Facility) %>%
       mutate(FLIGHTS = Departures + Arrivals) %>%
       select(AIRPORT_ID, FLIGHTS)

# 2024
    
  # TOTALS
    
      incidentcount24 <- nrow(NWSD24)
      airportcount24 <- n_distinct(NWSD24$AIRPORT_ID)
      speciescount24 <- n_distinct(NWSD24$SPECIES)
      datecount24 <- n_distinct(NWSD24$DATE)
      
  # BY SPECIES
      
    # all species, including unknown and nonavian
      
      NWSD24_taxa <- NWSD24 %>%
        group_by(SPECIES) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
      
    # filtering for non-avian taxa
      
      nonavian <- c( # non-avian taxa in 2024
                    "Brazilian free-tailed bat", "Microbats", "White-tailed jackrabbit", "Eastern red bat",
                    "Striped skunk", "Little brown bat", "Big brown bat", "Coyote", "Vesper bats",
                    "Virginia opossum", "White-tailed deer", "Evening bat", "Desert cottontail", "Hoary bat",
                    "Black-tailed jackrabbit", "Black-tailed prairie dog", "Red fox", "Turtles", "Woodchuck",
                    "Diamondback terrapin", "Gopher snake", "Silver-haired bat", "Seminole bat",
                    "Eastern cottontail", "Raccoon", "Free-tailed bats", "Nine-banded armadillo",
                    "Gopher tortoise", "Green iguana", "Hares", "North American porcupine", "Rabbits",
                    "Skunks", "Common box turtle", "Tri-colored bat", "American alligator",
                    "Common snapping turtle", "Domestic/feral cat", "Domestic/feral dog",
                    "Florida softshell turtle", "Foxes", "Northern yellow bat", "Pond slider",
                    "Unknown terrestial mammal", "American badger", "Big free-tailed bat", "Brush rabbit",
                    "California myotis", "Coastal plain cooter", "Common garter snake", "Common kingsnake",
                    "Diamondback water snake", "Eastern mud turtle", "Eastern rat snake", "Gray bat",
                    "Ground squirrels", "Gunnison's prairie dog", "Least weasel", "Leschenault's rousette",
                    "Long-tailed weasel", "Mohave rattlesnake", "Mule deer", "Northern map turtle",
                    "Peccaries", "Small Indian mongoose", "Snakes", "Water moccasin", "Western fox snake",
                    "Western red bat", "Western ribbon snake", "White-tailed prairie dog",
                    # non-avian taxa not in 2024
                    "African yellow bat", "Alligator snapping turtle", "American black bear", "American mink",
                    "American red squirrel", "Angolan free-tailed bat", "Antelope jackrabbit", "Antillean fruit-eating bat",
                    "Axis deer", "Bats", "Bearded seal", "Black mastiff bat", "Broad-eared bat", "Brown bear",
                    "Brown flower bat", "California ground squirrel", "California kingsnake", "Canids", "Cape serotine",
                    "Caribou", "Cattle", "Checkered garter snake", "Chicken turtle", "Collared peccary", "Common gray fox",
                    "Common pipistrelle", "Corn snake", "Coypu (nutria)", "Cuban flower bat", "Deer", "Domestic sheep",
                    "Eastern diamondback rattlesnake", "Eastern gray squirrel", "Eastern hognose snake", "Eastern pine snake",
                    "Eastern racer", "Eastern small-footed myotis", "Egyptian free-tailed bat", "Florida bonneted bat",
                    "Florida red-bellied cooter", "Flying foxes", "Fox squirrel", "Gray sac-winged bat",
                    "Grey-headed flying fox", "Horse", "Indian flying fox", "Indian pipistrelle", "Indian silverbill",
                    "Indiana bat", "Jamaican fruit bat", "Kelaart's pipistrelle", "Kit fox", "Kuhl's pipistrelle",
                    "Lagomorphs (rabbits, hares)", "Lesser bulldog bat", "Lined snake", "Little red flying fox",
                    "Long-eared myotis", "Long-legged myotis", "Marsh rabbit", "Mauritian tomb bat", "Megabats", "Milk snake",
                    "Moose", "Muskrat", "Naked-rumped tomb bat", "North American beaver", "Northern water snake",
                    "Ornate box turtle", "Painted turtle", "Pallas's mastiff bat", "Piute ground squirrel",
                    "Plains garter snake", "Pocketed free-tailed bat", "Prairie dogs, marmots, squirrels", "Prairie rattlesnake",
                    "Pronghorn", "Richardson's ground squirrel", "River cooter", "River otter", "Sinaloan mastiff bat",
                    "Snowshoe hare", "Sooty mustached bat", "Spectacled caiman", "Spiny softshell turtle", "Spotted bat",
                    "Striped mud turtle", "Swine (pigs)", "Texas blind snake", "Thirteen-lined ground squirrel",
                    "Tree squirrels", "Wagner's bonneted bat", "Wapiti (elk)", "Western diamondback rattlesnake",
                    "Western hognose snake", "Western mastiff bat", "Western pipistrelle", "Western rat snake",
                    "Western small-footed myotis", "Western yellow bat", "White-nosed coati", "Yellow-bellied marmot",
                    "Yuma myotis")
      
      nonavian24 <- NWSD24 %>%
        filter(SPECIES %in% nonavian) %>%
        group_by(SPECIES) %>%
        summarize(count = n(),
                  earliest_date = min(DATE, na.rm = TRUE),
                  latest_date = max(DATE, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(latest_date = format(latest_date, "%B %d")) %>%
        arrange(desc(count))
      
      # write.csv(nonavian24, "nonavian24.csv")
      
      nonavian24count <- nrow(nonavian24)
      nonavian24sp <- n_distinct(nonavian24$SPECIES)
      
     # filtering for taxa not identified to species
      
      unknown <- c("Unknown bird", "Unknown bird - small", "Unknown bird - medium", "Unknown bird - large",
                   "Perching birds (y)", "Unknown bird or bat", "Unknown terrestial mammal")
      
      spuh <- c("Alder/willow flycatcher complex", "Blackbirds", "Black-headed gull complex", "Blackbirds, meadowlarks, orioles",
                "Cardinals, NA tanagers, misc. buntings", "Chickadees", "Cormorants", "Cranes", "Crows", "Crows, ravens",
                "Cuckoos", "Cuckoos, roadrunners", "Diving duck (Aythya)", "Doves", "Ducks", "Ducks, geese, swans", "Eagles",
                "Egrets", "Falcons, caracaras", "Falcons, kestrels, falconets", "Finches, euphonias", "Frigatebirds", "Geese",
                "Goatsuckers, nightjars, frogmouths", "Grackles", "Grebes", "Grouse", "Gulls", "Hawks", "Herons",
                "Herons, egrets, bitterns",  "Hummingbirds", "Ibises", "Kites", "Kites, eagles, hawks", "Kittiwakes", "Larks",
                "Longspurs, snow buntings", "Loons", "Magpies", "Mallard/American black duck complex",
                "Mallard/mottled duck complex", "Meadowlarks", "Munias", "Mynas", "New World quail", "New World wood-warblers",
                "New World vultures", "Nightjars", "Noddies", "Old World vultures", "Orioles", "Parrots", "Partridges", "Owls",
                "Parakeets", "Pelicans", "Perching birds (y)", "Pheasants", "Pigeons", "Pigeons, doves", "Plovers",
                "Plovers, lapwings", "Ptarmigans", "Quails, pheasants",
                "Raptors: Hawks, eagles, vultures, kites, osprey, falcons, caracaras", "Rails",
                "Sandpipers, curlews, phalaropes, allies", "Shearwaters", "Shorebirds", "Snow goose/Ross's goose complex",
                "Sparrows", "Starlings, mynas", "Storm-petrels", "Swallows", "Swans", "Swifts", "Swift, tree-swifts, hummingbirds",
                "Terns, noddies", "Thrashers", "Thrushes", "Towhees", "Tropicbirds", "Turkeys", "Typical owls",
                "Tyrant (New World) flycatchers", "Vireos", "Waxbills, mannikins, parrotfinches", "Western/Clark's grebe complex",
                "White-/golden-crown sparrow complex", "White-headed gull complex", "Woodpeckers", "Wrens")
      
    # identified bird taxa
      
      birds24 <- NWSD24 %>%
        filter(!SPECIES %in% unknown,
               !SPECIES %in% nonavian,
               !SPECIES %in% spuh) %>%
        group_by(SPECIES) %>%
        summarize(count = n()) %>%
                  # latest_date = max(DATE, na.rm = TRUE),
                  # .groups = "drop") %>%
        # mutate(latest_date = format(latest_date, "%B %d")) %>%
        arrange(desc(count))
      
      # write.csv(birds24, "birds24.csv")
      
    # top 10 species/unknowns
      
      top10taxa24 <- NWSD24 %>%
        group_by(SPECIES) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        slice(1:10)
      
      top10taxa24_plot <- ggplot(top10taxa24, aes(x = reorder(SPECIES, count), y = count, fill = SPECIES)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme_minimal() +
        scale_fill_manual(values = c("#bddf26", "#7ad151", "#44bf70","#22a884","#21918c",
                                     "#2a788e", "#355f8d", "gray60", "gray70", "gray80")) +
        labs(title = "2024's top taxa",
             subtitle = str_wrap("Most wildlife strikes are identified, but the top two spots are still
                                 categories of unidentified birds.", 80),
             x = element_blank(),
             y = element_blank(),
             caption = "Data and code at github.com/teyalex/birdstrikes.") +
        coord_flip() +
        scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500),
                           position = "right",
                           expand = c(0,0)) +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 12),
              plot.caption = element_text(size = 10, color = "gray60"),
              axis.text = element_text(size = 12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              text = element_text(family = "PT Mono"))
      
      # ggsave("top10taxa24_plot.png", width = 10.8, height = 7.2, units = "in")
      
    # Mourning Dove breakdown
      
      # count of MODO incidents
      
        MODOtotals <- NWSD24 %>%
          filter(SPECIES == "Mourning dove") %>%
          group_by(AIRPORT) %>%
          summarize(count = n()) %>%
          arrange(desc(count))
        
      # ratio of MODO incidents
      
        MODOratios <- NWSD24 %>%
          group_by(AIRPORT, STATE, SPECIES = if_else(SPECIES == "Mourning dove", "Mourning dove", "Other species")) %>%
          summarize(count = n(), .groups = "drop") %>%
          pivot_wider(names_from = SPECIES, values_from = count, values_fill = list(count = 0)) %>%
          mutate(total = `Mourning dove` + `Other species`,
                 MODO_ratio = `Mourning dove` / total,
                 SP_ratio = `Other species` / total) %>%
          filter(`Other species` >= 30) %>%
          arrange(desc(MODO_ratio))
        
      # binomial test of MODO incidents
      
        MODObinom <- MODOratios %>%
          rowwise() %>%
          mutate(p_value = binom.test(`Mourning dove`, total, p = mean(MODO_ratio), alternative = "greater")$p.value,
                 AIRPORT = str_to_title(AIRPORT, locale = "en")) %>%
          arrange(p_value)
        
        # write.csv(MODObinom, "MODObinom.csv")
        
  # BY STATE
    
    states24 <- NWSD24 %>%
      group_by(STATE) %>%
      summarize(count = n()) %>%
      mutate(STATE = recode(STATE,
                            "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
                            "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut",
                            "DC" = "Washington, DC", "DE" = "Delaware",
                            "FL" = "Florida",
                            "GA" = "Georgia", "GU" = "Guam",
                            "HI" = "Hawaii",
                            "ID" = "Idaho",
                            "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
                            "KS" = "Kansas", "KY" = "Kentucky",
                            "LA" = "Louisiana",
                            "ME" = "Maine", "MD" = "Maryland", "MA" = "Massachusetts", "MI" = "Michigan",
                            "MN" = "Minnesota", "MP" =  "Northern Mariana Islands", "MS" = "Mississippi",
                            "MO" = "Missouri", "MT" = "Montana",
                            "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
                            "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota",
                            "OH" = "Ohio", "OK" = "Oklahoma", "OR" = "Oregon",
                            "PA" = "Pennsylvania", "PR" = "Puerto Rico",
                            "RI" = "Rhode Island",
                            "SC" = "South Carolina", "SD" = "South Dakota",
                            "TN" = "Tennessee", "TX" = "Texas",
                            "UT" = "Utah",
                            "VI" = "U.S. Virgin Islands", "VT" = "Vermont", "VA" = "Virginia",
                            "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming")) %>%
      arrange(desc(count))
    
   states24_plot <- ggplot(states24, aes(x = reorder(STATE, count), y = count, fill = count)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      coord_flip() +
      scale_fill_viridis_c() +
      theme_minimal() +
      scale_y_continuous(position = "right") +
      labs(title = "Incident counts by state in 2024",
           x = element_blank(),
           y = element_blank(),
           caption = "Data and code at github.com/teyalex/birdstrikes.") +
      theme(plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 10, color = "gray60"),
            axis.text = element_text(size = 10),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            text = element_text(family = "PT Mono"))
   
   # ggsave("states24_plot.png", width = 10.8, height = 7.2, units = "in")
    
  # BY AIRPORT
    
    # top airports
    
      airports24 <- NWSD24 %>%
        group_by(AIRPORT, AIRPORT_ID, STATE) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        mutate(AIRPORT = str_to_title(AIRPORT, locale = "en"))
      
    # number of flights comparison
      
      airportIDs <- NWSD24 %>%
        group_by(AIRPORT_ID, AIRPORT, STATE) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        mutate(AIRPORT_ID = sub(".", "", AIRPORT_ID))

      flights24 <- APM %>%
        inner_join(airportIDs, by = "AIRPORT_ID") %>%
        mutate(pct = (count/FLIGHTS)*100,
               AIRPORT = str_to_title(AIRPORT, locale = "en")) %>%
        arrange(desc(pct)) %>%
        select(AIRPORT, STATE, pct, count, FLIGHTS) %>%
        rename(Airport = AIRPORT, `%` = pct, `#` = count, `Total flights` = FLIGHTS)
      
      # write.csv(flights24, "flights24.csv")
      
      sum(flights24$`Total flights`)/2/9730020 # flights recorded in the ASPM 82, divided by 2 to account for the double-counting
                                                  # of arrivals and departures, divided by the total number of U.S. flights
      
      rate <- (mean(flights24$`%`)/100) # average strike rate
      
      1/rate # average number of flights between strikes
      
      ceiling(log(0.05) / log(1 - rate))  # number of flights at which one can by 95% certain that a strike has occurred
      
    # Denver breakdown
      
      # including unidentified species
      
        DENtaxa <- NWSD24 %>%
          filter(AIRPORT_ID == "KDEN") %>%
          group_by(SPECIES) %>%
          summarize(count = n()) %>%
          arrange(desc(count))
        
        # write.csv(DENtaxa, "DENtaxa.csv")
        
      # identified species
      
        DENsp <- NWSD24 %>%
          filter(AIRPORT_ID == "KDEN",
                 !SPECIES %in% unknown,
                 !SPECIES %in% spuh) %>%
          group_by(SPECIES) %>%
          summarize(count = n()) %>%
          arrange(desc(count))
        
        DENsp_top <- DENsp %>%
          slice(1:10) %>%
          mutate(HOLA = ifelse(SPECIES == "Horned lark", "No. 1 most frequent species", "Species 2-10"))
        
        DENsp_top_plot <- ggplot(DENsp_top, aes(x = reorder(HOLA, -count), y = count, fill = SPECIES)) +
          geom_bar(position = "stack", stat = "identity", show.legend = FALSE) +
          geom_text(aes(label = SPECIES), size = 3, position = position_stack(vjust = 0.5), family = "PT Mono") +
          theme_minimal() +
          scale_fill_manual(values = c("#d0e11c", "#a0da39", "#73d056","#4ac16d","#fde725",
                                       "#2db27d", "#1fa187", "#277f8e", "#2e6e8e", "#365c8d")) +
          labs(title = "Denver's top victims",
               subtitle = str_wrap("These 10 species accounted for nearly three-quarters of Denver's 817 incidents in 2024.
                                   Within these 10 species, Horned Lark accounted for over half of the incidents.", 60),
               x = element_blank(),
               y = element_blank(),
               caption = "Data and code at github.com/teyalex/birdstrikes.") +
          theme(plot.title = element_text(face = "bold", size = 20),
                plot.subtitle = element_text(size = 12),
                plot.caption = element_text(size = 10, color = "gray60"),
                axis.text.x = element_text(face = "bold", size = 10, color = "black"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                text = element_text(family = "PT Mono"))
        
        # ggsave("DENsp_top_plot.png", width = 7.2, height = 7.2, units = "in")

  # BY DATE
      
    date24 <- NWSD24 %>%
      group_by(DATE) %>%
      summarize(count = n())

    date24_plot <- ggplot(date24, aes(x = DATE, y = count)) +
      geom_line(color = "#C93312") +
      theme_minimal() +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(title = "Seasonal bird strike frequency",
           subtitle = "Nonzero incident counts by day through 2024.",
           x = element_blank(),
           y = element_blank(),
           color = "Year",
           caption = "Data and code at github.com/teyalex/birdstrikes.") +
      theme(plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 10, color = "gray60"),
            axis.text = element_text(size = 12),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            text = element_text(family = "PT Mono"))
    
    # ggsave("date24_plot.png", width = 10.8, height = 7.2, units = "in")
    
# ALL YEARS
    
  # TOTALS
    
    incidentcount <-nrow(NWSDall)
    airportcount <- n_distinct(NWSDall$AIRPORT_ID)
    speciescount <- n_distinct(NWSDall$SPECIES)
    
  # BY DATE
    
    date <- NWSDall %>%
      filter(YEAR < 2024) %>%
      group_by(DATE, YEAR) %>%
      summarize(count = n()) %>%
      mutate(DATE = as.Date(paste("2024", month(DATE), day(DATE), sep = "-")))
    
    date_plot <- ggplot(date, aes(x = DATE, y = count, group = YEAR, color = YEAR)) +
      geom_line(alpha = 0.95) +
      theme_minimal() +
      scale_color_continuous(low = "#EBCC2A", high = "#F11B00") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(title = "Seasonal bird strike frequency",
           subtitle = "Reported incident counts for days with at least one incident, 1990-2023.",
           x = element_blank(),
           y = element_blank(),
           color = "Year",
           caption = "Data and code at github.com/teyalex/birdstrikes.") +
      theme(plot.title = element_text(face = "bold", size = 20),
            plot.subtitle = element_text(size = 12),
            plot.caption = element_text(size = 10, color = "gray60"),
            axis.text = element_text(size = 12),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            text = element_text(family = "PT Mono"))
    
    # ggsave("date_plot.png", width = 10.8, height = 7.2, units = "in")
  
   # BY MONTH

    # DATA: number of incidents in each month
    
      months <- NWSDall %>%
        group_by(YEAR, MONTH) %>%
        filter(YEAR < 2024) %>%
        summarize(count = n()) %>%
        mutate(MONTH = factor(MONTH, levels = 1:12, labels = month.abb))
      
    # PLOT: box plots of each month's incident counts
    
      months_plot <- ggplot(months, aes(x = MONTH, y = count)) +
        geom_boxplot(fill = "#C93312") +
        theme_minimal() +
        labs(title = "Incident frequency by month",
             subtitle = "Distribution of monthly incident totals, 1990-2023.",
             x = element_blank(),
             y = element_blank(),
             caption = "Data and code at github.com/teyalex/birdstrikes.") +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 12),
              plot.caption = element_text(size = 10, color = "gray60"),
              axis.text = element_text(size = 12),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              text = element_text(family = "PT Mono"))
      
      # ggsave("months_plot.png", width = 10.8, height = 7.2, units = "in")
      
  # BY AIRPORT
    
    # number of incidents at each set of lat-long coordinates
    
      latlong <- NWSDall %>%
        group_by(LONGITUDE, LATITUDE) %>%
        filter(LATITUDE >= 24.396 & LATITUDE <= 49.384,
               LONGITUDE >= -125.0 & LONGITUDE <= -66.934) %>%
        summarize(count = n()) %>%
        arrange(count)
    
    # combined base map shapefile and latlong coordinates
    
      latlong_sf <- st_as_sf(latlong, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
      
      usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
      
      base_map <- ggplot(usa) +
        geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
        coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"),
                 datum = NA) +
        theme_minimal()
      
      map <- base_map +
        geom_sf(data = latlong_sf, aes(color = count, size = count), alpha = 0.85) +
        scale_color_continuous(low = "bisque", high = "#F11B00", guide = "legend",
                               breaks = c(1, 2000, 4000, 6000, 8000, 10000)) +
        scale_size_continuous(range = c(1, 10), guide = "legend",
                              breaks = c(1, 2000, 4000, 6000, 8000, 10000)) +
        theme_minimal() +
        labs(title = "Number of incidents reported at each airport",
             color = element_blank(),
             size = element_blank(),
             alpha = element_blank(),
             caption = "Data and code at github.com/teyalex/birdstrikes.") +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 12),
              plot.caption = element_text(size = 10, color = "gray60"),
              axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              text = element_text(family = "PT Mono"))
      
      # ggsave("map.png", width = 10.8, height = 7.2, units = "in")
      
    # number of incidents at each airport
        
      airports <- NWSDall %>%
        group_by(AIRPORT) %>%
        filter(AIRPORT != "UNKNOWN") %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        mutate(AIRPORT = str_to_title(AIRPORT, locale = "en"))
        
      # write.csv(airports, "airports.csv")
      
  # BY TIME OF DAY
    
    # number of incidents each month by time of day
    
      time <- NWSDall %>%
        select(TIME_OF_DAY, HEIGHT) %>%
        filter(complete.cases(.)) %>%
        group_by(TIME_OF_DAY) %>%
        summarize(count = n(),
                  mean = mean(HEIGHT),
                  median = median(HEIGHT),
                  sd = sd(HEIGHT),
                  q1 = quantile(HEIGHT, 0.25),
                  q3 = quantile(HEIGHT, 0.75))
      
      # write.csv(time, "time.csv")
      
      time_df <- NWSDall %>%
        select(TIME_OF_DAY, HEIGHT) %>%
        filter(complete.cases(.))
      
      time_plot <- ggplot(time_df, aes(x = TIME_OF_DAY, fill = TIME_OF_DAY, y = HEIGHT)) +
        geom_boxplot(outlier.shape = NA,
                     show.legend = FALSE,
                     color = "black") +
        coord_cartesian(ylim = c(0, 8000)) +
        theme_minimal() +
        scale_fill_manual(values = c("#F8AFA8", "#FAD510","#354823", "#273046")) +
        labs(title = "Altitudinal distribution by time of day",
             subtitle = str_wrap("Daytime bird strikes were more likely to occur at or near ground level,
                                 while incidents at night were more likely to occur at higher altitudes.", 80),
             x = element_blank(),
             y = "Altitude (ft.)",
             caption = "Data and code at github.com/teyalex/birdstrikes.") +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 12),
              plot.caption = element_text(size = 10, color = "gray60"),
              axis.text.x = element_text(face = "bold", size = 12, color = "black"),
              axis.text.y = element_text(size = 12),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              text = element_text(family = "PT Mono"))
      
      # ggsave("time_plot.png", width = 10.8, height = 7.2, units = "in")
      
  # BY SPECIES
  
    # species counts
     
      species <- NWSDall %>%
        group_by(SPECIES) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        filter(!SPECIES %in% unknown,
               !SPECIES %in% nonavian,
               count >= 1000)
      
      species_plot <- ggplot(species, aes(x = reorder(SPECIES, count), y = count, fill = count)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        coord_flip() +
        scale_fill_viridis_c() +
        theme_minimal() +
        scale_y_continuous(position = "right") +
        labs(title = "Most-reported taxa",
             subtitle = str_wrap("These 30 taxa have been reported in 1000 or more incidents since 1990."),
             x = element_blank(),
             y = element_blank(),
             caption = "Data and code at github.com/teyalex/birdstrikes.") +
        theme(plot.title = element_text(face = "bold", size = 20),
              plot.subtitle = element_text(size = 12),
              plot.caption = element_text(size = 10, color = "gray60"),
              axis.text = element_text(size = 12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              text = element_text(family = "PT Mono"))
      
      # ggsave("species_plot.png", width = 10.8, height = 7.2, units = "in")
      
    # unusual species
      
      ones <- NWSDall %>%
        group_by(SPECIES) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        filter(count == 1)
        
      outofrange <- NWSDall %>%
        arrange(DATE) %>%
        filter(OUT_OF_RANGE_SPECIES == TRUE)
       
  # COSTS, DEATHS, INJURIES
       
    # costs (all)
         
      costs <- NWSDall %>%
        select(YEAR, SPECIES, AIRPORT, AIRCRAFT, COST_REPAIRS_INFL_ADJ, COST_OTHER_INFL_ADJ) %>%
        filter( !is.na(COST_REPAIRS_INFL_ADJ) & 
                !is.na(COST_OTHER_INFL_ADJ)) %>%
        arrange(desc(COST_REPAIRS_INFL_ADJ))
      
      coststotal <- sum(costs$COST_REPAIRS_INFL_ADJ) + sum(costs$COST_OTHER_INFL_ADJ)
    
    # costs (2024)
    
      costs24 <- costs %>% 
        filter(YEAR == 2024)
      
      coststotal24 <- sum(costs24$COST_REPAIRS_INFL_ADJ) + sum(costs24$COST_OTHER_INFL_ADJ)
      costsavg <- coststotal / n_distinct(costs$YEAR)
      
    # DEATHS, INJURIES 
    
      casualties_df <- NWSDall %>%
      select(DATE, SPECIES, AIRPORT, AIRCRAFT, NR_INJURIES, NR_FATALITIES) %>%
      filter(!is.na(NR_INJURIES)
             | !is.na(NR_FATALITIES))
       
      injuries_df <- casualties_df %>%
        filter(!is.na(NR_INJURIES))
         
      injuries <- sum(injuries_df$NR_INJURIES)
         
      fatalities_df <- casualties_df %>%
        filter(!is.na(NR_FATALITIES))
         
      fatalities <- sum(fatalities_df$NR_FATALITIES)
      
  # STATE BIRDS
    
    statebirds <- NWSDall %>%
      select(STATE, SPECIES) %>%
      filter((STATE == "AL" & SPECIES == "Northern flicker")
             | (STATE == "AL" & SPECIES == "Wild turkey")
             | (STATE == "AK" & SPECIES == "Willow ptarmigan")
             | (STATE == "AZ" & SPECIES == "Cactus wren")
             | (STATE == "AK" & SPECIES == "Northern mockingbird")
             | (STATE == "CA" & SPECIES == "California quail")
             | (STATE == "CA" & SPECIES == "New World quail")
             | (STATE == "CO" & SPECIES == "Lark bunting")
             | (STATE == "CT" & SPECIES == "American robin")
             | (STATE == "DC" & SPECIES == "Wood thrush")
             | (STATE == "FL" & SPECIES == "Northern mockingbird")
             | (STATE == "GA" & SPECIES == "Brown thrasher")
             | (STATE == "GA" & SPECIES == "Northern bobwhite")
             | (STATE == "GU" & SPECIES == "Guam rail")
             | (STATE == "HI" & SPECIES == "Nene")
             | (STATE == "HI" & SPECIES == "Hawaiian goose")
             | (STATE == "ID" & SPECIES == "Mountain bluebird")
             | (STATE == "IL" & SPECIES == "Northern cardinal")
             | (STATE == "ID" & SPECIES == "Northern cardinal")
             | (STATE == "ID" & SPECIES == "Peregrine falcon")
             | (STATE == "IA" & SPECIES == "American goldfinch")
             | (STATE == "KS" & SPECIES == "Western meadowlark")
             | (STATE == "KY" & SPECIES == "Northern cardinal")
             | (STATE == "LA" & SPECIES == "Brown pelican")
             | (STATE == "ME" & SPECIES %in% "chickadee")
             | (STATE == "MD" & SPECIES == "Baltimore oriole")
             | (STATE == "MA" & SPECIES == "Black-capped chickadee")
             | (STATE == "MA" & SPECIES == "Wild turkey")
             | (STATE == "MI" & SPECIES == "American robin")
             | (STATE == "MN" & SPECIES == "Common loon")
             | (STATE == "MS" & SPECIES == "Northern mockingbird")
             | (STATE == "MS" & SPECIES == "Wood duck")
             | (STATE == "MO" & SPECIES == "Eastern bluebird")
             | (STATE == "AL" & SPECIES == "Northern bobwhite")
             | (STATE == "MT" & SPECIES == "Western meadowlark")
             | (STATE == "NE" & SPECIES == "Western meadowlark")
             | (STATE == "NV" & SPECIES == "Mountain bluebird")
             | (STATE == "NH" & SPECIES == "Purple finch")
             | (STATE == "NH" & SPECIES == "Red-tailed hawk")
             | (STATE == "NJ" & SPECIES == "American goldfinch")
             | (STATE == "NM" & SPECIES == "Greater roadrunner")
             | (STATE == "NY" & SPECIES == "Eastern bluebird")
             | (STATE == "NC" & SPECIES == "Northern cardinal")
             | (STATE == "ND" & SPECIES == "Western meadowlark")
             | (STATE == "MP" & SPECIES == "Mariana fruit-dove")
             | (STATE == "OH" & SPECIES == "Northern cardinal")
             | (STATE == "OK" & SPECIES == "Scissor-tailed flycatcher")
             | (STATE == "OK" & SPECIES == "Wild turkey")
             | (STATE == "OR" & SPECIES == "Western meadowlark")
             | (STATE == "OR" & SPECIES == "Osprey")
             | (STATE == "PA" & SPECIES == "Ruffed grouse")
             | (STATE == "PR" & SPECIES == "Puerto Rican spindalis")
             | (STATE == "RI" & SPECIES %in% "chicken")
             | (STATE == "SC" & SPECIES == "Carolina wren")
             | (STATE == "SC" & SPECIES == "Wild turkey")
             | (STATE == "SC" & SPECIES == "Wood duck")
             | (STATE == "SD" & SPECIES == "Ring-necked pheasant")
             | (STATE == "TN" & SPECIES == "Northern mockingbird")
             | (STATE == "TN" & SPECIES == "Northern bobwhite")
             | (STATE == "TX" & SPECIES == "Northern mockingbird")
             | (STATE == "UT" & SPECIES == "California gull")
             | (STATE == "VT" & SPECIES == "Hermit thrush")
             | (STATE == "VI" & SPECIES == "Bananaquit")
             | (STATE == "VA" & SPECIES == "Northern cardinal")
             | (STATE == "WA" & SPECIES == "American goldfinch")
             | (STATE == "WV" & SPECIES == "Northern cardinal")
             | (STATE == "WI" & SPECIES == "American robin")
             | (STATE == "WY" & SPECIES == "Western meadowlark")
             ) %>%
      group_by(STATE, SPECIES) %>%
      summarize(count = n()) %>%
      mutate(STATE = recode(STATE,
                            "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
                            "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut",
                            "DE" = "Delaware",
                            "FL" = "Florida",
                            "GA" = "Georgia", "GU" = "Guam",
                            "HI" = "Hawaii",
                            "ID" = "Idaho",
                            "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
                            "KS" = "Kansas", "KY" = "Kentucky",
                            "LA" = "Louisiana",
                            "ME" = "Maine", "MD" = "Maryland", "MA" = "Massachusetts", "MI" = "Michigan",
                              "MN" = "Minnesota", "MP" =  "Northern Mariana Islands", "MS" = "Mississippi",
                              "MO" = "Missouri", "MT" = "Montana",
                            "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
                              "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota",
                            "OH" = "Ohio", "OK" = "Oklahoma", "OR" = "Oregon",
                            "PA" = "Pennsylvania", "PR" = "Puerto Rico",
                            "RI" = "Rhode Island",
                            "SC" = "South Carolina", "SD" = "South Dakota",
                            "TN" = "Tennessee", "TX" = "Texas",
                            "UT" = "Utah",
                            "VI" = "U.S. Virgin Islands", "VT" = "Vermont", "VA" = "Virginia",
                            "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming"),
             SPECIES = case_when(STATE == "Alabama" & SPECIES == "Wild turkey" ~ "Wild turkey (state game bird)",
                                 STATE == "California" & SPECIES == "New World quail" ~ "quail sp., presumed California",
                                 STATE == "Georgia" & SPECIES == "Northern bobwhite" ~ "Northern bobwhite (state game bird)",
                                 STATE == "Idaho" & SPECIES == "Peregrine falcon" ~ "Peregrine falcon (state raptor)",
                                 STATE == "Massachusetts" & SPECIES == "Wild turkey" ~ "Wild turkey (state game bird)",
                                 STATE == "Mississippi" & SPECIES == "Wood duck" ~ "Wood duck (state waterfowl)",
                                 STATE == "Missouri" & SPECIES == "Northen bobwhite" ~ "Northern bobwhite (state game bird)",
                                 STATE == "New Hampshire" & SPECIES == "Red-tailed hawk" ~ "Red-tailed hawk (state raptor)",
                                 STATE == "Oklahoma" & SPECIES == "Wild turkey" ~ "Wild turkey (state game bird)",
                                 STATE == "Oregon" & SPECIES == "Western meadowlark" ~ "Western meadowlark (state songbird)",
                                 STATE == "Oregon" & SPECIES == "Osprey" ~ "Osprey (state raptor)",
                                 STATE == "Pennsylvania" & SPECIES == "Ruffed grouse" ~ "Ruffed grouse (state game bird)",
                                 STATE == "South Carolina" & SPECIES == "Wild turkey" ~ "Wild turkey (state wild game bird)",
                                 STATE == "South Carolina" & SPECIES == "Wood duck" ~ "Wood duck (state duck)",
                                 STATE == "Tennessee" & SPECIES == "Northern bobwhite" ~ "Northern bobwhite (state game bird)",
                                 TRUE ~ SPECIES)) %>%
      dplyr::rename(State = STATE,
             Bird = SPECIES,
             Count = count) %>%
      arrange(desc(Count))
    
    # write.csv(statebirds, "statebirds.csv")
    
    statebirdscount <- n_distinct(statebirds$State)
    
  # BANDED
    
    banded_df <- NWSDall %>%
      select(DATE, STATE, HEIGHT, AIRPORT_ID, AIRPORT, OPERATOR, SPECIES, BIRD_BAND_NUMBER) %>%
      filter(grepl("[0-9]", BIRD_BAND_NUMBER)) %>%
      arrange(DATE)
    
    bandedcount <- nrow(banded_df)
    bandedspcount <- n_distinct(banded_df$SPECIES)
    
    banded <- banded_df %>%
      group_by(SPECIES) %>%
      summarize(count = n()) %>%
      arrange(desc(count))