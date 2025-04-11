# data downloaded from the FAA National Wildlife Strike Database
  # wildlife.faa.gov

# setup

  setwd("/Volumes/SANDISK 1TB/code/wildlife strikes")
  
  # install.packages("tidyverse")
  # install.packages("Rmisc")
  
  library(tidyverse)
  library(dplyr)

  NWSD <- read_csv("NWSD.csv")

# selected incidents

  selections <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, AIRPORT, STATE, OPERATOR, AIRCRAFT, COST_REPAIRS_INFL_ADJ, SPECIES, REMARKS) %>%
    filter(INDEX_NR %in% c( # mentioned in published version (or late drafts) of Rare Bird piece
                            "633738", "1451347", "1406362", "1542619", "1545248", "1603761", "1533166",
                            "716359", "1575694", "1607611", "1534473", "1534583", "1617275", "1542382",
                            "1580943", "690699", "1039159", "1616951", "1568785", "1568785", "1608565",
                            "1280391", "1309795", "1451111", "1482377", "1612688", "638856", "1489343",
                            "752323", "1087098", "1504308", "721450", "1134028", "1306115", "1371624",
                            "1103031", "935515", "1082506", "1409270", "1604498", "1604500", "1604989",
                            "1410730", "1032685", "1240448", "1487097", "1487097", "1555634", "1098850",
                            "647707", "1434149", "1409474", "698618", "1514798", "1134083", "654131",
                            "1038056", "1033970", "1412850",
                            # from early research for Rare Bird piece
                            "1216284", "759304", "638646", "616927", "973272", "700640", "1169915",
                            "741668", "1096459", "655125", "1437874", "762245", "739153", "623810",
                            "1183321", "692878", "686582", "1133439", "1348980", "643351", "1365573",
                            "1371631", "1467792", "1411890", "1411661", "1471894", "1443137", "1431818",
                            "1436444", "1467866", "1445184", "1411521")
           | SPECIES %in% c(# other interesting species
                            "Moose", "American black bear", "Bearded seal", "Golden eagle", "Fairy tern",
                            "Sprague's pipit", "Least bittern", "Common pauraque"))
  
  write.csv(selections, "selections.csv", row.names = FALSE)

# all 2024 remarks

  remarks24 <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, INCIDENT_YEAR, AIRPORT_ID, SPECIES, REMARKS) %>%
    filter(INCIDENT_YEAR == 2024,
           REMARKS != "")
  
  write.csv(remarks24, "remarks24.csv", row.names = FALSE)

# Bald Eagle
  
  BAEA <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, INCIDENT_YEAR, AIRPORT_ID, AIRCRAFT, SPECIES, REMARKS) %>%
    filter(SPECIES == "Bald eagle",
           REMARKS != "")
  
  write.csv(BAEA, "BAEA.csv", row.names = FALSE)

# government operators

  gov <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, INCIDENT_YEAR, AIRPORT_ID, AIRCRAFT, OPERATOR, SPECIES, REMARKS) %>%
    filter(OPERATOR %in% c("GOVERNMENT", "US COAST GUARD", "FAA", "US CUSTOMS AND BORDER PROTECTION",
                           "US DEPT OF JUSTICE", "ROYAL AUSTRALIAN AIR FORCE"),
           REMARKS != "")
  
  write.csv(gov, "gov.csv", row.names = FALSE)
  
# banded birds
  
  banded <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, STATE, AIRPORT, AIRCRAFT, OPERATOR, SPECIES, BIRD_BAND_NUMBER, REMARKS) %>%
    filter(grepl("[0-9]", BIRD_BAND_NUMBER)) %>%
    dplyr::rename(DATE = INCIDENT_DATE, BAND = BIRD_BAND_NUMBER) %>%
    arrange(DATE)
  
  write.csv(banded, "banded.csv", row.names = FALSE)
  
# ones
  
  ones_df <- NWSD %>%
    group_by(SPECIES) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    filter(count == 1)
  
  ones_list <- as.vector(ones_df$SPECIES)
  
  ones <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, TIME_OF_DAY, STATE, AIRPORT, AIRCRAFT, OPERATOR, SPECIES, REMARKS) %>%
    filter(SPECIES %in% ones_list,
           !is.na(REMARKS))
  
  write.csv(ones, "ones.csv", row.names = FALSE)
  
# non-avian
  
  nonavian_list <- c( # non-avian taxa in 2024
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
  
  nonavian <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, STATE, AIRPORT, AIRCRAFT, OPERATOR, SPECIES, REMARKS) %>%
    filter(SPECIES %in% nonavian_list,
           !is.na(REMARKS))
  
  write.csv(nonavian, "nonavian.csv", row.names = FALSE)
  
# Hawaii
  
  hawaii <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, STATE, AIRPORT, AIRCRAFT, OPERATOR, SPECIES, REMARKS) %>%
    filter(STATE == "HI")
  
  write.csv(hawaii, "hawaii.csv", row.names = FALSE)
  
# foreign
  
  foreign <- NWSD %>%
    select(INDEX_NR, INCIDENT_DATE, STATE, AIRPORT, AIRCRAFT, OPERATOR, SPECIES, REMARKS) %>%
    filter(STATE == "FN")

  write.csv(foreign, "foreign.csv", row.names = FALSE)
  
# costs
  
  costs <- read_csv("NWSD through 20241107.csv") %>%
    select(INDEX_NR, INCIDENT_YEAR, SPECIES, AIRPORT, AIRCRAFT, COST_REPAIRS_INFL_ADJ, COST_OTHER_INFL_ADJ, REMARKS) %>%
    filter(!is.na(COST_REPAIRS_INFL_ADJ) & 
            !is.na(COST_OTHER_INFL_ADJ)) %>%
    arrange(desc(COST_REPAIRS_INFL_ADJ))
  
  write.csv(costs, "costs.csv", row.names = FALSE)
  
# casualties
  
  casualties <- NWSD %>%
    select(INCIDENT_DATE, SPECIES, AIRPORT, AIRCRAFT, NR_INJURIES, NR_FATALITIES, REMARKS) %>%
    filter(!is.na(NR_INJURIES)
           | !is.na(NR_FATALITIES))
  
  write.csv(casualties, "casualties.csv", row.names = FALSE)
  
# night
  
  night <- NWSD %>%
    filter(TIME_OF_DAY == "Night"
           & !grepl("Unknown", SPECIES)
           | !is.na(REMARKS)) %>%
    select(INDEX_NR, INCIDENT_DATE, STATE, AIRPORT, AIRCRAFT, OPERATOR, SPECIES, REMARKS)
  
  write.csv(night, "night.csv", row.names = FALSE)