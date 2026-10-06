# Setup -------------------------------------------------------------------

library(dplyr); library(tidyr); library(ggplot2); library(forcats); 
library(geodata); library(tidyterra); library(terra); library(ggpubr)


# Pull data frame from csv -------------------------------------------------

files = list.files(
  path = "input/",
  pattern = "*.csv"
)

df <- lapply(paste0("input/",files), read.csv)
df <- as.data.frame(df) %>% 
  rename(Sr_ratio = 'X87Sr86Sr', 
         Sr_ratio_error = 'X87Sr86Sr_error_1SD')


# Simplify material type etc ----------------------------------------------
# let's simplify some naming schema and classifications

df <- df %>% 
  mutate(material_type_simp = case_when(
    material_type == 'inorganic-organic composite : inorganic material : dry deposition' ~ 'dry deposition', 
    material_type == 'inorganic-organic composite : rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : bulk rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : rock mineral separate' ~ 'rock mineral', 
    material_type == 'inorganic-organic composite : sediment' ~ 'sediment', 
    material_type == 'inorganic-organic composite : sediment : sediment mineral' ~ 'sediment', 
    material_type == 'inorganic-organic composite : soil' ~ 'soil', 
    material_type == 'inorganic-organic composite : soil : soil mineral' ~ 'soil mineral', 
    material_type == 'inorganic-organic composite : water : total water' ~ 'water', 
    material_type == 'inorganic-organic composite : water : water particulate' ~ 'water particulate', 
    material_type == 'inorganic-organic composite : water : water solute' ~ 'water solute', 
    material_type == 'organic composite : lichen' ~ 'lichen', 
    material_type == 'organic composite : litter' ~ 'litter', 
    material_type == 'organism : animal : animal tissue' ~ 'animal, undefined tissue', 
    material_type == 'organism : animal : animal tissue : antler' ~ 'antler', 
    material_type == 'organism : animal : animal tissue : bone' ~ 'bone', 
    material_type == 'organism : animal : animal tissue : egg : shell' ~ 'eggshell', 
    material_type == 'organism : animal : animal tissue : excreta' ~ 'excreta', 
    material_type == 'organism : animal : animal tissue : feather' ~ 'feather', 
    material_type == 'organism : animal : animal tissue : hair' ~ 'hair', 
    material_type == 'organism : animal : animal tissue : otolith' ~ 'otolith', 
    material_type == 'organism : animal : animal tissue : shell' ~ 'shell', 
    material_type == 'organism : animal : animal tissue : tooth' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : dentin' ~ 'dentin', 
    material_type == 'organism : animal : animal tissue : tooth : dentin' ~ 'dentin',
    material_type == 'organism : animal : animal tissue : tooth : enamel' ~ 'enamel', 
    material_type == 'organism : animal : animal tissue : tusk' ~ 'tusk', 
    material_type == 'organism : animal : whole animal' ~ 'whole animal', 
    material_type == 'organism : plant : plant tissue' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : unknown' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : bark' ~ 'plant bark', 
    material_type == 'organism : plant : plant tissue : blade' ~ 'plant blade', 
    material_type == 'organism : plant : plant tissue : flowering body' ~ 'flowering body', 
    material_type == 'organism : plant : plant tissue : leaf' ~ 'leaf', 
    material_type == 'organism : plant : plant tissue : plant fruiting body' ~ 'fruit', 
    material_type == 'organism : plant : plant tissue : root' ~ 'root', 
    material_type == 'organism : plant : plant tissue : seed' ~ 'seed', 
    material_type == 'organism : plant : whole plant' ~ 'whole plant', 
    material_type == 'organism : plant : plant tissue : wood' ~ 'wood'
  )
  )

df <- df %>% 
  mutate(material_type_group = case_when(
    material_type == 'inorganic-organic composite : inorganic material : dry deposition' ~ 'dry deposition', 
    material_type == 'inorganic-organic composite : rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : bulk rock' ~ 'rock', 
    material_type == 'inorganic-organic composite : rock : rock mineral separate' ~ 'rock', 
    material_type == 'inorganic-organic composite : sediment' ~ 'sediment', 
    material_type == 'inorganic-organic composite : sediment : sediment mineral' ~ 'sediment', 
    material_type == 'inorganic-organic composite : soil' ~ 'soil', 
    material_type == 'inorganic-organic composite : soil : soil mineral' ~ 'soil', 
    material_type == 'inorganic-organic composite : water : total water' ~ 'water', 
    material_type == 'inorganic-organic composite : water : water particulate' ~ 'water', 
    material_type == 'inorganic-organic composite : water : water solute' ~ 'water', 
    material_type == 'organic composite : lichen' ~ 'organic other', 
    material_type == 'organic composite : litter' ~ 'organic other', 
    material_type == 'organism : animal : animal tissue' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : antler' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : bone' ~ 'bone', 
    material_type == 'organism : animal : animal tissue : egg : shell' ~ 'animal, other', 
    material_type == 'organism : animal : excreta' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : feather' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : hair' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : otolith' ~ 'otolith', 
    material_type == 'organism : animal : animal tissue : shell' ~ 'animal, other', 
    material_type == 'organism : animal : animal tissue : tooth' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : dentin' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tooth : enamel' ~ 'tooth', 
    material_type == 'organism : animal : animal tissue : tusk' ~ 'tooth', 
    material_type == 'organism : animal : whole animal' ~ 'animal, other', 
    material_type == 'organism : plant : plant tissue' ~ 'plant',
    material_type == 'organism : plant : whole plant' ~ 'plant',
    material_type == 'organism : plant : plant tissue : unknown' ~'plant',
    material_type == 'organism : plant : plant tissue : bark' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : blade' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : flowering body' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : leaf' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : plant fruiting body' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : root' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : seed' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : whole plant' ~ 'plant', 
    material_type == 'organism : plant : plant tissue : wood' ~ 'plant'
  ))

animaldf <- subset(df, material_type_group %in% c("animal, other", "bone", "tooth", "otolith"))

animaldf %>%
  ggplot(aes(x = fct_infreq(material_type_group))) +
  geom_bar() +
  labs(x = "group") + 
  theme_classic()


# Identifying migration ranges --------------------------------------------

# migratory or home ranges 100+ across
large <- c("Acrocephalus schoenobaenus", "Alces alces", "Antilocapra americana", 
           "Bison", "Bison bison", "Canis lupus", "H. molitrix", "Leopardus pardalis",
           "Cervus", "Cervus canadensis", "Cervus elaphus", "Cervus elaphus L.", 
           "Charadrius hiaticula", "Ctenopharyngodon idella", "Damaliscus dorcas dorcas bontebok", 
           "Damaliscus lunatus", "Equid sp.", "Equus", "Equus sp.", "Equus cabalus",
           "Alosa sapidissima", "Calidris alpina", "Damaliscus pygargus", "Dendroica caerulescens", 
           "Elephantidae", "Esox", "Esox lucius", "Gazella subgutturosa", "Haematopus", 
           "Kobus kob", "Lota lota", "Mammuthus", "Numenius", "Numenius phaeopus", 
           "Odocoileus", "Odocoileus virginianus", "Oncorhynchus tshawytscha", 
           "Pluvialis apricaria", "Puma", "Rangifer", "Rangifer tarandus", 
           "Salminus brasiliensis", "Salmo salar", "Sander lucioperca", "Sander vitreus", 
           "Setophaga caerulescens", "Syncerus caffer caffer", "Tachycineta bicolor", 
           "Tringa totanus", "Vanellinae", "Kobus ellipsiprymnus", "M. primigenius", 
           "Phoca groenlandica", "Pusa hispida", "R. tarandus", "Raphicerus campestris steenbok",
           "Rhinocerotidae", "Ursus arctos"
)

# migratory or home ranges between 6-99 km across
medium <- c("Aepyceros melampus", "Alligator sinensis", "Antidorcas marsupialis", "Aplodinotus grunniens",
            "Arapaima spp", "Erethizontidae", "Erinaceinae", "Erinaceus europaeus", "Erinaceus europeus",
            "Archaeolemur", "Archaeolemur edwardsi", "Archaeolemur majori", "Arianta arbustorum", 
            "Avahi laniger", "Babakotia radofilai", "Bos", "Castor fiber",
            "Bos taurus", "Bos taurus L.", "Bovidae", "Caiman yacare", "Capra",
            "Capra aegagrus hircus", "Capreolus", "Capreolus capreolus", "Capreolus capreolus L.", 
            "Caprinae", "Caprini", "Castor canadensis", "Cathartidae", "Cervidae", 
            "Cercopithecus ascanius", "Cercopithecus mitis", "Cheirogaleus crossleyi", 
            "Cingulata", "Crocuta crocuta", "Cuniculus", "Cuniculus paca",
            "Cynomys", "Atherinosoma microstoma", "Brachyplatystoma rousseauxii",
            "Cerdocyon thous", "Cyprinus carpio", "Dasypodidae sp.", "Dasyprocta punctata",
            "Diceros bicornis", "Dasypus novemcinctus", "Didelphis", "Didelphis marsupialis",
            "Dicotyles tajacu", "Equidae", "Eulemur rubriventer", "Eulemur rufifrons", 
            "Felis", "Hare Lepus timidus/L. europaeus P.", "Hystrix", "Ichneumia albicauda",
            "Hexaprotodon guldbergi", "Hippopotamus amphibius", "Hippopotamus lemerlei", "Hippotragus niger", 
            "Iguanidae", "Lagomorpha", "Lama vicugna", "Lemur catta", "Leopardus sp.", 
            "Leopardus wiedii", "Lepilemur", "Lepilemur petteri", "Leporidae", "Lepus", 
            "Lepus americanus", "Lepus saxatilis", "Lomo guanicoe", "M. agilis", 
            "M. giganteus", "M. piceus", "M. rufus", "Martes martes L.", "Megaladapis madagascariensis", 
            "Meleagris gallopavo", "Microcebus griseorufus", "Micropterus dolomieu", 
            "Micropterus salmoides", "Mustela frenata", "Mustelidae", "Myocastor coipus", 
            "Oreochromis niloticus", "Ovis", "Ovis aries L.", "Pan troglodytes", 
            "Panthera leo", "Papio", "Papio anubis", "Papio ursinus", "Pecari tajacu", 
            "Phacochoerus africanus", "Phascolarctos cinereus", "Pomoxis nigromaculatus", 
            "Procyon lotor", "Propithecus coquereli", "Propithecus verreauxi", 
            "Raphicerus campestris", "Redunca arundinum",  
            "Scolopacidae", "Serpentes", "Sylvicapra grimmia", "Sylvilagus", 
            "Sylvilagus cunicularius", "Tapirella bairdii", "Tapiridae", "Tapirus terrestris", 
            "Tayassu pecari", "Tayassuidae", "Tragelaphus scriptus", "Vulpes vulpes", 
            "Vulpes vulpes L.", "Leptus saxatilis", "Lepus sp.", "Lepus timidus", "Lepus townsendii", 
            "Lutra lutra", "Mazama", "Mazama sp.", "Meles meles", "Mephitis mephitis", 
            "Microcebus rufus", "Mustela erminea", "Mustela nivalis", "Rabbit", 
            "Struthioniformes"
)
#	Castor fiber L, Eurasian beaver, is an aquatic species to that might affect things.
# small home ranges/migration patterns, 5km^2 or less
small <- c("Acomys cahirinus", "Akodon sp.", "Anomalocardia brasiliana", "Anthozoa",
           "Anura", "Apodemus flavicollis", "Arvicola amphibius", "Arvicola terrestris", "Bivalvia", 
           "Blattodea", "Bufo", "Bufonidae", "Bulimulidae", "Lemmus trimucronatus",
           "Cavia", "Cavia porcellus", "Cepaea hortensis", "Cepaea nemoralis", "Chilostoma sp.",  
           "Clausiliidae", "Corbicula sp.", "Cornu aspersum", "Cricetidae", "Crocidura", 
           "Crocidura flavescens deltae", "Cryptomys hottentotus", "Dicrostonyx groenlandicus", "Dicrostonyx groenlandicus rubricatus",
           "Ctenomys sp.", "Arvicolinae", "Eligmodontia sp.", "Eliurus majori", "Eliurus",
           "Eliurus minor", "Erinaceus europ.", "Fruticicolidae", "Galea sp.", "Gastropoda", 
           "Geomyidae", "Geomys bursarius", "Gerbilliscus brantsii", "Gyraulus convexiusculus", 
           "Helix aspersa", "Helix pomatia", "Helix sp.", "Marmota xaviventris", "Helicidae",
           "Meriones sp.", "Micaelamyus namaquensis", "Microtus arvalis/agrestis", 
           "Mollusca", "Mus", "Mus musculus", "Neocyclotus dysoni", "Nesokia indica",
           "Octodon sp.", "Ondatra zibethicus", "Orthalicus princeps", "Orthogeomys hispidus", 
           "Orycteropus afer", "Oryctolagus", "Oryctolagus cuniculus", "Oryctolagus sp.", 
           "Ostrea sp.", "Otocyon megalotis", "Otomys irroratus", "Otomys unisulcatus", 
           "Pachychilus sp.", "Pectinidae", "Pedetes capensis", "Perforatella incarnata",
           "Philander opossum", "Phyllotis sp.", "Pomacea flagellata", "Pomatias elegans", 
           "Procavia capensis", "Pronolagys rupestris", "Psammobates geometricus", 
           "Pupillidae", "Radix xauricularia", "Rhabdomys pumilio", "Sciuridae", 
           "Sigmodon hispidus", "Soricidae", "Spalax ehrenbergi", "Suncus murinus",
           "Talpa sp.", "Tatera indica", "Tenrecidae", "Thomomys talpoides", "Trichia", 
           "Trochoidea hoggarensis", "Xerotricha hoggarensis", "Xerus inauris", 
           "Zaedyus pichiy", "Zootelcus insularis", "Euglandina cylindracea", "Snail", 
           "Macroscelides/Elephaniuius sp. elephant shrew", "Macroscelididae", "Masticophis mentovarius", 
           "Megalonaias nervosa", "Microfauna", "Microgale cowani", "microtus agrestis", 
           "Microtus miurus", "Microtus oeconomus", "Microtus oeconomus macfarlani", 
           "Microtus oeconomus operarius", "Microtus pennsylvanicus", "Microtus xanthognathus", 
           "Mole", "Mouse, species undet.", "Muridae", "mus trimucronatus", "Myodes rutilus", 
           "Myodes rutilus dawsoni", "Neomys fodiens", "Rodentia", "Sciurus vulgaris", 
           "Sorex araneus", "Sorex minutus", "Talpa europeana", "Neotominae"
           # several aquatic species in here actually, such as clams
)

df <- df %>% 
  mutate(migration = case_when(
    scientific_name %in% large ~ 'large', 
    scientific_name %in% medium ~ 'medium', 
    scientific_name %in% small ~ 'small'))

animaldf <- subset(df, material_type_group %in% c("animal, other", "bone", "tooth", "otolith"))

migration_list <- as.data.frame(c(large, medium, small)) %>% 
  rename(scientific_name = 'c(large, medium, small)')

missing_names <- anti_join(animaldf, migration_list) %>% 
  filter(!is.na(scientific_name))
table(missing_names$scientific_name) # what's 'missing' at this point are species I'm ignoring for various reasons
# Some things I'm ignoring because they're too tied with humans: Canis lupus familiaris, Rattus rattus,
# Ovis/Capris/Ares genus (if generic and not a specific wild species), Sus, Lama
# really generic families/orders are also left off (e.g., Reptilia, Rhicocerotidae)
# extinct species where the ranges just aren't necessarily well known ('e.g. archaeolemurs)
# Probably a lot of errors as I'm not a biologist but ah well. 

df <- anti_join(df, missing_names, by = "sample_measurement_id") # lose those 
# ~3k animal samples that are potentially note representative of local biosphere



# Checking for missing data -----------------------------------------------

missing_data <- df %>% summarize(across(everything(), ~sum(is.na(.))))
# can also click on data frame, with RStudio's updated summary table options 

#why are we missing one Sr ratio? a mystery. 
nasr <- filter(df, is.na(Sr_ratio))
