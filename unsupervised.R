library(plyr)
library(dplyr)
library(ggplot2)
library(factoextra)
library(ggthemes)
library(ggcorrplot)
library(GGally)
library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)
library(pheatmap)

# RS_225	Existence of a road safety lead agency
# RS_228	Existence of a national road safety strategy
# RS_229	Availability of funding for national road safety strategy

safety_agency<- read.csv("WHO Road/institutional-framework/data/RS_225.csv") %>% 
  filter(TimeDim == "2016") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>%
  rename(safety = Value) 
safety_strategy <- read.csv("WHO Road/institutional-framework/data/RS_228.csv") %>% 
  filter(TimeDim == "2016") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>%
  rename(strategy = Value)
funding_safety<- read.csv("WHO Road/institutional-framework/data/RS_229.csv") %>% 
  filter(TimeDim == "2016")

funding_safety$funding <- revalue(funding_safety$Value, c("−" = "0", "Partially funded" = "1", "Fully funded" = "2", "Not funded" = "0"))
funding_safety$funding <- as.integer(funding_safety$funding) 
funding_safety <- funding_safety %>% select(c(SpatialDimensionValueCode,funding))
data <- safety_agency %>%
  merge(safety_strategy, all = TRUE) %>%
  merge(funding_safety, all = TRUE)

# RS_204	Existence of a national drink-driving law
# RS_205	Definition of drink-driving by BAC
# RS_207	Blood Alcohol Concentration (BAC) limit for drivers
# RS_208	Attribution of road traffic deaths to alcohol (%)
# RS_209	Existence of a national seat-belt law
# RS_212	Seat-belt wearing rate (%)
# RS_213	Existence of a national child-restraint law
# RS_214	Existence of national speed limits
# RS_217	Maximum speed limits
# RS_219	Applicability of national motorcycle helmet law to all occupants
# RS_223_BIS	Law requires helmet to be fastened

drink_law <- read.csv("WHO Road/national-legislation/data/RS_204.csv") %>%
  select(c(SpatialDimensionValueCode,Value)) %>%
  rename(drink_law = Value)
bac <- read.csv("WHO Road/national-legislation/data/RS_205.csv") %>%
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(bac = Value)


bac_limit <- read.csv("WHO Road/national-legislation/data/RS_207.csv") %>%
  mutate(Value = gsub(" |g/dl|<|=", "", Value)) %>% 
  mutate(Value = if_else(Value == "-", NA_character_, Value)) %>% 
  select(c(SpatialDimensionValueCode,Value,DisaggregatingDimension1ValueCode,Comments))

bac_general <- bac_limit %>% filter(DisaggregatingDimension1ValueCode == "RS-DRIVERTYPE-GENERAL") %>% select(c(SpatialDimensionValueCode,Value,Comments)) %>% 
  mutate(Value = ifelse(SpatialDimensionValueCode == "CAN", "0.08", 
                        ifelse(SpatialDimensionValueCode == "VNM", "0.00", Value))) %>% 
  rename(general_bac = Value) %>% 
  select(c(SpatialDimensionValueCode,general_bac))
bac_young <- bac_limit %>% filter(DisaggregatingDimension1ValueCode == "RS-DRIVERTYPE-YOUNG") %>% select(c(SpatialDimensionValueCode,Value,Comments)) %>%                                                                                      
  mutate(Value = ifelse(SpatialDimensionValueCode == "USA", "0.02", 
                            ifelse(SpatialDimensionValueCode == "VNM", "0.00", ifelse(SpatialDimensionValueCode == "CAN", "0.00" ,Value)))) %>% 
  rename(young_bac = Value) %>% 
  select(c(SpatialDimensionValueCode,young_bac))

data <- data %>%  
  merge(bac_general, all = TRUE) %>% 
  merge(bac_young, all = TRUE) %>% 
  merge(drink_law, all = TRUE)

perc_alcohol <- read.csv("WHO Road/national-legislation/data/RS_208.csv") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  mutate(Value = ifelse(Value == "–", NA_character_, Value)) %>% 
  mutate(Value = ifelse(SpatialDimensionValueCode == "ESP", "11.85", 
                        ifelse(SpatialDimensionValueCode == "GBR", "15", ifelse(SpatialDimensionValueCode == "BLR", "14.3" ,Value)))) %>% 
  mutate(Value = ifelse(grepl("-", Value), 
                        sapply(strsplit(Value, "-"), function(x) mean(as.numeric(x))),
                        Value)) %>% 
  rename(perc_alcohol = Value)
  

seat_law <- read.csv("WHO Road/national-legislation/data/RS_209.csv") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(seat_law = Value)
data <- data %>%  
  merge(seat_law, all = TRUE)

seat_wear <- read.csv("WHO Road/national-legislation/data/RS_212.csv") %>% 
  select(c(SpatialDimensionValueCode,Value,DisaggregatingDimension1ValueCode))

seat_driver <- seat_wear %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-DRIVERSONLY") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(seat_driver = Value)

seat_front <- seat_wear %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-FRONTSEAT") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(seat_front = Value)

seat_rear <- seat_wear %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-REARSEAT") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(seat_rear = Value)

seat_all <- seat_wear %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-ALLOCCUPANTS") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(seat_all = Value)

## A lot of null values
# data <- data %>% 
#   merge(perc_alcohol, all = TRUE) %>% 
#   merge(seat_driver, all = TRUE) %>% 
#   merge(seat_front, all = TRUE) %>% 
#   merge(seat_rear, all = TRUE) %>% 
#   merge(seat_all, all = TRUE)

restraint_law <- read.csv("WHO Road/national-legislation/data/RS_213.csv") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(restraint_law = Value)
speed_limits <- read.csv("WHO Road/national-legislation/data/RS_214.csv") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(speed_limits = Value)
max_speed <- read.csv("WHO Road/national-legislation/data/RS_217.csv") %>% 
  select(c(SpatialDimensionValueCode,Value,DisaggregatingDimension1ValueCode)) %>% 
  mutate(Value = gsub(" |km/h|\\?|No", "", Value))
speed_rural <- max_speed %>% filter(DisaggregatingDimension1ValueCode == "RUR") %>% 
  mutate(Value = ifelse(grepl("-", Value), 
                        sapply(strsplit(Value, "-"), function(x) mean(as.numeric(x))),
                        Value)) %>% 
  rename(speed_rural = Value) %>% 
  select(c(SpatialDimensionValueCode,speed_rural))
speed_urban <- max_speed %>% filter(DisaggregatingDimension1ValueCode == "URB") %>% 
  mutate(Value = ifelse(grepl("-", Value), 
                 sapply(strsplit(Value, "-"), function(x) mean(as.numeric(x))),
                 Value)) %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(speed_urban = Value)

data <- data %>%  
  merge(restraint_law, all = TRUE) %>% 
  merge(speed_limits, all = TRUE) %>% 
  merge(speed_rural, all = TRUE) %>% 
  merge(speed_urban, all = TRUE)

helmet_all <-  read.csv("WHO Road/national-legislation/data/RS_219.csv") %>% 
  select(c(SpatialDimensionValueCode,Value,DisaggregatingDimension1ValueCode))
helmet_child <- helmet_all %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-CHILD-PASSAGER") %>% 
  mutate(Value = ifelse(grepl("Prohibited", Value), "Yes",  ifelse(Value == "Not restricted", "No", Value))) %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(helmet_child = Value) 
helmet_adult <- helmet_all %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-ADULT-PASSAGER") %>% 
  mutate(Value = ifelse(Value == "—", NA_character_, Value)) %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(helmet_adult = Value) 
helmet_driver <-  helmet_all %>% filter(DisaggregatingDimension1ValueCode == "RS-DDC-DRIVERS") %>% 
  mutate(Value = ifelse(Value == "—", NA_character_, Value)) %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(helmet_driver = Value)

helmet_fasten <- read.csv("WHO Road/national-legislation/data/RS_223_BIS.csv") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  mutate(Value = ifelse(Value == "—", NA_character_, Value)) %>% 
  rename(helmet_fasten = Value)

data <- data %>% 
  merge(helmet_child, all = TRUE) %>% 
  merge(helmet_adult, all = TRUE) %>% 
  merge(helmet_driver, all = TRUE) %>% 
  merge(helmet_fasten, all = TRUE)


#RS_242	Vehicle standards

vehicle_std <- read.csv("WHO Road/policy/data/RS_242.csv") %>% 
  select(c(SpatialDimensionValueCode,Value,DisaggregatingDimension1ValueCode)) %>% 
  mutate(Value = ifelse(Value == "YES", "Yes", ifelse(Value == "No ", "No", Value)))
code_vehicle <- read.csv("WHO Road/policy/codes/VEHICLESTANDARD.csv")
std_child_seats <- vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_CHILD_SEATS") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_child_seats = Value) 
std_esc <- vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_ESC") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_esc = Value)
std_front <- vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_FRONTAL_IMPACT") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_front = Value)
std_moto <- vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_MOTORCYCLE") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_moto = Value)
std_pedestrian <- vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_PEDESTRIANPROTECTION") %>%
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_pedestrian = Value)
std_belt <-vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_SEAT_BELTS") %>%
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_belt = Value)
std_belt_anc <- vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_SEAT_BELTS_ANCHORAGES") %>%
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_belt_anc = Value)
std_side <-  vehicle_std %>% filter(DisaggregatingDimension1ValueCode == "RS_VS_SIDE_IMPACT") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(std_side = Value)

data <- data %>% 
  merge(std_child_seats, all = TRUE) %>% 
  merge(std_esc, all = TRUE) %>% 
  merge(std_front, all = TRUE) %>% 
  merge(std_moto, all = TRUE) %>% 
  merge(std_pedestrian, all = TRUE) %>% 
  merge(std_belt, all = TRUE) %>% 
  merge(std_belt_anc, all = TRUE) %>% 
  merge(std_side, all = TRUE)

#Existence of a universal access telephone number for pre-hospital care
universal_number <- read.csv("WHO Road/post-crash-response/data/RS_238.csv") %>% 
  select(c(SpatialDimensionValueCode,Value)) %>% 
  rename(emergency_number = Value)
universal_number$emergency_number <- revalue(universal_number$emergency_number, c("-" = "0","None" = "0", "Partial coverage" = "1", "National, single number" = "2", "National, multiple numbers" = "3")) 
  

data <- data %>% merge(universal_number)


# RS_196	Estimated number of road traffic deaths
# RS_198	Estimated road traffic death rate (per 100 000 population)
# RS_246	Distribution of road traffic deaths by type of road user (%)

#deaths <- read.csv("WHO Road/road-traffic-mortality/data/RS_196.csv")
death_rate <- read.csv("WHO Road/road-traffic-mortality/data/RS_198.csv") %>% 
  filter(TimeDim == 2021) %>% 
  select(c(SpatialDimensionValueCode,NumericValue)) %>%  #,DisaggregatingDimension1ValueCode)) 
  rename(death = NumericValue)

##Not in the new data from 2021 (only in the old -> 2016)
# death_rate_m <- death_rate %>% filter(DisaggregatingDimension1ValueCode == "MLE") %>% 
#   select(c(SpatialDimensionValueCode,NumericValue)) %>% 
#   rename(death_m = NumericValue)
# death_rate_f <- death_rate %>% filter(DisaggregatingDimension1ValueCode == "FMLE") %>% 
#   select(c(SpatialDimensionValueCode,NumericValue)) %>% 
#   rename(death_f = NumericValue)
# death_rate_t <- death_rate %>% filter(DisaggregatingDimension1ValueCode == "BTSX") %>% 
#   select(c(SpatialDimensionValueCode,NumericValue)) %>% 
#   rename(death_t = NumericValue)


death_user <- read.csv("WHO Road/road-traffic-mortality/data/RS_246.csv") %>% 
  filter(TimeDim == 2016) %>% 
  select(c(SpatialDimensionValueCode,NumericValue,DisaggregatingDimension1ValueCode)) 
death_2 <- death_user %>% filter(DisaggregatingDimension1ValueCode == "ROADUSERTYPE_RS-DDC-2OR3WHEELS") %>% 
  select(c(SpatialDimensionValueCode, NumericValue)) %>% 
  rename(death_2 = NumericValue)
death_4 <- death_user %>% filter(DisaggregatingDimension1ValueCode == "ROADUSERTYPE_RS-DDC-4WHEELS") %>% 
  select(c(SpatialDimensionValueCode, NumericValue)) %>% 
  rename(death_4 = NumericValue)
death_cy <- death_user %>% filter(DisaggregatingDimension1ValueCode == "ROADUSERTYPE_RS-DDC-CYCLISTS") %>% 
  select(c(SpatialDimensionValueCode, NumericValue)) %>% 
  rename(death_cy = NumericValue)
death_ped <- death_user %>% filter(DisaggregatingDimension1ValueCode == "ROADUSERTYPE_RS-DDC-PEDESTRIANS") %>% 
  select(c(SpatialDimensionValueCode, NumericValue)) %>% 
  rename(death_ped = NumericValue)
death_oth <- death_user %>% filter(DisaggregatingDimension1ValueCode == "ROADUSERTYPE_RS-DDC-OTHER") %>% 
  select(c(SpatialDimensionValueCode, NumericValue)) %>% 
  rename(death_oth = NumericValue)

data <- data %>% 
  merge(death_2, all = TRUE) %>% 
  merge(death_4, all = TRUE) %>% 
  merge(death_cy, all = TRUE) %>% 
  merge(death_oth, all = TRUE) %>% 
  merge(death_ped, all = TRUE) %>% 
  merge(death_rate, all = TRUE)



data <- data %>%
  mutate_if(is.character, tolower) %>%
  mutate_all(~ifelse(. == "yes", 1, ifelse(. == "no", 0, .)))

data <- data %>%
  mutate_if(is.character, ~na_if(., ""))


data$SpatialDimensionValueCode <- toupper(data$SpatialDimensionValueCode)
rownames(data) <- data$SpatialDimensionValueCode
data <- data[,-1]

data <- data %>%
  mutate_all(~ifelse(is.na(.), NA, as.numeric(.)))

#countries with no speed limit -> set max speed to max + 10
data$speed_urban[data$speed_limits == 0] <-  max(data$speed_urban, na.rm = TRUE) + 10
data$speed_rural[data$speed_limits == 0] <- max(data$speed_rural, na.rm = TRUE) + 10
data$general_bac[data$drink_law == 0] <- max(data$general_bac, na.rm = TRUE)
data$young_bac[data$drink_law == 0] <- max(data$young_bac, na.rm = TRUE)


summary(data)


#filter, a lot of null values
data <- data %>% select(-c("drink_law","death_2","death_4","death_cy","death_oth","death_ped","general_bac","young_bac"))

data <- na.omit(data)


corr_matrix <- cor(data)

ggcorrplot(corr_matrix, method = "circle",lab = TRUE, lab_size = 2,colors = c("#6D9EC1", "white", "#E46726"))
#remove helmet driver ( = adult)
#mean of std for car, high correlation
data$std_car <- rowMeans(data[, c(
  'std_child_seats',
  'std_esc',
  'std_front',
  'std_pedestrian',
  'std_belt',
  'std_belt_anc',
  'std_side'
)]) 
data <- data %>% select(-c('std_child_seats',
                           'std_esc',
                           'std_front',
                           'std_pedestrian',
                           'std_belt',
                           'std_belt_anc',
                           'std_side','helmet_driver'))
corr_matrix <- cor(data)
ggcorrplot(corr_matrix, method = "circle",lab = TRUE, lab_size = 2.5,colors = c("#6D9EC1", "white", "#E46726"))

world <- ne_countries(scale = "medium", returnclass = "sf")


world <- merge(world, data, by.x = "iso_a3_eh", by.y = 0, all = TRUE)

ggplot(world) +
  geom_sf(aes(fill = death)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", name = "Deaths") +  
  theme_fivethirtyeight() +
  labs(title = "Heatmap of Deaths")

create_map <- function(data, var, title, color = c("#C1D31B","#F8EF47","#E9C956","#C28615","#C25215") ) {
  ggplot(data) +
    geom_sf(aes(fill = var)) +  
    scale_fill_manual(values = color) +
    labs(title = title) +
    theme_fivethirtyeight()  
}

world$group_urban <- cut(
  world$speed_urban,
  breaks = c(-Inf, 50, 60, 80,100, Inf), 
  labels = c("<50", "[50-60)", "[60-80)", "[80-100)",">=100"),  
  right = FALSE  
)

create_map(world,factor(world$group_urban),"Max speed in urban street")

world$group_rural <- cut(
  world$speed_rural,
  breaks = c(-Inf, 60, 80, 100,120, Inf), 
  labels = c("<60", "[60-80)", "[80-100)", "[100-120)",">=120"),  
  right = FALSE  
)

create_map(world,factor(world$group_rural),"Max speed in rural street")

world$group_std <- cut(
  world$std_car,
  breaks = c(0, 0.5, 0.7, 0.9, Inf), 
  labels = c("Low", "Medium-low", "Medium-high", "High"), 
  right = FALSE 
)
create_map(world,factor(world$group_std),"Standard car", c("#C25215","#E9C956","#F8EF47","#C1D31B"))

numerical_vars <- c( 'speed_rural','speed_urban','death')#,'GDP'  

scaled <- data
scaled[numerical_vars] <- scale(scaled[numerical_vars])
no_death <- scaled %>% select(-c("death"))

res.pca <- prcomp(no_death)
var_explained <- (res.pca$sdev)^2
percent_var_explained <- var_explained / sum(var_explained) * 100
sum(percent_var_explained[1:3])
comp<-as.data.frame(res.pca$x[,1:3])
load <- as.data.frame(res.pca$rotation[,1:3])
comp
load

fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = load[,3],
             gradient.cols = c("#557bb4", "#f98510", "#cb2c2a"),
             repel = TRUE 
)

fviz_pca_ind(res.pca,
             col.ind = comp[,3], 
             gradient.cols = c("#557bb4", "#f98510", "#cb2c2a"),
             labelsize = 2,
             repel = TRUE
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#f98510", 
                col.ind = "#cb2c2a"
)

fviz_nbclust(no_death, kmeans, method = "wss")

show_cluster <- function(data, cluster, title){
  fviz_cluster(list(data=data,cluster=cluster),
               repel = TRUE,labelsize = 8,
  ) +
    scale_color_manual(values = c("#FE4A49","#2AB7CA","#E5E059","#3E8E41","#643173"))+
    theme_fivethirtyeight() +
    ggtitle(title)
}
k <- 5
kmeans_model <- kmeans(no_death, centers = k)
cluster_labels <- kmeans_model$cluster
data$cluster <- cluster_labels

show_cluster(no_death,data$cluster,"K-Means Cluster")

sil <- silhouette(data$cluster, dist(no_death), ordered = FALSE)
row.names(sil) <- row.names(data) 
fviz_silhouette(sil, label = TRUE) +   
  scale_color_manual(values = c("#FE4A49","#2AB7CA","#E5E059","#3E8E41","#643173")) +
  theme_fivethirtyeight() 

pheatmap(
  no_death, 
  scale = "row", 
  clustering_distance_rows = "euclidean",
  fontsize_row = 3,  
  fontsize_col = 8  
)

distance <- dist(no_death)
hclust_ward<- hclust(distance, method = "ward.D2")  
fviz_dend(hclust_ward, k = k, rect = TRUE,  cex = 0.3, labels_track_height = 0.5)  +
  scale_color_manual(values = c("#FE4A49","#2AB7CA","#E5E059","#3E8E41","#643173","black")) +
  theme_fivethirtyeight() +
  ggtitle("Dendogram Cluster (Ward)")
data$hclust_ward <- cutree(hclust_ward, k = k) 

hclust_comp <- hclust(distance, method = "complete")
fviz_dend(hclust_comp, k = k, rect = TRUE,  cex = 0.3, labels_track_height = 0.5) +
  scale_color_manual(values = c("#FE4A49","#2AB7CA","#E5E059","#3E8E41","#643173","black")) +
  theme_fivethirtyeight() +
  ggtitle("Dendogram Cluster (Complete)")
data$hclust_comp <- cutree(hclust_comp, k = k) 


show_cluster(no_death,data$hclust_ward,"Hierachical Cluster (Ward)")

show_cluster(no_death,data$hclust_comp,"Hierachical Cluster (Complete)")

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- merge(world, data, by.x = "iso_a3_eh", by.y = 0, all = TRUE)

create_map(world,factor(world$cluster), "K-means clustering map",c("#FE4A49","#2AB7CA","#E5E059","#3E8E41","#643173"))
create_map(world,factor(world$hclust_ward), "Hierarchical (Ward) clustering map",c("#FE4A49","#2AB7CA","#E5E059","#3E8E41","#643173"))
create_map(world,factor(world$hclust_comp), "Hierarchical (Complete) clustering map")

data$cluster <- as.factor(data$cluster)
data$hclust_ward <- as.factor(data$hclust_ward)
data$hclust_comp <- as.factor(data$hclust_comp)

create_boxplot <- function(data, var, title) {
  plot_ly(
    data = data,
    x = as.formula(paste("~", var)),  
    y = as.formula(paste("~", "death")), 
    type = 'box'  
  ) %>%
    add_trace(
      type = 'scatter',
      mode = 'markers',
      x = as.formula(paste("~", var)), 
      y = as.formula(paste("~", "death")), 
      text = ~paste("Country:", geounit), 
      marker = list(size = 6, color = 'red') 
    ) %>%
    layout(
      title = title,  
      xaxis = list(title = "Cluster"), 
      yaxis = list(title = "Deaths for 100.000 inhabitants" )  
    )
}
countries <- world %>% select("iso_a3_eh","geounit")
data <- world <- merge(countries, data, by.x = "iso_a3_eh", by.y = 0, all.y = TRUE)
create_boxplot(data,"cluster","Boxplot: deaths for 100.000 inhabitants (K-Means)")
create_boxplot(data,"hclust_ward","Boxplot: deaths for 100.000 inhabitants (Hierarchical Ward)")
create_boxplot(data,"hclust_comp","Boxplot: deaths for 100.000 inhabitants (Hierarchical Complete)")



scatter_vars <- c("funding","seat_law","speed_rural", "speed_urban","helmet_fasten", "std_car", "std_moto", "death", "cluster")

scatter_data <- data[scatter_vars]

ggpairs(scatter_data,
        mapping = aes(color = cluster), 
        diag = list(continuous = "densityDiag"), 
        upper = list(continuous = "cor"), 
        lower = list(continuous = "points")  
) +
  theme_fivethirtyeight() + 
  ggtitle("Scatterplot matrix with cluster as color") 

