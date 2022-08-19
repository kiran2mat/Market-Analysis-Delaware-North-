library(tidyverse)
library(stringr)
bridgestonedf <- read_csv('bridgestonejoined2.csv')

#pattern <- '([\\d\\.]+oz|Sngl|Dbl|Dft|Daniel|Tito\'s)'
alcoholpattern <- '(Craft|Double|Dft|Lager|Pinot|[Bb]eer|PT- Narragansett Shandy|[Ss]eltzer|Corona|Truly|Twisted|[Aa]le|SPIKED|Cabernet|Chardonnay|Sauv|Stella|Malibu|Moscato)'
liquors <- '(Sngl|Dbl|Sgl|Dulce|Shot|TAVERN|dbl|Dark Red|D\'usse|Belle Meade|Vodka|Bacardi|WhistlePig|Patron|Maker|Jim Beam|Bourbon|Bloody Mary|Steigl Radler|Woodford|Dewars|Disaronno|Double|Finlandia|Glenlivet|Grand Marnier|Grey Goose|Gin|Martini|New Amsterdam|Old Fashioned|Paloma|Cocktail|Kahlua|Jagermeister|Jameson|Casamigos|Jack|Sauza|Sailor Jerry|Skrewball|Southern Mule|Vodka|Daniel\'s|Tito\'s|Titos|Dewar|Diskin|Crown Royal|Whiskey|Liquor|Ole Smoky)'
Foodpattern <-'Cheeto|Lays|Lay\'s'
NAbevpattern <- '([Ww]ater|[Cc]oke|[Ss]prite|Core Power|Smoothie|Dr\\. Pepper|Fanta|[Mm]ellow|[Mm]onster|[Ss]oda|[Tt]ea|[Pp]unch|[cC]offee|Icee|ICEE|[Ee]nergy|[Dd]rink|[Pp]owerade|[Ll]emonade|Milk|Hot Chocolate|Body Armor|Juice)'
ozpattern <- '[\\d\\.]+[Oo][Zz]|[\\d\\.]+ [Oo][Zz]'

bottledpattern <- '[Bb]ottle'
packagedpattern <- '[Pp]ackage|[Pp]kg'
canpattern <- '[Cc]an'
cocktailpattern <- 'Shot|Dulce|[Bb]ottle|Cocktail|LL|Vodka|Rita|Smoky|Casamigos|Gin'
draftpattern <- '[Dd]raft|[Dd]ft|[Bb]eer|Diskin|Jacalope'
seltzerpattern <- 'Truly|SPIKED|Spiked|White Claw'
domesticpattern <- 'Domestic|PBR|Bud|Miller|Coors|Michelob|Mich|Yuengling|Rolling Rock|Busch'
craftpattern <- 'IPA|[Aa]le|[Ll]ager|Blue Moon|Heineken|Sierra Nevada|Shiner Bock|Fat Tire|Amstel|Sam Adam|Yeehaw'
winepattern <- 'Wine|Cabernet|Dark Red|Chard|Shiraz|Blend|Sauv|Pinot|Prosecco|Rose|Riesling|Moscato|Merlot'

bdf <- bridgestonedf %>% 
  mutate(
    `Product - Cat 0` = case_when(
      source_system == 'netsuite' ~ 'Retail',
      str_detect(item_name,'Koozie|[Dd]onation|DONATION') ~ 'Retail',
      str_detect(item_name,Foodpattern) ~ 'Food',
      str_detect(item_name,liquors) ~ 'Alcohol',
      str_detect(item_name,alcoholpattern) ~ 'Alcohol',
      str_detect(item_name,NAbevpattern) ~ 'NA Beverage',
      str_detect(item_name,seltzerpattern) ~ 'Alcohol',
      str_detect(item_name,domesticpattern) ~ 'Alcohol',
      str_detect(item_name,craftpattern) ~ 'Alcohol',
      str_detect(item_name,winepattern) ~ 'Alcohol',
      str_detect(item_name,ozpattern) ~ 'Alcohol',
      str_detect(item_name,canpattern) ~ 'Alcohol',
      TRUE ~ 'Food')) 

#number of transactions
bdf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 0`))

#average amount
bdf %>% ggplot() +
  stat_summary(mapping=aes(x=`Product - Cat 0`,y=amount),stat="mean",geom="bar")

##############################################################################################################################################################################
##############################################################################################################################################################################
##############################################################################################################################################################################


#### Add Product Cat 1 ####
alcohol_df <- bdf[bdf$`Product - Cat 0`=="Alcohol",]
na_df <- bdf[bdf$`Product - Cat 0`=="NA Beverage",]
food_df <- bdf[bdf$`Product - Cat 0`=="Food",]
retail_df <- bdf[bdf$`Product - Cat 0`=="Retail",]

adf <- alcohol_df %>%
  mutate(
    `Product - Cat 1` = case_when(
      str_detect(item_name,liquors) ~ 'Non-Beer',
      str_detect(item_name,draftpattern) ~ 'Draft',
      str_detect(item_name,bottledpattern) ~ 'Bottled',
      str_detect(item_name,canpattern) ~ 'Packaged',
      TRUE ~ 'Non-Beer'))



########Alcohol cat -2 ####################

adf <- adf %>%
  mutate(
    `Product - Cat 2` = case_when(
      str_detect(item_name,domesticpattern) ~ 'Domestic',
      str_detect(item_name,winepattern) ~ 'Wine',
      str_detect(item_name,seltzerpattern) ~ 'Seltzer',
      #str_detect(item_name,'Sngl|Dbl') ~ trimws(str_match(item_name, ".*(Sngl|Dbl)[ -]*([\\w ']*)")[, 3]), #NOTE: Adding a hypen to the last pattern will go down a level of analysis to specific drinks
      #str_detect(item_name,liquors) ~ trimws(str_match(item_name, ".*PT[ -]*([\\w ']*)")[, 2]), #NOTE: Adding a hypen to the last pattern will go down a level of analysis to specific drinks
      str_detect(item_name,craftpattern) ~ 'Craft',
      str_detect(item_name,cocktailpattern) ~ adf$item_name,
      adf$Price>150 ~ "Premium",
      TRUE ~ 'Domestic'))

#number of transactions
adf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 2`
                       )) + coord_flip()

#top products
adf %>% filter(`Product - Cat 2`=='Domestic') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 100) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()
##############################################################################################################################################################################
##############################################################################################################################################################################

###########NA CATEGORIZATION -1###########

na_df <- na_df %>%
  mutate(
    `Product - Cat 1` = case_when(
      str_detect(item_name,bottledpattern) ~ 'Bottled',
      TRUE ~ 'Fountain'))

#number of transactions
na_df %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 1`))

#top products
na_df %>% filter(`Product - Cat 1`=='Fountain') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 1) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()


####################################################################################################################
####################################################################################################################


#### FOOD sub cat ####
#### Food Categorization 1 ####

#top products
bdf %>% filter(`Product - Cat 0`=='Food') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 100) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()

#Snack, Candy, Entrée, Side, Add-On, Dog/Sausage, Pizza
snackpattern <- 'Ruffles|Cheez It|501 Club|Chili|Gardettos|Cheeto|Lay\'s|Waffle|Corn|Snack|Cheez It|Baked Original|Yummies|Protein Bar|Snack|Oreo|Flipz|[Nn]acho|Muffin|[Cc]hip|CHIP|[Pp]eanuts|[Pp]opcorn|Pretzel|Corn|Combo|Smartfood|Dorito|Kind Bar|Trail Mix|Chex Mix'
candypattern <- 'Rolo|Honey|Gum|Chcolate|Tumie Yummies|Life Saver|Airheads|mints|Slushee|SweetTarts|Kit Kat|Haribo|Raisinet|Hershey|Starburst|Mike & Ike|Swedish Fish|Twizzler|M&M|Snickers|Gummy Bears|Skittles|Sour Patch|Reese|Rice Krispies|Candy'
entreepattern <- 'Pancake|Food|Beefeater|Hashbrowns|Slab|Pumpkinhead|Flatbread|Mushroom|Sub|Fry|Combo|Wrap|Kid|Sandwich|Patty Melt|Hoagie|Salad|Mac & Cheese|[Cc]hicken|[Pp]ork|[Bb]risket|[Gg]rilled|[Tt]aco|BBQ|[Bb]urger|Reuben|[Ww]ing|[Bb]urrito|Hog|[Ff]latbread'
dogpattern <- '[Dd]og|[Ss]ausage|[Bb]ratwurst|Hog'
sidepattern <- 'Jerky|Side|Sides|Dip|Biscuit|Bagel|Spread|Egg Rolls|Fruit|[Ff]rench [Ff]ry|[Ff]ries|Cheese Sticks|Spinach Dip|[Ss]oup|Mashed Potato'
dessertpattern <- 'CC|Danish|Ice Cream|Muffins|Breakfast|Cone|Dots|Brownie|Miss Vickie|Cookie'
addonpattern <- 'X Cheese|Add Queso|Add on|Add Bacon|Add [Oo]n|Add'


food_df <- food_df %>%
  mutate(
    `Product - Cat 1` = case_when(
      str_detect(item_name,snackpattern) ~ 'Snack',
      str_detect(item_name,candypattern) ~ 'Candy',
      str_detect(item_name,entreepattern) ~ 'Entree',
      str_detect(item_name,dogpattern) ~ 'Dog/Sausage',
      str_detect(item_name,'[Pp]izza') ~ 'Pizza',
      str_detect(item_name,sidepattern) ~ 'Side',
      str_detect(item_name,dessertpattern) ~ 'Dessert',
      str_detect(item_name,addonpattern) ~ 'Addon',
       TRUE ~ 'Retail'))

food_df %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 1`))

#top products
food_df %>% filter(`Product - Cat 1`=='NA') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 10) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()
#### Food Categorization 2 ####

chipspattern<- 'Ruffles|Cheez-it|Gardettos|Chili Fritos|Nacos|Tot|Nacho|Nachos|Funyuns|[Cc]hip|CHIP|Baked OriginalNacos|[Cc]heetos|Dorito|[Ll]ays|BBQ|Dips'
sandwichpattern <-'[Tt]oast|Reuben|[Bb]un|501 Club|[Dd]ough|[Cc]lub|Sanwiches|[Bb]eef|[qQ]uesadilla|STEAK|Bacon|Ming\'s Bings|[Tt]una|[Bb]risket|[Mm]elt|Hoagie|Patty Melt|[Pp]ork|[Mm]ELT|Wrap|Sandwhich|[Ss][Aa][Nn][Dd][Ww][Ii][Cc][Hh]'
popcornpattern <- '[Pp]opcorn'
wrappattern <- '[Ww]rap|[Bb]urrito'
spreadpattern <- '[Dd]ulce|Dip|Spread|[Rr]ose|[Nn]utella|Hummus'
pretzelpattern<-'Sticks|Pretzel|Bagel|Cheese Sticks'
peanutspattern <-'[Pp]eanuts'
friespatterns <- 'Tots|Rafters|[Ff]ries|Calamari|Jalapeno Poppers'
barspattern <- 'Protein Bar|Bar'
cookiespattern <- 'Oreo|Cookies|Brookie|[cC]hurro|Biscuit'
combopatterns <- 'piece'
tenderpatterns <- 'Chicken Fingers|Tenders|[Ll]ofts|[Cc]hicken|[Ww]ing|Bb]oneless|Tender'
nutspattern <- 'Gourmet Nut Mix|Mix|Gourmet'
paninipattern<- '[Pp]anini'
burgerpattern <- 'Shishito|[Bb]jurger|[Bb]urger|BURGER'
saladpattern <- '[Ss]alad|[Gg]reen|Caprese|Veg|Chili|Broccolini|[Cc]obb|Hummus Platter|Fruit|Vegetable|Fruit Cup|Caesar|cruditeYork Street Veggie Tray|Tray|[Ww]edges|Salsa Trio'
appetizerpattern <- 'Meal|Pie|Cranberry|Mezze|Steak|Dot Cup|Shark|Potatoes|Pineapple|Turkey|Kosher|Clam Strips|Edamame|Roll|Rice and Beans|Artisan|Mashed|Deli|Califlower|Pies|Burrata|Appetizer Special|[Bb]aked|[Cc]heese|Spring Roll|[Tt]aco|Meatball|Egg Rolls|[dD]umpling|[sS]ushi|[Ff][Oo][Oo][Dd]|[oO]melet|Thai Brussels|[Ff]ish|FISH|[lL]obster|[Rr]oasted|[Ll]amb|[oO]ysters|Atlantic|[Ff]irecracker Shrimp|Boardroom Food|[Bb]urger|APPETIZER|Starter|Pancake|Shrimp Skewer|Chicken FingersBurrata|Royal Red Shrimp Scampi|[Ff]luffernutter|[Pp]ortobello|[Pp]uttanesca|[Aa]ffogato|Huevos Rancheros|Eggs|Ribs|Meatloaf|[Ee]ntree'
pastapattern <- 'Mac and Cheese|Sliders|[Pp]asta|Mac & Cheese|[lL]asagna|Chowder|Macaroni & Cheese'
souppattern <- '[Ss]oup'
wholepizzapattern <- '[Pp]izza'
slicepizzapattern <-'[Ss]lice'


food_df <- food_df %>%
  mutate(
    `Product - Cat 2` = case_when(
      str_detect(item_name,chipspattern) ~ 'Chips',
      str_detect(item_name,candypattern) ~ 'Chocolate',
      str_detect(item_name,sandwichpattern) ~ 'Sandwich',
      str_detect(item_name,wrappattern) ~ 'Wrap',
      str_detect(item_name,dogpattern) ~ 'Hot Dog',
      str_detect(item_name,addonpattern) ~ 'Additional(Extras)',
      str_detect(item_name,wholepizzapattern) ~ 'Pizza',
      str_detect(item_name,slicepizzapattern) ~ 'Slice of Pizza',
      str_detect(item_name,spreadpattern) ~ 'Spread',
      str_detect(item_name,dessertpattern) ~ food_df$item_name,
      str_detect(item_name,pretzelpattern) ~ 'Pretzel',
      str_detect(item_name,peanutspattern) ~ 'Peanuts',
      str_detect(item_name,friespatterns) ~ 'Fries',
      str_detect(item_name,barspattern) ~ 'Bars',
      str_detect(item_name,cookiespattern) ~ 'Cookies',
      str_detect(item_name,combopatterns) ~ 'Combos',
      str_detect(item_name,nutspattern) ~ 'Nuts',
      str_detect(item_name,tenderpatterns) ~ 'Tenders',
      str_detect(item_name,paninipattern) ~ 'Panini',
      str_detect(item_name,burgerpattern) ~ 'Burger',
      str_detect(item_name,saladpattern) ~ 'Salad',
      str_detect(item_name,appetizerpattern) ~ food_df$item_name,
      str_detect(item_name,pastapattern) ~ 'Pasta',
      str_detect(item_name,souppattern) ~ 'Soup',
      str_detect(item_name,popcornpattern) ~ 'Popcorn',
      TRUE ~ 'Retail'))


food_df %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 2`))

####################################################################################################################
####################################################################################################################
###########NA CATEGORIZATION -2###########

waterpattern <- '[Ww]ater'
sodapattern <- '[Ss]oda|[Cc]oke|[Ss]prite|Dr\\. Pepper|Fanta|[Mm]ellow|[Ll]emonade|Minute Maid|Punch|Icee|ICEE|Frozen'
energypattern <- '[Mm]onster|[Ee]nergy|[Pp]owerade'
coffeepattern <- '[Cc]offee'
teapattern <- '[Tt]ea'

na_df <- na_df %>%
  mutate(
    `Product - Cat 2` = case_when(
      str_detect(item_name,waterpattern) ~ 'Water',
      str_detect(item_name,sodapattern) ~ 'Soda',
      str_detect(item_name,energypattern) ~ 'Energy Drink',
      str_detect(item_name,coffeepattern) ~ 'Coffee',
      str_detect(item_name,teapattern) ~ 'Tea',
      TRUE ~ 'Non-Soda'))

na_df %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 2`))

#top products
na_df %>% filter(`Product - Cat 2`=='NA') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 1) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()
####################################################################################################################
####################################################################################################################
####################################################################################################################



finaldf = rbind(adf,na_df,food_df,retail_df)

#### Add Product Cat 3 ####
largepattern <- '[Ll][Aa][Rr][Gg][Ee]|[Ll]g|[Ll][Gg][Rr]'
mediumpattern <-'[Mm][Ee][Dd][Ii][Uu][Mm]'
smallpattern <- '[Ss][Mm][Aa][Ll][Ll]'
souvenirpattern <- '[Ss]ouvenir'
singlepattern <- '[Ss]ngl|[Ss]gl'
doublepattern <-'[Dd][Bb][Ll]|Double'
finaldf <- finaldf %>%
  mutate(
    `Product - Cat 3` = case_when(
      str_detect(item_name,ozpattern) ~ str_extract(finaldf$item_name,"[\\d\\.]+[Oo][Zz]|[\\d\\.]+ [Oo][Zz]"),
      #str_detect(item_name,mlpattern) ~ str_extract(finaldf$item_name,"[\\d\\.]+[Mm][Ll]|[\\d\\.]+ [Mm][Ll]"),
      str_detect(item_name,souvenirpattern) ~ 'Souvenir',
      str_detect(item_name,singlepattern) ~ 'Single',
      str_detect(item_name,doublepattern) ~ 'Double',
      TRUE ~ 'Regular'))

#number of transactions
finaldf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 3`))


write_csv(finaldf,'bridgestonecategorizationfl12.csv')

