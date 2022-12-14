library(tidyverse)
library(stringr)
library(writexl)
df <- read_csv('TDdownload.csv')



alcoholpattern <- '(Cambridge Flower|[Cc]oors|Dft|Lager|Erath|O\'Douls|BEO|Pabst Blue Ribbon|Bombolini|Dom Pin Grig|[Bb][Ee][Ee][Rr]|[Ii][Pp][Aa]|[Hh]eineken|Corona|Truly|Twisted|[Aa]le|SPIKED|Stella|Exhibit\ A\ Cat\'s\ Me|[Bb]ud|[Dd]raft|[Gg]uinness|[Mm]oon|[Jj]ack|Abby|[Ss]mithwicks|[Hh]arpoon|[Yy]uengling|[Ww]icked|[Cc]aptain|[Ll]obo|Stormalong|[Aa]llagash|Rye|[Cc]orona|[Pp]rosecco|[Ss]eltzer|Selzter|Allagash|[Cc]raft|Michelob|[Bb][Tt][Ll]|Barrel|Nevada|[Gg]oose|JW|Claw|Goslings|[Hh]ands|[Aa]llagash|[Aa]mstel|[Oo]rchard|[Bb]allast|[Bb]elleruche|Martini|BABE|[Cc]ourvousier|[Kk]ombucha|[Tt]onic|[Ss]apphire|[Cc]hampionship|Herradora|IPA|Bells|Wachusett|Mich|Pear|Bulleit|Malibu|Coors|Slapshot|Storypoint|Hobo|Boston\ Garden|Miller|Lagunitas|[Gg][Aa][Ss][Cc][Oo][Nn]|[Rr]eyka|[Rr]uffino|Ardbeg|Jim|[Bb]otanist|Monkey|Hayden|Belvedere|[Cc]hivas|Ariel|Dekuyper|Square One|Sterling|Dr\\ McGillicuddy\'s|Ricky|Beantown|Riesling|VSOP|Marnier|Noon|Iced\ Spiced\ Baby|18|Anej|Seagrams|Lagavulin|Artemis|Mezcal\ Vida|Keel|Martell|Redbridge|Blizzard|Blossom|[Kk]ombucha|[Tt]onic|[Pp]uck|[Ll]icor|[Mm][cC]oy|[Gg]lenmorangie|[Mm]artin|Ambler|[Mm]olson|Grey|[Cc]ourovisier|[Kk]arlsson\'s|[Mm]outon|[Aa]ppleton|Lawsons|Craft|Double|Dft|Lager|Pinot|[Bb]eer|PT- Narragansett Shandy|[Ss]eltzer|Corona|Truly|Twisted|[Aa]le|SPIKED|Cabernet|Chardonnay|Sauv|Stella|Malibu|Moscato)'
liquors <- '(Bee\'s\ Knees|wine|[Tt]ai|COCKTAIL|Sngl|Dbl|[Bb]ourbon|Casamigos|[rR]eserve|Sauza|Vodka|G & T|B&B|Daniel\'s|Tito\'s|Pinot|Crown Royal|Whiskey|Liquor|[Cc]ocktail|[Tt]equilla|[Hh]eineken|[Dd]os|[Ii]sland|[Aa]vion|[Mm]argarita|Sauv|[Bb]ear|Chardonnay|Cabernet|[Aa]bsolut|[Dd]ust|[Mm]ary|[Gg]in|[Hh]ennessey|[Ww]HITNEY|Whitney|Jagermeister|[Oo]mission|[Mm]eiomi|[Ww][Ii][Nn][Ee]|[Rr]um|Daiquiri|Cincoro|Blanco|[Aa]maretto|[Pp]retty|Gimlet|Jameson|Ketel|Macallan|Mark|Patron|Fashioned|[Ww]ine|[Cc]hard|[Cc]hart|Rickey|Stoli|Titos|Woodford|Tanqueray|Camarena|Creek|Crimes|Layup|Sam|Bacardi|Tequila|Hennessy|Cuervo|CANYON|Dark|Comfort|Domani|[Tt]erlato|Hendricks|[Pp]ig|[Rr]eposado|[Dd]on|[Cc]raig|[Tt]ullamore|LIQUOR|[Aa]ppleton|[Aa]gave|[Oo]ban|[Pp][Nn]|[Pp]enfolds|[Uu]pgrade|[Cc]isco|[Dd]ublin|[Aa]nejo|[Aa]msterdam|[Ww]alker|Dewars|Kolsch|Magic\ Hat|Bailey\'s|Baileys|Speakeasy|[Ss]tout|Peeper|Cointreau|Kahlua|Kona|[Dd]ecoy|[Ff]at|[Cc]ampo|[Ww]hitn|[Gg]lenlivet|[Cc]oors|[Aa]nejo|[Mm]erlot|[Bb][Ee][Aa][Rr]|[Cc]licquot|Chivas|Caberne|Mumm|Mezcal|Mimosa|Bloody|Midori|Belvedere|Disarono|Svedka|[Jj]agermesiter|[Bb]elvedere|[Bb]ellini|[Cc]oastal|[Cc]oozie|[Cc]anadian|[Mm]outon|[Cc]eltics|[Ss]ixpoint|[Pp]orter|[Bb]acardi|[Cc]hai|[Ww]hirlpo|[Gg]rapefruit|[Ee]clipse|[Ww]hirlpool|[Rr]ickshaw|[Ss]tone|[Tt]riple|[Cc]rystal|[Mm]imosa|[Ss]angria|[Cc]ynar|[Cc]osmo|[Oo]wen\'s|Paloma|[Pp]arquet|[Ff]ounders|[Yy]amazaki|[Bb]ushmills|Lemondrop|Balvine|Chambord|Local\ Cra|Seagram\'s|Karlsson\'s|Pint|Tempus|Komos|Ambler|Atomic|Martin|Glenmorangie|Meow|Long\ Trail|Redbridge|Schanpps|Branca[Mm]odelo|[Cc]ampari|[Ff]rangelico|[Bb]eefeater|[Pp]aloma|[Dd]ekuyper|[Ss]terling|[Mm]cGillicuddy\'s|[Cc]aposaldo|[Kk]ung|VSOP|[Mm]arnier|[Nn]oon|[Ss]piced|[Pp]lantas|[Jj]aegermesiter|Modelo|Forester|Daisey|Seagrams|Fernet|Clementine|Keel|Sngl|Dbl|Sgl|Dulce|Shot|TAVERN|dbl|Dark Red|D\'usse|Belle Meade|Vodka|Bacardi|WhistlePig|Patron|Maker|Jim Beam|Bourbon|Bloody Mary|Steigl Radler|Woodford|Dewars|Disaronno|Double|Finlandia|Glenlivet|Grand Marnier|Grey Goose|Gin|Martini|New Amsterdam|Old Fashioned|Paloma|Cocktail|Kahlua|Jagermeister|Jameson|Casamigos|Jack|Sauza|Sailor Jerry|Skrewball|Southern Mule|Vodka|Daniel\'s|Tito\'s|Titos|Dewar|Diskin|Crown Royal|Whiskey|Liquor|Ole Smoky)'
NAbevpattern <- '(Hot Chocolate|Golden Road|[aA]quafina|[Gg]atorade|[Mm]atcha|Coco|Shirley|[Ww][Aa][Tt][Ee][Rr]|[M]ilk|[Cc]oke|[Ss]prite|Milkshake|Milksha|N/A Beverage Package ED|Dr\\. Pepper|Fanta|Chocolate Milk|[Mm]ellow|[Mm]onster|[Ff]ountain|[Ss][Oo][Dd][Aa]|[Tt]ea|[Pp]unch|[cC]offee|(Icee)|(ICEE)|[Ee]nergy|[Dd]asani|[Dd]rink|[Pp]owerade|[Ll]emonade|[Cc]ider|[Aa]mericano|[Cc]appuccino|[Cc]afe|[Bb]rew|[Ee]spresso|Cintron|Mule|Coca|[Pp]owerad|Sweet\ &\ Sour|Pellegrino|Lattee|[jJ]uice|Sangria|O\'Douls|BEO|[Mm]ountain Dew|SG\ Flavor|[Pp]epsi|Bubly Cherry|Hot Beverage|[Ww]ater|[Cc]oke|[Ss]prite|Core Power|Smoothie|Dr\\. Pepper|Fanta|[Mm]ellow|[Mm]onster|[Ss]oda|[Tt]ea|[Pp]unch|[cC]offee|Icee|ICEE|[Ee]nergy|[Dd]rink|[Pp]owerade|[Ll]emonade|Milk|Hot Chocolate|Body Armor|Juice)'
ozpattern <- '[\\d\\.]+[Oo][Zz]|[\\d\\.]+ [Oo][Zz]'
mlpattern <- '[\\d\\.]+[Mm][Ll]|[\\d\\.]+ [Mm][Ll]'
retailpattern <- '[Yy]es|YES'
foodpattern <- '[Cc]hips|[dD]oritos|[Pp]retzel|[Pp]opcorn|[Cc]heetos|[Cc]hicken|[Tt]urkey|[Ss]teak|[Ff]ries|[Ss]andwich|Cheeto|Lays|Lay\'s'


bottledpattern <- '[Bb]ottle|[aA]quafina'
packagedpattern <- '[Pp]ackage|[Pp][Kk][Gg]'
canpattern <- '[Cc]an'
cocktailpattern <- 'Shot|Dulce|[Bb]ottle|Cocktail|LL|Vodka|Rita|Smoky|Casamigos|Gin|Avion Margarita|Bloody Mary|Apple of my Rye|Raspberry Lime Ricky|Bourbon Cider|[Mm]argarita'
draftpattern <- '[Dd]raft|[Dd]ft|[Bb]eer|Diskin|Jacalope'
seltzerpattern <- 'Truly|SPIKED|Spiked|White Claw|[sS]eltzer'
domesticpattern <- 'Domestic|PBR|Bud|Miller|Coors|Michelob|Mich|Yuengling|Rolling Rock|Busch'
craftpattern <- 'IPA|[Aa]le|[Ll]ager|Blue Moon|Heineken|Sierra Nevada|Shiner Bock|Fat Tire|Amstel|Sam Adam|Yeehaw|[Ii]PA|[Aa]le|[Ll]ager|Blue Moon|Heineken|Sierra Nevada|Shiner Bock|Fat Tire|Amstel|Sam Adam|Yeehaw|Large Goose Island|Blood Orange Wheat|Jacks Abby|Jack\'s Abby'
winepattern <- 'Wine|Cabernet|Dark Red|Chard|Shiraz|Blend|Sauv|Pinot|Prosecco|Rose|Riesling|Moscato|Merlot'


df1 <- df %>% 
  mutate(
    `Product - Cat 0` = case_when(
      str_detect(Retail_x,retailpattern) ~ 'Retail',
      source_system == 'netsuite' ~ 'Retail',
      str_detect(item_name,'Koozie|[Dd]onation|DONATION') ~ 'Retail',
      str_detect(item_name,foodpattern) ~ 'Food',
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

df1 %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 0`))

df1 %>% ggplot() +
  stat_summary(mapping=aes(x=`Product - Cat 0`,y=amount),stat="mean",geom="bar")


#### Add Product Cat 1 ####
alcohol_df <- df1[df1$`Product - Cat 0`=="Alcohol",]
na_df <- df1[df1$`Product - Cat 0`=="NA Beverage",]
food_df <- df1[df1$`Product - Cat 0`=="Food",]
retail_df <- df1[df1$`Product - Cat 0`=="Retail",]






adf <- alcohol_df %>%
  mutate(
    `Product - Cat 1` = case_when(
      str_detect(item_name,packagedpattern) ~ 'Packaged',
      str_detect(item_name,alcoholpattern) ~ 'Draft',
      str_detect(item_name,draftpattern) ~ 'Draft',
      str_detect(item_name,liquors) ~ 'Non-Beer',
      str_detect(item_name,bottledpattern) ~ 'Bottled',
      str_detect(item_name,canpattern) ~ 'Bottled',
      TRUE ~ 'Bottled'))


adf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 1`))


#top products
adf %>% filter(`Product - Cat 1`=='Bottled') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 1) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()


unqAlc<-unique(adf[c("item_name")])


#####NA Beverages catergorization##############
ndf <- na_df %>%
  mutate(
    `Product - Cat 1` = case_when(
      str_detect(item_name,bottledpattern) ~ 'Bottled',
      TRUE ~ 'Fountain'))


#number of transactions
ndf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 1`))


#top products
ndf %>% filter(`Product - Cat 1`=='Fountain') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n > 1) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()

unqNA<-unique(ndf[c("item_name")])


#### FOOD categirization  ####
#Snack, Candy, Entr??e, Side, Add-On, Dog/Sausage, Pizza

snackpattern <- 'Ruffles|Cheez It|Nacos|Funyuns|Mints|Nestle|Sticks|Egg Rolls|Cheese|Tots|[Tt]oast|[Bb]un|[Dd]ough|[Dd]ulce|[Rr]ose|[Nn]utella|Bar|Baked OriginalNacos|Bacon|Yummies|Protein Bar|Snack|Oreo|Flipz|[Nn]acho|Muffin|[Cc]hip|CHIP|[Pp]eanuts|COOKIES|[Pp]opcorn|Pretzel|Dorito|[Cc]heetos|Cheez-it|Ming\'s Bings|Gourmet Nut Mix|Ruffles|Cheez It|501 Club|Chili|Gardettos|Cheeto|Lay\'s|Waffle|Corn|Snack|Cheez It|Baked Original|Yummies|Protein Bar|Snack|Oreo|Flipz|[Nn]acho|Muffin|[Cc]hip|CHIP|[Pp]eanuts|[Pp]opcorn|Pretzel|Corn|Combo|Smartfood|Dorito|Kind Bar|Trail Mix|Chex Mix'
candypattern <- '[Pp]remium [Bb]ar|Nestle|NestleRolo|Mints|[Tt]wix|Yummies|Nestle|Mings|Kit Kat|Starburst|Swedish Fish|Twizzler|M&M|Snickers|Sour Patch|Reese|Rice Krispies|Candy|Rolo|Honey|Gum|Chcolate|Tumie Yummies|Life Saver|Airheads|mints|Slushee|SweetTarts|Kit Kat|Haribo|Raisinet|Hershey|Starburst|Mike & Ike|Swedish Fish|Twizzler|M&M|Snickers|Gummy Bears|Skittles|Sour Patch|Reese|Rice Krispies|Candy'
entreepattern <- 'Shishito|[Pp]anini|[Ff][Oo][Oo][Dd]|Appetizer Special|Spring Roll|[Nn]oodle|[Bb]uffet|Shrimp Skewer|Royal Red Shrimp Scampi|[Ff]luffernutter|[Pp]ortobello|[Pp]uttanesca|[Aa]ffogato|[Mm]elt|[Mm]ELT|[Bb]jurger|Huevos Rancheros|Eggs|Ribs|Meatloaf|Cheese|Nashville Tender|[Ee]ntree|Brookie|APPETIZER|Starter|Burrata|Pancake|Wrap|Kid|BURGER|Sandwhich|Lofts|Rafters|Meatball|[Ss][Aa][Nn][Dd][Ww][Ii][Cc][Hh]|Patty Melt|[cC]alamari|[Ss]ub|Hoagie|Salad|Mac & Cheese|Mac and Cheese|[Cc]hicken|[Pp]ork|[Bb]risket|[Gg]rilled|[Tt]aco|BBQ|[Bb]urger|Reuben|[Ww]ing|[Bb]urrito|Hog|[Ff]latbread|[Cc]lub|[Bb]oneless|Firecracker Shrimp|Roast|Boardroom Food|[Tt]una|[Pp]asta|Atlantic Cod|[Cc]obb|[Ss]alm|[qQ]uesadilla|[dD]umpling|[Bb]eef|[oO]ysters|Hummus Platter|[sS]ushi|[cC]rudite|[oO]melet|STEAK|[cC]hurro|[Ff]ish|FISH|[Ll]amb|[lL]asagna|[lL]obster|York Street Veggie Tray|Jalapeno Poppers|Pancake|Food|Beefeater|Hashbrowns|Slab|Pumpkinhead|Flatbread|Mushroom|Sub|Fry|Combo|Wrap|Kid|Sandwich|Patty Melt|Hoagie|Salad|Mac & Cheese|[Cc]hicken|[Pp]ork|[Bb]risket|[Gg]rilled|[Tt]aco|BBQ|[Bb]urger|Reuben|[Ww]ing|[Bb]urrito|Hog|[Ff]latbread'
dogpattern <- '[Dd]og|[Ss]ausage|[Bb]ratwurst|DOG|Kk]osher|Hog'
sidepattern <- 'Veggie|Jerky|Side|Biscuit|Bagel|Fruit|[Cc]repe|[Ff]rench [Ff]ry|[Ff]ries|Cheese Sticks|Spinach Dip|[Ss]oup|Mashed Potato|[Cc]howder|Lemon Wedges|[Ww]edges|Salsa Trio|Thai Brussels|side of cheese|Jerky|Side|Sides|Dip|Biscuit|Bagel|Spread|Egg Rolls|Fruit|[Ff]rench [Ff]ry|[Ff]ries|Cheese Sticks|Spinach Dip|[Ss]oup|Mashed Potato'
dessertpattern <- 'Creme Brule|Big Alaska|Dibs|[Cc]repe|[Cc]risp|[Tt]art|[Dd][Ee][Ss][Ss][Ee][Rr][Tt]|Tiramisu|CC|Danish|Ice Cream|Cone|Dots|Brownie|Miss Vickie|Cookie|[Cc]ake|Creme Brulee|[gG]elato|[dD]essert|Chocolate Strawberries|[Ss]coop|CC|Danish|Ice Cream|Muffins|Breakfast|Cone|Dots|Brownie|Miss Vickie|Cookie'
addonpattern <- 'X Cheese|Add Queso|Add on|Add Bacon|add ice cream|Beverage Package Add On|Add Guacamole|Add Shrimp|X Cheese|Add Queso|Add on|Add Bacon|Add [Oo]n|Add'
pizzapattern <- '[Pp]izza|[Ss]lice'

fdf <- food_df %>%
  mutate(
    `Product - Cat 1` = case_when(
      str_detect(item_name,snackpattern) ~ 'Snack',
      str_detect(item_name,candypattern) ~ 'Candy',
      str_detect(item_name,entreepattern) ~ 'Entree',
      str_detect(item_name,dogpattern) ~ 'Dog/Sausage',
      str_detect(item_name,pizzapattern) ~ 'Pizza',
      str_detect(item_name,sidepattern) ~ 'Side',
      str_detect(item_name,dessertpattern) ~ 'Dessert',
      str_detect(item_name,addonpattern) ~ 'Addon',
      TRUE ~ 'others'))


fdf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 1`))

#top products
fdf %>% filter(`Product - Cat 1`=='others') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n >10) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()


otherFood <- fdf[fdf$`Product - Cat 1`=="others",]
unqOf<-unique(otherFood[c("item_name")])

#rdf <- tdf[tdf$ProductCat0=="Retail",]
rdf <- retail_df %>%
  mutate(
    'Product - Cat 1' = case_when(
      TRUE ~ 'Retail'))

rdf <- rdf %>%
  mutate(
    'Product - Cat 2' = case_when(
      TRUE ~ 'Retail'))


waterpattern <- '[Ww][Aa][Tt][Ee][Rr]|[Dd]asani|Pellegrino|BEO WATER|[aA]quafina'
sodapattern <- '[Ss][Oo][Dd][Aa]|[Cc]oke|[Ss]prite|[Cc]oke|[Ss]prite|Shirley|BEO SODA|N/A Beverage Package ED|Fanta|[Mm]ellow|[Ll]emonade|Minute Maid|Punch|Icee|ICEE|Frozen|[Ff]ountain|[Mm]ountain Dew'
energypattern <- '[Mm]onster|[Ee]nergy|[Pp]owerade|[Dd]rink|[Pp]owerad|Cintron|Mule'
coffeepattern <- '[Cc]offee|[Cc]appuccin|Lattee|Coca|[Aa]mericano|[Cc]afe|[Bb]rew|[Ee]spresso'
teapattern <- '[Tt]ea|[Mm]atcha|Mule'
hotchocolatepattern <- '[Hh]ot Chocolate|Coco'

ndf <- ndf %>%
  mutate(
    `Product - Cat 2` = case_when(
      str_detect(item_name,waterpattern) ~ 'Water',
      str_detect(item_name,sodapattern) ~ 'Soda',
      str_detect(item_name,energypattern) ~ 'Energy Drink',
      str_detect(item_name,coffeepattern) ~ 'Coffee',
      str_detect(item_name,teapattern) ~ 'Tea',
      str_detect(item_name,hotchocolatepattern) ~ 'Hot Chocolate',
      #str_detect(item_name,nonsodapattern) ~ 'Non-Soda',
      TRUE ~ 'Non-Soda'))

ndf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 2`))


#### Food Categorization 2 ####

chipspattern<- 'Ruffles|Cheez-it|Nacos|Nacho|Nachos|Funyuns|[Cc]hip|CHIP|Baked OriginalNacos|[Cc]heetos|Dorito|[Ll]ays|BBQ|Dips|[Cc]heeto\'s|Lay\'s'
sandwichpattern <-'[Tt]oast|Reuben|[Bb]un|[Dd]ough|[Cc]lub|Sanwiches|[Bb]eef|[qQ]uesadilla|STEAK|Bacon|Ming\'s Bings|[Tt]una|[Bb]risket|[Mm]elt|Hoagie|Patty Melt|[Pp]ork|[Mm]ELT|Wrap|Sandwhich|[Ss][Aa][Nn][Dd][Ww][Ii][Cc][Hh]'
popcornpattern <- '[Pp]opcorn'
wrappattern <- '[Ww]rap|[Bb]urrito'
spreadpattern <- '[Dd]ulce|Spread|[Rr]ose|[Nn]utella|Hummus'
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
saladpattern <- '[Ss]alad|[Gg]reen|Veg|Broccolini|[Cc]obb|Hummus Platter|Fruit|Vegetable|Fruit Cup|Caesar|cruditeYork Street Veggie Tray|Tray|[Ww]edges|Salsa Trio'
appetizerpattern <- 'Meal|Artisan|Mashed|Burrata|Appetizer Special|[Bb]aked|[Cc]heese|Spring Roll|[Tt]aco|Meatball|Egg Rolls|[dD]umpling|[sS]ushi|[Ff][Oo][Oo][Dd]|[oO]melet|Thai Brussels|[Ff]ish|FISH|[lL]obster|[Rr]oasted|[Ll]amb|[oO]ysters|Atlantic|[Ff]irecracker Shrimp|Boardroom Food|[Bb]urger|APPETIZER|Starter|Pancake|Shrimp Skewer|Chicken FingersBurrata|Royal Red Shrimp Scampi|[Ff]luffernutter|[Pp]ortobello|[Pp]uttanesca|[Aa]ffogato|Huevos Rancheros|Eggs|Ribs|Meatloaf|[Ee]ntree'
pastapattern <- 'Mac and Cheese|[Pp]asta|Mac & Cheese|[lL]asagna|Chowder|Macaroni & Cheese'
souppattern <- '[Ss]oup'
wholepizzapattern <- '[Pp]izza'
slicepizzapattern <-'[Ss]lice'



fdf <- fdf %>%
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
      str_detect(item_name,dessertpattern) ~ fdf$item_name,
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
      str_detect(item_name,appetizerpattern) ~ fdf$item_name,
      str_detect(item_name,pastapattern) ~ 'Pasta',
      str_detect(item_name,souppattern) ~ 'Soup',
      str_detect(item_name,popcornpattern) ~ 'Popcorn',
      TRUE ~ 'others'))



fdf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 2`))

#top products
fdf %>% filter(`Product - Cat 2`=='others') %>%
  drop_na(item_name) %>%
  group_by(item_name) %>%
  count() %>%
  filter(n >1) %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(item_name,n),y=n)) +
  coord_flip()


otherFood <- fdf[fdf$`Product - Cat 2`=="others",]




adf <- adf %>%
  mutate(
    `Product - Cat 2` = case_when(
      str_detect(item_name,domesticpattern) ~ 'Domestic',
      str_detect(item_name,winepattern) ~ 'Wine',
      str_detect(item_name,mlpattern) ~ 'Wine',
      str_detect(item_name,seltzerpattern) ~ 'Seltzer',
      str_detect(item_name,craftpattern) ~ 'Craft',
      str_detect(item_name,cocktailpattern) ~ adf$item_name,
      adf$Price>150 ~ "Premium",
      TRUE ~ 'Domestic'))


naAlc <- adf[adf$`Product - Cat 2`=="Domestic",]
unqAlc<-unique(naAlc[c("item_name")])



#number of transactions
adf %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 2`
  )) + coord_flip()



largepattern <- '[Ll][Aa][Rr][Gg][Ee]|[Ll]g|[Ll][Gg][Rr]'
mediumpattern <-'[Mm][Ee][Dd][Ii][Uu][Mm]'
smallpattern <- '[Ss][Mm][Aa][Ll][Ll]'
souvenirpattern <- '[Ss]ouvenir'
singlepattern <- '[Ss]ngl|[Ss]gl'
doublepattern <-'[Dd][Bb][Ll]|Double'

final_df <- rbind(fdf,adf,ndf,rdf,otherFood)


final_df <- final_df %>%
  mutate(
    `Product - Cat 3` = case_when(
      str_detect(item_name,largepattern) ~ 'Large',
      str_detect(item_name,mediumpattern) ~ 'Medium',
      str_detect(item_name,smallpattern) ~ 'Small',
      str_detect(item_name,souvenirpattern) ~ 'Souvenir',
      str_detect(item_name,ozpattern) ~ str_extract(final_df$item_name,"[\\d\\.]+[Oo][Zz]|[\\d\\.]+ [Oo][Zz]"),
      str_detect(item_name,mlpattern) ~ str_extract(final_df$item_name,"[\\d\\.]+[Mm][Ll]|[\\d\\.]+ [Mm][Ll]"),
      TRUE ~ 'Regular'))








final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '12Oz' | final_df$`Product - Cat 3` == '12oz' | final_df$`Product - Cat 3` == '12 oz'] <- '12 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '9Oz' | final_df$`Product - Cat 3` == '9oz' | final_df$`Product - Cat 3` == '9 oz'] <- '9 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '6Oz' | final_df$`Product - Cat 3` == '6oz' | final_df$`Product - Cat 3` == '6 oz'] <- '6 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '16 Oz' | final_df$`Product - Cat 3` == '16OZ' |final_df$`Product - Cat 3` =='16Oz' | final_df$`Product - Cat 3` == '16oz' | final_df$`Product - Cat 3` == '16 oz'] <- '16 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '25 Oz' | final_df$`Product - Cat 3` == '25OZ' |final_df$`Product - Cat 3` == '25Oz' | final_df$`Product - Cat 3` == '25oz' | final_df$`Product - Cat 3` == '25 oz'] <- '25 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '24 Oz' | final_df$`Product - Cat 3` == '24OZ' |final_df$`Product - Cat 3` == '24Oz' | final_df$`Product - Cat 3` == '24oz' | final_df$`Product - Cat 3` == '24 oz'] <- '24 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '20 Oz' | final_df$`Product - Cat 3` == '20OZ' |final_df$`Product - Cat 3` == '20Oz' | final_df$`Product - Cat 3` == '20oz' | final_df$`Product - Cat 3` == '20 oz'] <- '20 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '19.2 Oz' | final_df$`Product - Cat 3` == '19.2OZ' |final_df$`Product - Cat 3` == '19.2Oz' | final_df$`Product - Cat 3` == '19.2oz' | final_df$`Product - Cat 3` == '19.2 oz'] <- '19.2 Ounces'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '14.9 Oz' | final_df$`Product - Cat 3` == '14.9OZ' |final_df$`Product - Cat 3` == '14.9Oz' | final_df$`Product - Cat 3` == '14.9oz' | final_df$`Product - Cat 3` == '14.9 oz'] <- '14.9 Ounces'

final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '500ML' | final_df$`Product - Cat 3` == '500ml' | final_df$`Product - Cat 3` == '500 ML'] <- '500 ml'
final_df$`Product - Cat 3`[final_df$`Product - Cat 3` == '375ML' | final_df$`Product - Cat 3` == '375ml' | final_df$`Product - Cat 3` == '375 ML'] <- '375 ml'

final_df %>% ggplot() +
  geom_bar(mapping=aes(x=`Product - Cat 3`
  )) + coord_flip()

write.csv(final_df, 'TDFinal.csv')  
