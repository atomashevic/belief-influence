# Filename: 01-data-clean.R
# This script: 
#   - reduces `data` dataframe to variables of interest
#   - converts factor variables to numeric
#   - changes column names to human-readable format            

voi <- c('psppsgva','psppipla','trstprl', 'trstplt',
         'trstprt',
         'trstlgl','trstplc','stfeco',
         'stfgov',
         'stfdem','stfedu',
         'stfhlth','frprtpl','gvintcz',
         'poltran','gndr','agea',
         'cntry','rlgblg','eduyrs','class8','class5')

data <- data %>% select(voi)
data[c(
  'psppsgva',
  'psppipla',
  'trstprl',
  'trstlgl',
  'trstplc',
  'trstplt',
  'trstprt',
  'stfeco',
  'stfgov',
  'stfdem',
  'stfedu',
  'stfhlth',
  'frprtpl',
  'gvintcz',
  'poltran'
)] <- factorsNumeric(data[c(
  'psppsgva',
  'psppipla',
  'trstprl',
  'trstlgl',
  'trstplc',
  'trstplt',
  'trstprt',
  'stfeco',
  'stfgov',
  'stfdem',
  'stfedu',
  'stfhlth',
  'frprtpl',
  'gvintcz',
  'poltran'
)])

colnames(data) <-c('PeopleAllow','PeopleInfluence','Parliament',
                   'Legal','Police','Politicians','Parties','Economy',
                   'Government',
                   'Democracy','Education',
                   'Health','FairChance','CitInterest',
                   'Transparent','Gender','Age','Country','ReligionBelong','Education','Class8','Class5')
