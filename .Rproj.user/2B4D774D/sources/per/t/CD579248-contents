factorsNumeric <-  function(d)
  modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

load('data-clean/data-oesch.Rds')


voi <- c('psppsgva','psppipla','trstprl', 'trstplt',
         'trstprt',
         'trstlgl','trstplc','stfeco',
         'stfgov','stfdem','stfedu',
         'stfhlth','frprtpl','gvintcz',
         'poltran','gndr','agea',
         'cntry','rlgblg','eduyrs','class8','class5')

data <- data_clean %>% select(voi)
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
                   'Government','Democracy','Education',
                   'Health','FairChance','CitInterest',
                   'Transparent','Gender','Age','Country','ReligionBelong','Education','Class8','Class5')

save(data,file='data-clean/data-clean.Rds')
