source('code/data-wrangling.R')
countries <- levels(data$Country)
n = length(levels(data$Country))

country_results <- matrix(0,n,12)
  
for (i in 1:n)
{
  country = countries[i]
  c = get_country_results(data,network_variables,country,type='summary')
  country_results[i,] = as.matrix(c)
}

results = as.data.frame(country_results)
colnames(results) <- c("Temperature","GIC Mean","GIC SD","GIC Kurtosis","GI Mean","GI SD","GI Kurtosis", "GSM Mean ","GSM SD","GSM Kurtosis","Avg. Energy")
saveRDS(results,'data/country-results.Rds')

country_influence <- as.data.frame(matrix(0,n,8))
measure = "gics"
for (i in 1:n)
{
  country = countries[i]
  c = get_country_results(data,network_variables,country,type='inf')
  c = as.data.frame(c[measure,])
  country_influence[i,] = cbind(country,c)
}

colnames(country_influence) <- c("Country",colnames(network_data))
saveRDS(country_influence,'data/country-gics.Rds')

country_influence <- as.data.frame(matrix(0,n,8))
measure = "gsms"
for (i in 1:n)
{
  country = countries[i]
  c = get_country_results(data,network_variables,country,type='inf')
  c = as.data.frame(c[measure,])
  country_influence[i,] = cbind(country,c)
}
colnames(country_influence) <- c("Country",colnames(network_data))
saveRDS(country_influence,'data/country-gsms.Rds')


