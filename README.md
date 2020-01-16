# Intercity-travel-pattern-in-NYC
ABSTRACT  Smartphone applications (“apps”) providing transit information are commonly used in urban areas. Many of these apps are available in multiple cities and automatically detect a user’s location via the location services in the smartphone. The multi-city nature of these apps provides a unique opportunity to understand how transit riders seek information as they travel between cities. The objective of this paper is to identify intercity travelers to/from the New York metropolitan region and understand their travel pattern in the visiting city, using one month of backend data from an application called “Transit”. Intercity travelers are identified based on the number of days each user has opened the app inside and outside of the New York region. Then, frequency of using the app are aggregated at county level and principal component analysis is performed to find the counties that usage within them impacts total app usage, by the intercity travelers, more significantly. In the next part, two trend analysis methods, decomposition method and wavelet method, are used to assess intercity traveler’s usage pattern over 31 days. Finally, to better capture pattern of using the app by day of week, trend function model and Poisson density distribution function are fitted to the data. Results of trend analysis showed that frequency of using “Transit” app is highly auto-correlated in one month as well as a big change point in the 10th day of the month. After removing the first 10 days, the auto-correlation in data was removed. For the remaining 20 days, trend analysis showed a 3 day period and a sin-cos function (with the period of 3 days) was found to be the best fitted model for frequency of using the app per day. This result captured the daily app usage pattern which was the same as the pattern that is usually observed in the transit usage data. The limitation of this research is that only one month of the Transit app data was used, errors in GPS data and difference between transit users’ behavior in searching for transit information and actually using the transit system.