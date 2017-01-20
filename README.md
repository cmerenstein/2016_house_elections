# 2016_house_elections
## A very last-minute attempt to predict House elections

I made all of this 2 weeks before the 2016 elections. Because of the time crunch, there's a lot that is a bit sloppy.


It primarily just relied on the generic ballot polling and each districts partisan voting index (PVI).
PVI is calculated based on presidential results in each district relative to the national vote margin.
An additional bump was given to incumbents running for reelection based on the typical incumbent advantage from 2004-2014.
This effect was calculated in the script incumbent_effect.r

![Really gotta switch the colors here...](Election_results_by_PVI.png?raw=true "Election Margins vs Partisan Voter Index")

Additionally, I tried to use the amount of campaign contributions that each candidate had raised as a predictor.
This turned out to be an imperfect metric because I couldn't find a source to differentiate between primary and general election.
It also took some adjustments to account for non-competitive races, which lower the correlation.

![](Results_by_Contributions.png?raw=true "Election Margins vs Contributions")

