# ssp2024_amended
In the current version of the SSP economic data (v3.1), a few countries do not have GDP data from the OECD ENV-Growth model.

The script [R/amend_ssp2024.R](https://github.com/iiasa/ssp2024_amended/blob/main/R/amend_ssp2024.R) currently adds these countries only in two simple ways:

1. Taking the IIASA pathways for these missing countries. (generally lowest GDP/cap outcome)

Thoughts: while easily understandable, the IIASA trajectories have a quite different footprint, and this would general mean that the pathways for these countries see lower GDP growth across the board compared to where a ENV-Growth solution might have been. So we don't think these absolute numbers should be used.

Taking the shares of each country in the GDP of each R10 region they are in.
So, Venezuela  in 2025 would have been 2% of the "Latin America and Caribbean" region in another dataset, I calculate the new GDP as 
`gdp_Venezuela = (gdp_r10/(1-share_of_Venezuela_of_r10gdp_of_other_dataset))-gdp_r10 = (gdp_r10/0.98)-gdp_r10`
I did this for two datasets: 

2. From the OECD original SSP data. (generally highest GDP/cap outcome)

Thoughts: I think this has two potential issues. (1) the 2025 value is probably out of line, not incorporating recent evidence. E.g. Venezuela has 5% of Latin American GDP|PPP in 2025, coming out at 19k/cap. (2) it might be too optimistic about at least the near-term growth rates. Some of this could be resolved by applying a harmonization method (e.g. apply a declining multiplicative offset)  

3. From the IIASA new SSP data. (generally middle GDP/cap outcome)

Thoughts: I think this might be an acceptable middle-of-the-road option, where we (a) follow the general OECD growth trajectory of the region, and (b) incorporate recent evidence on recent challenges [having the same starting point as IIASA] for 2025 numbers; e.g. Venezuela in 2025 has GDP|PPP of 6k/cap. 

Folder structure:
* R: scripts
* input: data inputs (SSP releases)
* output: data outputs (amended SSP GDP data, and a few other diagnostics)
