# ssp2024_amended
 Use SSP v3 data and amend it where data for some countries is missing.  


This repository starts from that the most current updated SSP data (v3.1), does not feature all countries.
In particular, it focuses on (marker) OECD GDP data set missing a few key countries. 

The script currently adds these countries only in two simple ways:

1. "v3only": add the IIASA GDP from the same SSP release for the (4) big missing countries
2. "oecdfirst": first add the SSPv2 OECD, after transforming the units, and then IIASA GDP where there's no price data for the unit change


Folder structure:
* R: scripts
* input: data inputs (SSP releases)
* output: data outputs (amended SSP GDP data, and a few other diagnostics)
