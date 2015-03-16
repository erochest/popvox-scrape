
# popvox-scrape

Scrapes data from popvox.com and mashes it up with some from opensecrets.org.

## Installation

To install this, just download it and use `cabal`:

```bash
cd popvox-scrape
cabal install
```

# Data Flow

This section will look at the outputs and try to trace each field back to where
it came from.

## `bill-ranks.csv`

### Inputs

The inputs for this data file come from three places:

**Bill Data**: Downloaded from
[Govtrack.us](https://www.govtrack.us/congress/bills/). This is JSON.

**Legislator Scores**: These are DW-NOMINATE scores downloaded from
[Voteview.com](http://voteview.com/dwnominate.asp). This is a fixed-width data
file.

**Legislator Information**: These are indexes of legislators with various IDs
that have been used with them in different data sources, along with other
information about each legislator and the terms he or she served. These were
downloaded from the United State's [Github
account](https://github.com/unitedstates/congress-legislators). This is YAML.

### Output

**Bill**: Values in this field look like "HC1 (109)". The parts come from the
bill data, the *number*, *prefix*, and *session* fields.

**Congress**: Values in this field look like "109". It comes from the bill
data's *session* field.

**Sponsor Count**: Values in this field are numeric counts. They are taken by
counting the number of sponsors listed in the bill data.

**Dem Support**: Values in this field are numeric counts. They are taken by
filtering the sponsor information in the bill data based on each legislator's
party, which is taken from the legislator information data.

**GOP Support**: Values in this field are numeric counts. They are taken by
filtering the sponsor information in the bill data based on each legislator's
party, which is taken from the legislator information data.

**Score**: Values in this field are real numbers. This is the mean DW-NOMINATE
score for the sponsoring legislators, and it comes from the legislator score
data.

## `contrib-data.csv`

### Inputs

**DIME**: This data contains information about campaign contributions that have
been made to legislators. It was downloaded from Stanford's [Social Science
Data Collection](http://data.stanford.edu/dime). This is in CSV format.

**MapLight Data**: This data contains information about each of the bills and
the organizations who supported it. It was downloaded from the [MapLight
site](http://maplight.org/us-congress/guide/data), and it is in JSON format.

### Output

**Organization**: This is the name of the organization who supported any bill.
This comes from the DIME data field "contributor_name".

**District 10s**: Values in this field are in the form "TX1". It represents
the contributor's congressional district in the 2010s. It comes from the DIME
data field "contributor_district_10s".

*Contribution Columns*: These show how much each organization has contributed
to both of the two major parties. This is found by summing the "amount" column
for that organization to each party's candidates.

*Bill Columns*: These columns show whether the organization supported or
opposed specific bills. Support is coded '1', neutral '0', and opposition '-1'.
The organizations' dispositions are taken from the MapLight data's
"organizations" key.

*Total Columns*: This column is the amount that the organization donated to
different parties.

