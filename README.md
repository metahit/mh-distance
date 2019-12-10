# mh-distance
Processing distances for Metahit

## units for distances

inputs/190918_data_from_RTS:	thousand km per year

inputs/VehicleType_LALevel:	miles per year

inputs/five_road_data:	thousand km per year

outputs/mode_road_city:		thousand km, 6 years

outputs/mode_road_city_las:		thousand km, 6 years

outputs/all_distances:		km per week


## Files

`generate_city_distances_using_road_links.R` uses raw AADF (annual average daily flow) counts to estimate primary road distances and explores some methods for extrapolating to minor roads at the city-region level. Output = outputs/mode_5road_city.csv. (Saved here for reference - not used at present in mh.)

`fleet_distances_for_injury.R` uses raw AADF (annual average daily flow) counts to estimate motorway and other road distances for all years. Output = mh-injury/rds_storage/mode_road_city_year.csv.

`generate_la_distances_for_travel_matrices.R` uses raw AADF counts to estimate primary road distances for LAs and concatenates the total LA distance for use in metahit/mh-route-commutes for the allocation of trips of the synthetic population to LAs and road types. Output = outputs/mode_road_city_las.csv.

`process_distances_for_execute.R` takes the synthetic population and the travel-mapping matrices to compute travel summaries used in mh-execute. Output = mh-execute/inputs/distances.

`compare_SP_to_RTS_total.R` takes the synthetic population and the travel-mapping matrices to compute travel summaries for the whole of England to compare to five_road_data. Output = outputs/compare_rts_sp. 

## Distances for execute

The main output is the set of processed distances for the execution of Metahit. In summary, they are:

| Label | Description | Dimensions | Units | City boundary | Modes | Who |
| --- | --- | --- | --- | --- | --- | --- |
| emission | Contribution to AP emissions | Road, LA, mode | Distance | Travel within city | Car, motorcycle, *bus*, *van* | Drivers |
| noise | Contribution to noise emissions | Road, LA, mode | Distance | Travel within city | Car, motorcycle, *bus*, *van* | Drivers |
| cas | Travel at risk of injury | Road, city, mode, demography | Distance | Travel within city | Car, motorcycle, walk, cycle | Drivers and passengers |
| strike | Travel posing risk of injury | Road, city, mode, demography | Distance | Travel within city | Car, motorcycle, walk, cycle | Drivers |
| pa | Physical activity | Mode, id | Duration | Travel of city residents | Walk, cycle | Drivers |
| inh | Travel exposed to AP | Road, LA, mode, id | *Duration* | Travel of city residents within city | Car, motorcycle, bus, walk, cycle, *tube, train*, *van* | Drivers and passengers |

In *italics* are things not implemented, or not implemented properly.
