# mh-distance
Processing distances for Metahit

## units for distances

inputs/190918_data_from_RTS:	thousand km per year

inputs/VehicleType_LALevel:	miles per year

outputs/mode_road_city:		thousand km, 6 years

outputs/all_distances:		km per week


## Files

`generate_city_distances_using_road_links.R` uses raw AADF (annual average daily flow) counts to estimate primary road distances and explores some methods for extrapolating to minor roads at the city-region level. Output = outputs/mode_road_city.csv.

`generate_la_distances_for_scenario.R` uses raw AADF counts to estimate primary road distances for LAs and concatenates the total LA distance for use in metahit/mh-scenarios for the allocation of trips of the synthetic population to LAs and road types. Output = outputs/mode_road_city_las.csv.

`process_distances_for_execute.R` takes the synthetic population and the travel-mapping matrices to compute travel summaries used in mh-execute. Output = outputs/all_distances.Rds. (Should be saved directly into mh-execute/inputs.)
