# Hex-Analysis
Analysis of the Hex TCG ladder data

# Usage
1. Source "AnalyzeHex.R"
2. Download archive data from the day/s you want to analyze from: https://hexpvptools.net/archive - unzip the file and change filetype to .json - in case you want to analyze multiple days you have to combine the json files
3. Create data frame using "load_initial_data" using the json file as function input - in case you want to analyze archetypes (=decks with the same manabase), specify "with_archetypes" as TRUE
4. You are now set up for further analysis with the set of functions "create_wins_overview_by_hero", "create_wins_overview_by_archetype" and "common_cards_winning_decks"
