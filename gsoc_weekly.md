## Weekly plan for First Coding Month of GSoC 2019 (R-Ladies Dashboard)

### Week 1.

Use `meetupr` and `tidyverse` to retrieve and arrange R-Ladies groups. Update `find_groups()` if possible/required. 
Store resulting data in CSV/JSON files.
        
        
   #### Deliverables
        
   1. Setup Travis CI for this repo to automatically build the project, install `meetupr` and supporting packages,
           and call `deploy.sh`
        
   2. Write bash script (`deploy.sh`) to automatically run rladies.R script and update docs/data/rladies.csv file
           straight from the Meetup API
           
   3. Update `find_groups()` and `get_events()` in `meetupr` to retrieve optional fields like `past_event_counts` and `upcoming_event_counts`
           and use it within [R/rladies.R](https://github.com/benubah/r-community-explorer/blob/master/R/rladies.R) script)
        
   4. Submitted a [pull request](https://github.com/rladies/meetupr/pull/48) to rladies/meetupr repo with the changes above
   
   5. Created a new function `get_hosts()` in [R/rladies_hosts.R](https://github.com/benubah/r-community-explorer/blob/master/R/rladies_hosts.R) which depends on the new `find_groups()` and `get_events()` I created. `get_hosts()`
   retrieve `event_hosts` data from all R-Ladies events, and extracts the organizer's names (host names) and host count
   and with the help of Travis, stores them automatically in [docs/data/rladies_hosts.csv](https://github.com/benubah/r-community-explorer/blob/master/docs/data/rladies_hosts.csv) directory in descending order.
   
   

### Week 2. 

Layout Dashboard Page for R-Ladies using HTML based template

 #### Deliverables
 
 1. Improved R/rladies.R to analyze R-ladies group data and persist analyzed data in docs/data/rladies_summary.json and 
    docs/data/rladies_cumulative.json.
    The first JSON file holds summaries of numbers around R-Ladies chapters including countries, cities, number of chapters, 
    past and upcoming events, etc.
    The second JSON file holds data for a cumulative count of chapters over time till date. There were no chapter creations on
    meetup.com between 2012 - 2016, so, data was created for this period with zero values to make the visualization resemble reality.
    
 2. Provided a layout for the R-Ladies dashboard, with data widgets, a world map, list of the global leadership team, and a cumulative
    chart showing chapter growth. This is work for next week that has been front-loaded
    
 3. A dataTable enabling the searching and sorting of all R-Ladies chapters has been added to the dashboard to make locating a R-ladies
    chapter even more easier. A user can search by country, or city, or chapter name, and also sort based on member count, or past event     and upcoming event counts. This is also work for next week front-loaded.
    
 4. The web page has been deployed usng Github pages and is now available here: [R-Ladies dashboard](https://benubah.github.io/r-community-explorer/rladies.html)

 5. Added some more code to my pull request to rladies/meetupr package based on request from the authors.
 
 
### Week 3. 

Use Javascript to connect CSV/JSON data to dashboard and produce visualizations

1. Reviewed feedback from mentors. 

   - Adjusted sidebar to move away from toggle icon. 
   
   - Added a scrollable 'Global Team' list just below 'Global Leadership Team'

   - Removed echarts.js map - as it did not turn out very suitable. Recoded it into a binary map (it is yet to be decided where it would be placed or if it would be needed).

2. Explored and learnt about the `leaflet.js` Javascript charting library. Explored and learnt about GeoJSON data format. Explored the `leafletR` R package (especially the `toGeoJSON()` function that turns a data.frame with `lat` and `lon` data to GeoJSON.   Tried out a few examples of `leafletR` and the `leaflet.js` library. Found a way using Travis to drop a `.geojson` file (from `leafletR`) of the R-Ladies groups in the `docs/data/` directory of the project's repository. It gets interesting thence!

Create leaflet map using this `.geoson` file. Find a map that looks quite different from the default from carto.com

3. Explore leaflet awesome markers javascript package for custom markers - marker color and icons. Add it to the project. Include custom markers on the leaflet map.


4. Update rladies_summary.json file to contain a summary of member data for each region. Create a rose chart of this data and display on the dashboard.

Prettify the Cumulative growth charts for better visual appeal.


5. Update my `find_groups()` call in rladies.R to get `last_event` date for each group. Update rladies.R to process this variable into a date. Use this variable in the `.geoson` `feature.properties` to determine marker colors for those chapters that are inactive, unbegun and active.

Bind more information in each leaflet marker. Include calls in the marker to R-ladies community members to 'Become an organizer' for those chapters that are unbegun or inactive.

Find the new dashboard here: https://benubah.github.io/r-community-explorer/rladies.html


### Week 4. 

Mentors review, evaluate, and feedback.

1. Make correction of assigning co-founder to all global leadership team members

2. Update code to count inactive, active and unbegun groups and display same below the map.

3. Update map code to calculate 'Months Inactive' for Inactive groups based on their last date of event. Also, for Unbegun groups based on their creation date (since they have had no event).

4. Add a country chart - visualizing the number of chapters across all 47 countries.


## Weekly plan for Second Coding Month of GSoC 2019 (R User Groups Dashboard)

Week 1:

Week 2:

Week 3: 

Week 4:


## Weekly plan for Third Coding Month of GSoC 2019 (R-GsoC Dashboard)

Week 1:

Week 2:

Week 3: 

Week 4:
