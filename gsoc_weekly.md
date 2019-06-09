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

### Week 4. 

Mentors review, evaluate, and feedback.


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
