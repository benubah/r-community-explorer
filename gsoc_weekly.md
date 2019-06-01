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
