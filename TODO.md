

 - [ ] Check randomForest for allowing INF predictors, recently these were disallowed
   - [ ] prep_data_randomForest, check these
   
- [ ] Tidy interface for all ... the thing to be transformed goes first

- [ ] Resolve differences between 
      - `apply.pattern` 
      - `emulate` 
      - `prep_data_scoring` 
      - `meta` 
      - `conformist::conform`
      
      - [ ] adopt S4 method `emulate` since multiple dispatch is necessary on the template/pattern
      - [ ] Use template as the second argument
      - [ ] Allow template to have more fields than the pattern and just use that are necessary
      - [ ] Allow for setting rules of how 

- [ ] What is the relationship with `conformist`

- [ ] Factor out RF specific function to rf.tools

- [ ] rf_test_names -> check_features

- [ ] prep_data: explicit_na "(Missing)"
 
- [ ] use `forcats` or `forcat.tools`
  - move `base.tools::add_level` to `forcat.tools`, compare to `fct_expand`
  

# bin-split-apply-combine-spread (BSACS)  
  
- [ ] Move to different package featurize 
  - [ ] `f_hist` may have to be an S4 method.
  - [ ] make `f_hist` more like *dplyr* functions 
  - [ ] add function to create the feature on its own.  
  - [ ] use `data.table::dcast( data, by ~ target, ... )` for summaries
        - use for continuos (?)
        
- [ ] Add rounding to ... splits