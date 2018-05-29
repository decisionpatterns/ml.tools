
 - CARET
   to get the response from a caret::train.formula object: 
   
      fit %>% formula %>% lhs
   
   
   
 - [ ] Factor out ml.report to include all facilities for generating reports from 
   the model. 
   - cf inst/, velocity solutions dda overdraft 
   
 - Finish writing ggSeparate (Class Separation Plot)
 
 - ggROC 
   - Allow multiple plots, e.g. the comparison between models and
     of predictors as well as the contribution?
     
   - Allow a weighting factor to demonstrate the effects of dollars
   - https://cran.r-project.org/package=plotROC esp style_roc()

   - [ ] Write a general ggROC.default(x=model,y=data) for an arbitrary model.  
         1. First check if
   
   - [ ]Reduce to S3 method? Yes. Probably
   
   

 - [x] Confuse use `obs` and `pred` argument names instead of `actual` and 
       `predicted`. Labels should be observed and predicted however.
   
- [ ] Tidy interface for all `...`; the thing to be transformed goes first

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

- [x] Factor out RF specific function to randomForest.tools

- [ ] Check randomForest for allowing INF predictors, recently these were disallowed
   - [ ] prep_data_randomForest, check these

- [x] rf_test_names -> check_features

- [ ] prep_data: explicit_na "(Missing)"
 
- [ ] use `forcats` or `forcat.tools`
  - move `base.tools::add_level` to `forcat.tools`, compare to `fct_expand`
  

# bin-split-apply-combine-spread (BSACS)  
  
- [x] Move to different package featurize 
  - [ ] `f_hist` may have to be an S4 method.
  - [ ] make `f_hist` more like *dplyr* functions 
  - [ ] add function to create the feature on its own.  
  - [ ] use `data.table::dcast( data, by ~ target, ... )` for summaries
        - use for continuos (?)
        
- [ ] Add rounding to ... splits

- [ ] Write `name_it` that produces a canonical name for an object based on the 
      objects contents; For example, a caret model might be named 
      `fit.loss.rf.2000` indicating it is a model of loss using rf and a sample 
      size of 2000?