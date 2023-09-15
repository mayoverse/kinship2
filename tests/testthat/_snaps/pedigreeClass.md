# pedigree works

    Code
      summary(ped)
    Output
      Pedigree object with 0 individuals
          indId             fatherId           motherId             gender   
       Length:0           Length:0           Length:0           Min.   : NA  
       Class :character   Class :character   Class :character   1st Qu.: NA  
       Mode  :character   Mode  :character   Mode  :character   Median : NA  
                                                                Mean   :NaN  
                                                                3rd Qu.: NA  
                                                                Max.   : NA  
          family            available      affected       sex           
       Length:0           Min.   : NA   Min.   : NA   Length:0          
       Class :character   1st Qu.: NA   1st Qu.: NA   Class :character  
       Mode  :character   Median : NA   Median : NA   Mode  :character  
                          Mean   :NaN   Mean   :NaN                     
                          3rd Qu.: NA   3rd Qu.: NA                     
                          Max.   : NA   Max.   : NA                     
          avail                id               dadid              momid          
       Length:0           Length:0           Length:0           Length:0          
       Class :character   Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character   Mode  :character  
                                                                                  
                                                                                  
                                                                                  
          error              steril             status         
       Length:0           Length:0           Length:0          
       Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character  
                                                               
                                                               
                                                               
      and 0 special relationships.
          indId1             indId2               code        family         
       Length:0           Length:0           Min.   : NA   Length:0          
       Class :character   Class :character   1st Qu.: NA   Class :character  
       Mode  :character   Mode  :character   Median : NA   Mode  :character  
                                             Mean   :NaN                     
                                             3rd Qu.: NA                     
                                             Max.   : NA                     
           id1                id2               error          
       Length:0           Length:0           Length:0          
       Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character  
                                                               
                                                               
                                                               
      The filling scales columns are: 
      The border scale column are: 

# pedigree from sampleped and affectation

    Code
      summary(ped1)
    Output
      Pedigree object with 41 individuals
          family             indId             fatherId           motherId        
       Length:41          Length:41          Length:41          Length:41         
       Class :character   Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character   Mode  :character  
                                                                                  
                                                                                  
                                                                                  
                                                                                  
           gender         affected        available              sex    
       Min.   :1.000   Min.   :0.0000   Min.   :0.0000   male      :20  
       1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:0.0000   female    :21  
       Median :2.000   Median :1.0000   Median :0.0000   unknown   : 0  
       Mean   :1.512   Mean   :0.5143   Mean   :0.3902   terminated: 0  
       3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:1.0000                  
       Max.   :2.000   Max.   :1.0000   Max.   :1.0000                  
                       NA's   :6                                        
           avail             id               dadid              momid          
       Min.   :0.0000   Length:41          Length:41          Length:41         
       1st Qu.:0.0000   Class :character   Class :character   Class :character  
       Median :0.0000   Mode  :character   Mode  :character   Mode  :character  
       Mean   :0.3902                                                           
       3rd Qu.:1.0000                                                           
       Max.   :1.0000                                                           
                                                                                
          error            steril            status   affected_aff   
       Length:41          Mode:logical   Min.   :0   Min.   :0.0000  
       Class :character   NA's:41        1st Qu.:0   1st Qu.:0.0000  
       Mode  :character                  Median :0   Median :1.0000  
                                         Mean   :0   Mean   :0.5143  
                                         3rd Qu.:0   3rd Qu.:1.0000  
                                         Max.   :0   Max.   :1.0000  
                                                     NA's   :6       
      and 0 special relationships.
          indId1             indId2               code        family         
       Length:0           Length:0           Min.   : NA   Length:0          
       Class :character   Class :character   1st Qu.: NA   Class :character  
       Mode  :character   Mode  :character   Median : NA   Mode  :character  
                                             Mean   :NaN                     
                                             3rd Qu.: NA                     
                                             Max.   : NA                     
           id1                id2               error          
       Length:0           Length:0           Length:0          
       Class :character   Class :character   Class :character  
       Mode  :character   Mode  :character   Mode  :character  
                                                               
                                                               
                                                               
      The filling scales columns are: affected
      The border scale column are: avail

