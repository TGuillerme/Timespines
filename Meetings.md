## 2018/04/17

We will now try to focus the project on the "through time" aspect of it (the relation between spininess and body size from the Devonian to the present).

I (Thomas) will work on getting the pipeline ready for the living species.
This includes the full analysis from data preparation and visualisation to hypothesis testing.

We will test the hypothesis in two ways:

 1. Using the threshold model to detect a correlation between the discrete trait (armourness) and the continuous trait (body size). The test is implemented in `phytools::threshBayes`.
 2. Using a null model expectancy were we can make both traits evolve with three correlation scenarios: (1) no correlation between body size, armourness and birth-death; (2) a correlation between body size and armourness where _small_-armoured taxa are more likely to speciate and less likely to go extinct; (3) a correlation between body size and armourness where _big_-armoured taxa are more likely to speciate and less likely to go extinct. We can then compare the resulting distributions to the observed one using a permutation style test.

### The things we need to do:

 - [ ] get a tree for living sharks genus ([this one?](https://www.nature.com/articles/s41559-017-0448-4?error=cookies_not_supported&code=9d5dd48d-ee56-48dc-b669-4431c6c1bf55)). [Anna?]

 - [ ] get the spine data for the fossil fishes! [everyone?]

 - [ ] differentiate the loss and gains of armour in the analysis and the plots. [Thomas]

 - [ ] pipeline the `threshBayes` test. [Thomas]

 - [ ] pipeline the null test. [Thomas]

 - [ ] linking the fossil trees together and to the living tree (if possible - the pruned tree is [here](https://github.com/TGuillerme/Timespines/tree/master/Data/Processed/342t_living.tre)). [Lauren?]

## 2017/11/27

> Getting there!
 * We will use the three approaches roughly described [here](https://cdn.rawgit.com/TGuillerme/Timespines/a149bcb5/Analysis/ArmorChange.html) for looking at the changes in body armour per group:
 	* Based on taxonomy
 	* Based on topology (Fitch downpass)
 	* Based on branch length (Fitch downpass + ancestral states reconstructions)
 * For both data sets (living ones and fossil ones) we will need groups phylogenies (they can be crude), body size data and armourness data

|   | Living  | Fossil  |
|---|---|---|
| Body sizes | OK | OK |
| Armourness | OK | missing |
| Phylogenies | OK | OK |

 * Richard and Marco can have a look at the armourness for the fossils and see what they can easily collect and what will still need to be done
 * Somebody will have to find phylogenies for the living taxa (it should be easy since we only have them at the Familly-ish level)
 * I (Thomas) will fine tune the analysis to look how we count body size when changes in armourness:
 	* Relative to group's max and median body size
 	* Relative to the max body size at the same age
 	* Relative to the max body size at the same age and location

Next meeting some time late January?

## 2017/07/27

> More fish!
 * Lauren is joining the team with great idea and nice data! Yay!
 * We simplify the project for a first pass:
 	* the idea will be to only look at fish groups that have a switch-on/switch-off of body armour
 	* body armour (presence/absence) can now be measured relatively from within the group (is there a change or not)
 	* we can then measure the change in body in the group, through time for each gain/loss
 		* Carefull there to adjust the change in body mass by correcting for the general trend in body mass (increase).
 	* one idea of the analysis would then be to check whether there is an increase/decrease in body mass related to gain/loss in armour through time (if danger zone, we would expect an increase/decrease but not no changes).
 * These are all really exciting ideas, we should try to find some meeting grants we could apply for organising a timespines meeting in real life.

Next meeting some time late August?

## 2017/04/20

 * Rafael is joining the team!
 * We keep collecting data from last meeting
 * Thomas will get some analysis done for the BES Macro meeting
 
## 2017/03/01

> Fish time!
 * Data collection continues
 * Richard will focus on collecting and formating the data from Sallan and Galimberti 2015 Nature (http://science.sciencemag.org/content/350/6262/812.full)
 * Marco will focus on collecting and formating the data from Clarke et al 2016 PNAS (http://www.pnas.org/content/113/41/11531.abstract)	
 * Anna will focus on collecting and formating the data from Price et al 2015 Proc B (http://rspb.royalsocietypublishing.org/content/282/1819/20151428)

## 2017/01/20

> Data collection protocol:
 * Don't worry about the type of size measurement per sites (length vs mass). We well be measuring things in units of standard deviation so the only important thing is to be consistent per sites.
 * Start pushing any good-ish data on GitHub (everybody has push access).

> Analysis protocol
 * We can use the data from the "Danger zone" paper as our baseline for extant ecosystems. We will do some sensitivity analysis (rarefaction) on it.
 * We can use any good fish lagerstatten as our base expectation: fishes are easy to deal with (they all predate-ish, they are all tube-like-ish, they are all blue and swim-in-the-water-ish)

Next meeting/deadline on the 20th of February.

## 2016/11/28

> Data collection protocol refinement:
 * For body mass/length measurements, always go with the maximum
 * For body mass/length estimates, use regressions from the closest relative group you can find
 * When partial (but identifiable) bits are found for a species, use estimates from its genus (if possible)
 * If just a small part (femur head, teeth, etc...) is available without more info, put `NA` everywhere
 
Next meeting/deadline on the 16th of January (we suggested two sites to be properly collected and submitted on GitHub by then).
 
