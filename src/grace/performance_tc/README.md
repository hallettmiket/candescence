This code is dedicated to exploring the output of a single experiment with MMDETECTION. That is, it takes the results that are left in
OUTPUT from running MMDETECTION using the config files in src/3-curriculum.

(An experiment is a run of MMDETECTION on a specific set of meta-parameters. An experiment covers all 6 grades.)

The structure of this code is a bit crazy, so we made Supplemental Figure 7 to describe the interdependencies. To clean this up and put it into a single R or jupyter notebook is on the list of things to do. 

It is the result of a half year of experimenting with different techniques and strategies for the data science.

evaluator.py is the basic object that slightly extends the evaluator class used in experiments 0-3. These are minor change that helped us collect and re-organize the results for each visualization etc.

It is necessary however to use this Evaluator class that is found in this folder for any analysis in src/performance.

IN generate_test_objects.py, the OUTPUT variable should be set to point to the directory that contains the output from running the configs of src/curriculum for your experiment.

The VARASANA dataset is  the location of the parent directory you downloaded from  
  http://csfg-algonquin.concordia.ca/~hallett/candescence/varasana/
from the temporary location of the Varasana files.

numba is used as a tag for all output files. 
thresh is the lower bound for labelling a bounding box.

CANDESCENCE should point to the parent directory of all files related to Candescence. 
For us this, was
/home/data/refined/candescence

Then we saved all the analyses for some experiment (eg experiment_27) in a subdirectory
e.g. /home/data/refined/candescence/performance/exp27_results/


