# velociraptor

Command line tool for measuring speed of gaining amount of
information in source files. Overall and per contributor.

##### Input
Via command line arguments (TODO: invent format)

1. auth token
2. [Repository] | User | Organization
3. Time frame to analise
4. Level of detail <Daily | Weekly | Monthly >
6. [source files extensions]


##### Output
Stdout. One entry per line, comma separated. Entry format:

1. Time frame start (ISO time)
2. Change in amount of information over - lines count <+|-(num)>
3. Changes in amount per contributor.
4. Number of contributors in this entry.


##### Notes
###### "Per contributor"
Only major contributors are taken into account. We arrange all
contributors for a time frame by amount of changes per the time frame
and do not ake into account guys who cumulatively introduced
less than 10% of changes.