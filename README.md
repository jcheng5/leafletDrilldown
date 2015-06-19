# leafletDrilldown

I've been able to successfully implement a drill-down going from country to state/province with an info control that changes on hover, but it's still incredibly buggy. For example, I can't quite figure out how to modify the drill-down behavior (using observeEvent) once it's already at the lowest level, state in this case. So my app just completely crashes when clicking on a state level polygon.

To help, just fork this repo and download the files. Once you unzip the two folders with the shape files, it should be fully reproducible.

Issues:

1. I am trying to use this variable called ind to somehow determine what level I'm at in the drill-down, "c" for country and "s" for state. If I know what level it's at, then I can alter functionality using the if/else conditional statements. It doesn't work, though. I'm guessing it has something to do with environments. Or does the value need to be made reactive to work the way I'm wanting? I've tried using both reactiveValues and eventReactive with no success.

2. A closely related issue is that the hover info control does not work at the state level. I believe solving #1 will simultaneously solve #2.

3. The length of time from starting the app to the first map loading is way too long. Any suggestions for speeding it up? I actually bumped the smoothFactor from 0 to 0.5, and it didn't seem to make a difference. Fortunately, leafletProxy works pretty quickly when drilling down.


BTW, in testing the drill-down functionality, I've only tried US, Canada, and Mexico. Mexico doesn't work, and I'm just assuming it's due to name differences in the two data sets (hopefully). So for testing just stick with US and Canada.
