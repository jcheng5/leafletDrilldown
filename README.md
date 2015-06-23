# leafletDrilldown

I've been able to successfully implement a drill-down going from country to state/province with an info control that changes on hover, but it's still incredibly buggy. For example, I can't quite figure out how to modify the drill-down behavior (using observeEvent) once it's already at the lowest level, state in this case. So my app just completely crashes when clicking on a state level polygon with the following message: 

    Warning: Unhandled error in observer: missing value where TRUE/FALSE needed
    observeEvent(input$mymap_shape_click)


To help, just fork this repo and download the files. Once you unzip the two folders with the shape files, it should be fully reproducible.

Issues:

1. I am trying to use this variable called ind to somehow determine what level I'm at in the drill-down, "c" for country and "s" for state. If I know what level it's at, then I can alter functionality using the if/else conditional statements. It doesn't work, though. I'm guessing it has something to do with environments. Or does the value need to be made reactive to work the way I'm wanting? I've tried using both reactiveValues and eventReactive with no success.

2. A closely related issue is that the hover info control does not work at the state level. I believe solving #1 will simultaneously solve #2.

3. The length of time from starting the app to the first map loading is way too long. Same goes for resetting to the country map using the action button. Any suggestions for speeding it up? Could using another data format like geoJSON improve performance? I actually bumped the smoothFactor from 0 to 0.5, and it didn't seem to make a difference. Fortunately, leafletProxy works pretty quickly when drilling down.

4. The last issue I'd like to resolve is with the info control. Ideally, it would operate like http://leafletjs.com/examples/choropleth.html where it can tell when you're not over a country and adjust the information accordingly. It doesn't seem like I'll be able to use input$mymap_shape_mouseover or input$mymap_shape_mouseout for that. Any other strategies to include this funtionality?



BTW, in testing the drill-down functionality, I've only tried US, Canada, and Mexico. Mexico doesn't work, and I'm just assuming it's due to name differences in the two data sets (hopefully). So for testing just stick with US and Canada.

The shape files for countries and states/provinces come from Natural Earth. See http://www.naturalearthdata.com/downloads/10m-cultural-vectors/.
