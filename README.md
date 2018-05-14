# scala-geo-tree

GeoTree is a data structure (currently, append-only) allowing to find a closest items to the given point.

GeoTree works faster than Redis in the cases when we ask to find nearest points with big radius. 

From Redis documentation: 
> However note that internally the command needs to perform an effort proportional to the number of items matching the specified area, so to query very large areas with a very small COUNT option may be slow even if just a few results are returned.

For instance, on my laptop it takes 0.7 seconds to find 10 points in radius 1000km for the point in the center of city with 1M points. GeoTree does it in ~6ms, 100x times faster.
 

### How to use

```scala

 import scala.geotree.map.tree.FractalArea.Coordinates
 import scala.geotree.map.tre
 val tree = new GeoTree[String]
 val latitude = 45.44
 val longitude = 56.67
 val world = World.Earth
 tree.insert(world.convertCoordinates(latitude, longitude))
 ... put other points
 
 val latitudeToSearch = 23.333
 val longitudeToSearch = 45.666
 val coordinates = world.convertCoordinates(latitudeToSearch,longitudeToSearch)
 val iterator = tree.getClosestPoints(coordinates)(Ordering.by(c => world.distance(c, coordinates))
 // iterator returns closest point in order of increasing distance from `coordanates`
```
 



### The limitations:
1. I code whole Earth surface with one unsigned long. So, it can work with limited precision only: now it's impossible to differentiate points closer than 1.8 centimeters. I suppose that it's okey when we use gps with precision about ~10m.
2. It can show wrong results for cities close to the International Date Line and poles.
3. It uses great circle distance which can be not very precise.
4. Memory consumption can be optimized.

#### To be done

- [ ] Fix checks for points close to the poles and the International Date Line.
- [ ] Provide immutable implementations
- [ ] Additional tests
- [ ] Provide implementation for 'plane' world
- [ ] Eliminate memory consumption
- [ ] Allow to remove items.