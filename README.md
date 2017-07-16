Polygon-partitioner
===================

[![Build Status](https://travis-ci.org/djordon/polygon-partitioner.svg?branch=master)](https://travis-ci.org/djordon/polygon-partitioner) 
[![Coverage Status](https://coveralls.io/repos/github/djordon/polygon-partitioner/badge.svg?branch=master)](https://coveralls.io/github/djordon/polygon-partitioner?branch=master)
[![MIT License](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT)

For partitioning polygons into disjoint rectangles. Because you never know when you need to partition some polygons.

Features
--------

This package implements two main methods:

1.  A method that approximates a polygon by a orthogonal polygon.

    This is guaranteed to cover the input polygon. There are also methods that allow you to change the coarseness of the approximation.

2.  A method that partitions a orthogonal polygon into non-overlapping rectangles.

    This method handles polygons with holes and chords.

The functions in this package use [JTS](https://github.com/locationtech/jts) Polygons as the class for the input Polygon.

Installation
------------

If you want to install `polygon-partitioner` and all the submodules, which includes `core`, `plot`, and `db`, add the following to your `build.sbt` file:

```scala
lazy val partitioner = RootProject(uri("git://github.com/djordon/polygon-partitioner.git"))

lazy val root = Project("root", file("."))
  .dependsOn(partitioner)
```

If you only want the algorithms in the `core` module and don't care about the plotting and db modules, then you can install it by adding something like the following to `build.sbt`:

```scala
lazy val partitionerCore = ProjectRef(uri("git://github.com/djordon/polygon-partitioner.git"), "core")

lazy val root = Project("root", file("."))
  .dependsOn(partitionerCore)
```

Documentation
-------------

### Covering a polygon with an orthogonal polygon

If you want to approximate a polygon by an orthogonal polygon, do something like

```scala
import org.partitioner.OrthogonalPolygonBuilder
import com.vividsolutions.jts.geom.Polygon

val myPolygon: Polygon = ...
val orthogonalPolygon: Polygon = OrthogonalPolygonBuilder.createExteriorCover(myPolygon)
```

The above does not handle holes in the input. For that, use the `cover` method, which attempts to accommodate holes. Here is an example:

```scala
import org.partitioner.OrthogonalPolygonBuilder
import com.vividsolutions.jts.geom.Polygon

val myPolygon: Polygon = ...
val orthogonalPolygon: Polygon = OrthogonalPolygonBuilder.cover(myPolygon)
```

Note that the output polygon in the above example is not guaranteed to have as many holes from the input polygon.

For both `createExteriorCover` and `cover`, there are parameters that you can use to tune the coarseness of the cover. The default returns the finest orthogonal polygon that covers the input using the particular algorithm implemented here. 

### Partitioning an polygon

If you want to partition an orthogonal polygon into non-overlapping rectangles, use the following

```scala
import org.partitioner.orthogonal.OrthogonalPolygonPartitioner
import org.partitioner.Rectangle
import com.vividsolutions.jts.geom.Polygon

val myPolygon: Polygon = ...
val rectangles: List[Rectangle] = OrthogonalPolygonPartitioner.partition(myPolygon)
```

If you want to find a collection of non-overlapping rectangles whose union covers the input, use the following

```scala
import org.partitioner.PolygonPartitioner
import org.partitioner.Rectangle
import com.vividsolutions.jts.geom.Polygon

val myPolygon: Polygon = ...
val rectangles: List[Rectangle] = PolygonPartitioner.partition(myPolygon)
```

Bugs and issues
---------------

File bugs/issues/requests at <https://github.com/djordon/polygon-partitioner/issues>.

Copyright and license
---------------------

Code and documentation Copyright 2017 Daniel Jordon. Code released under the [MIT license](https://github.com/djordon/polygon-partitioner/blob/master/LICENSE.txt).
