Polygon-partitioner
===================

|Build Status| |Coverage Status| |license|

For partitioning polygons into disjoint rectangles. Because you never know when you need to partition some polygons.


Features
--------

This package implements two main methods:

1. A method that approximates a polygon by a rectilinear polygon.

   This is guaranteed to cover the input polygon. There are also methods that allow you to change the coarseness of the approximation.

2. A method that partitions a rectilinear polygon into non-overlapping rectangles.

   This method handles polygons with holes and chords.

.. comments

   If the input polygon is chord-free, the output is guaranteed to be the minimum number of non-overlapping rectangles.

Documentation
-------------

If you want to approximate a polygon by an orthogonal polygon, do something like

.. code-block:: scala

   import org.partitioner.OrthogonalPolygonBuilder
   import com.vividsolutions.jts.geom.Polygon

   val myPolygon: Polygon = ...
   val orthogonalPolygon: Polygon = OrthogonalPolygonBuilder
     .createExteriorCover(myPolygon)


The above does not handle holes in the input. For that, use

.. code-block:: scala

   import org.partitioner.OrthogonalPolygonBuilder
   import com.vividsolutions.jts.geom.Polygon

   val myPolygon: Polygon = ...
   val orthogonalPolygon: Polygon = OrthogonalPolygonBuilder
     .cover(myPolygon)


The output polygon, `orthogonalPolygon`, is not guaranteed to have as many holes from the input polygon. If you want to partition an orthogonal polygon, do something like

.. code-block:: scala

   import org.partitioner.partition.OrthogonalPolygonPartitioner
   import org.partitioner.Rectangle
   import com.vividsolutions.jts.geom.Polygon

   val myPolygon: Polygon = ...
   val rectangles: List[Rectangle] = OrthogonalPolygonBuilder
     .partition(myPolygon)


Installation
------------

.. code-block:: scala

   lazy val partitioner = RootProject(uri("git://github.com/djordon/polygon-partitioner.git"))

   lazy val root = Project("root", file("."))
     .dependsOn(partitioner)


Bugs and issues
---------------

File bugs/issues/requests at https://github.com/djordon/polygon-partitioner/issues.


Copyright and license
---------------------

Code and documentation Copyright 2017 Daniel Jordon. Code released
under the `MIT
license <https://github.com/djordon/polygon-partitioner/blob/master/LICENSE.txt>`__.

.. |Build Status| image:: https://travis-ci.org/djordon/polygon-partitioner.svg?branch=master
   :target: https://travis-ci.org/djordon/polygon-partitioner

.. |Coverage Status| image:: https://coveralls.io/repos/github/djordon/polygon-partitioner/badge.svg?branch=master
   :target: https://coveralls.io/github/djordon/polygon-partitioner?branch=master

.. |license| image:: https://img.shields.io/github/license/mashape/apistatus.svg
    :alt: MIT License
    :target: https://opensource.org/licenses/MIT
