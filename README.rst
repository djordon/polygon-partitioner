Polygon-partitioner
===================

|Build Status| |Coverage Status| |license|

For partitioning polygons into disjoint rectangles. Because you never know when you need to partition some polygons.


Features
--------

This package implements two main methods:
1. A method that approximates a polygon by a rectilinear polygon.

    This is guaranteed to cover the input with the default settings. You can change the coarseness of the approximation, leading to an output with far fewer points than the input (but without a guarantee of if covering the input).
 
2. A method that partitions a rectilinear polygon into non-overlapping rectangles.


Documentation
-------------


Installation
------------

```scala
lazy val partitioner = RootProject(uri("git://github.com/djordon/polygon-partitioner.git"))

lazy val root = Project("root", file("."))
  .dependsOn(partitioner)
```


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
