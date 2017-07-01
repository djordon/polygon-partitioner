Polygon-partitioner
===================

|Build Status| |Coverage Status| |license|

For partitioning polygons into disjoint rectangles. Because you never know when you need to partition some polygons.

<head>
<title></title>
<script src="https://cdn.plot.ly/plotly-1.12.0.min.js"></script>
</head>
<body>
<div id="chart"></div>
<script>
(function () {
  var data0 = {"type":"scatter","x":[-50.0,-0.0],"y":[100.0,100.0],"mode":"lines","marker":{"color":"rgba(0, 15, 249, 0.7)","line":{"color":"rgba(0, 15, 249, 0.7)","width":1.0}}};
  var data1 = {"type":"scatter","x":[-50.0,-0.0],"y":[125.0,125.0],"mode":"lines","marker":{"color":"rgba(0, 15, 249, 0.7)","line":{"color":"rgba(0, 15, 249, 0.7)","width":1.0}},"fill":"tonexty"};
  var data2 = {"type":"scatter","x":[-25.0,-0.0],"y":[100.0,100.0],"mode":"lines","marker":{"color":"rgba(0, 15, 146, 0.7)","line":{"color":"rgba(0, 15, 146, 0.7)","width":1.0}}};
  var data3 = {"type":"scatter","x":[-25.0,-0.0],"y":[200.0,200.0],"mode":"lines","marker":{"color":"rgba(0, 15, 146, 0.7)","line":{"color":"rgba(0, 15, 146, 0.7)","width":1.0}},"fill":"tonexty"};
  var data4 = {"type":"scatter","x":[-25.0,-0.0],"y":[50.0,50.0],"mode":"lines","marker":{"color":"rgba(0, 15, 185, 0.7)","line":{"color":"rgba(0, 15, 185, 0.7)","width":1.0}}};
  var data5 = {"type":"scatter","x":[-25.0,-0.0],"y":[75.0,75.0],"mode":"lines","marker":{"color":"rgba(0, 15, 185, 0.7)","line":{"color":"rgba(0, 15, 185, 0.7)","width":1.0}},"fill":"tonexty"};
  var data6 = {"type":"scatter","x":[0.0,25.0],"y":[0.0,0.0],"mode":"lines","marker":{"color":"rgba(0, 15, 82, 0.7)","line":{"color":"rgba(0, 15, 82, 0.7)","width":1.0}}};
  var data7 = {"type":"scatter","x":[0.0,25.0],"y":[25.0,25.0],"mode":"lines","marker":{"color":"rgba(0, 15, 82, 0.7)","line":{"color":"rgba(0, 15, 82, 0.7)","width":1.0}},"fill":"tonexty"};
  var data8 = {"type":"scatter","x":[75.0,100.0],"y":[50.0,50.0],"mode":"lines","marker":{"color":"rgba(0, 15, 151, 0.7)","line":{"color":"rgba(0, 15, 151, 0.7)","width":1.0}}};
  var data9 = {"type":"scatter","x":[75.0,100.0],"y":[100.0,100.0],"mode":"lines","marker":{"color":"rgba(0, 15, 151, 0.7)","line":{"color":"rgba(0, 15, 151, 0.7)","width":1.0}},"fill":"tonexty"};
  var data10 = {"type":"scatter","x":[-50.0,-25.0],"y":[150.0,150.0],"mode":"lines","marker":{"color":"rgba(0, 15, 229, 0.7)","line":{"color":"rgba(0, 15, 229, 0.7)","width":1.0}}};
  var data11 = {"type":"scatter","x":[-50.0,-25.0],"y":[175.0,175.0],"mode":"lines","marker":{"color":"rgba(0, 15, 229, 0.7)","line":{"color":"rgba(0, 15, 229, 0.7)","width":1.0}},"fill":"tonexty"};
  var data12 = {"type":"scatter","x":[0.0,50.0],"y":[25.0,25.0],"mode":"lines","marker":{"color":"rgba(0, 15, 200, 0.7)","line":{"color":"rgba(0, 15, 200, 0.7)","width":1.0}}};
  var data13 = {"type":"scatter","x":[0.0,50.0],"y":[250.0,250.0],"mode":"lines","marker":{"color":"rgba(0, 15, 200, 0.7)","line":{"color":"rgba(0, 15, 200, 0.7)","width":1.0}},"fill":"tonexty"};
  var data14 = {"type":"scatter","x":[-0.0,100.0],"y":[50.0,50.0],"mode":"lines","marker":{"color":"rgba(0, 15, 139, 0.7)","line":{"color":"rgba(0, 15, 139, 0.7)","width":1.0}}};
  var data15 = {"type":"scatter","x":[-0.0,100.0],"y":[225.0,225.0],"mode":"lines","marker":{"color":"rgba(0, 15, 139, 0.7)","line":{"color":"rgba(0, 15, 139, 0.7)","width":1.0}},"fill":"tonexty"};
  var data16 = {"type":"scatter","x":[-25.0,-0.0],"y":[125.0,125.0],"mode":"lines","marker":{"color":"rgba(0, 15, 119, 0.7)","line":{"color":"rgba(0, 15, 119, 0.7)","width":1.0}}};
  var data17 = {"type":"scatter","x":[-25.0,-0.0],"y":[200.0,200.0],"mode":"lines","marker":{"color":"rgba(0, 15, 119, 0.7)","line":{"color":"rgba(0, 15, 119, 0.7)","width":1.0}},"fill":"tonexty"};
  var data18 = {"type":"scatter","x":[25.0,100.0],"y":[200.0,200.0],"mode":"lines","marker":{"color":"rgba(0, 15, 133, 0.7)","line":{"color":"rgba(0, 15, 133, 0.7)","width":1.0}}};
  var data19 = {"type":"scatter","x":[25.0,100.0],"y":[225.0,225.0],"mode":"lines","marker":{"color":"rgba(0, 15, 133, 0.7)","line":{"color":"rgba(0, 15, 133, 0.7)","width":1.0}},"fill":"tonexty"};
  var data20 = {"type":"scatter","x":[75.0,100.0],"y":[75.0,75.0],"mode":"lines","marker":{"color":"rgba(0, 15, 123, 0.7)","line":{"color":"rgba(0, 15, 123, 0.7)","width":1.0}}};
  var data21 = {"type":"scatter","x":[75.0,100.0],"y":[100.0,100.0],"mode":"lines","marker":{"color":"rgba(0, 15, 123, 0.7)","line":{"color":"rgba(0, 15, 123, 0.7)","width":1.0}},"fill":"tonexty"};
  var data22 = {"type":"scatter","x":[-0.0,25.0],"y":[200.0,200.0],"mode":"lines","marker":{"color":"rgba(0, 15, 130, 0.7)","line":{"color":"rgba(0, 15, 130, 0.7)","width":1.0}}};
  var data23 = {"type":"scatter","x":[-0.0,25.0],"y":[225.0,225.0],"mode":"lines","marker":{"color":"rgba(0, 15, 130, 0.7)","line":{"color":"rgba(0, 15, 130, 0.7)","width":1.0}},"fill":"tonexty"};
  var data24 = {"type":"scatter","x":[75.0,100.0],"y":[150.0,150.0],"mode":"lines","marker":{"color":"rgba(0, 15, 115, 0.7)","line":{"color":"rgba(0, 15, 115, 0.7)","width":1.0}}};
  var data25 = {"type":"scatter","x":[75.0,100.0],"y":[175.0,175.0],"mode":"lines","marker":{"color":"rgba(0, 15, 115, 0.7)","line":{"color":"rgba(0, 15, 115, 0.7)","width":1.0}},"fill":"tonexty"};
  var data26 = {"type":"scatter","x":[75.0,75.0],"y":[100.0,75.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data27 = {"type":"scatter","x":[25.0,25.0],"y":[225.0,200.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data28 = {"type":"scatter","x":[-25.0,-0.0],"y":[125.0,125.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data29 = {"type":"scatter","x":[50.0,-0.0],"y":[50.0,50.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data30 = {"type":"scatter","x":[25.0,0.0],"y":[25.0,25.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data31 = {"type":"scatter","x":[-0.0,-0.0],"y":[50.0,75.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data32 = {"type":"scatter","x":[-0.0,-0.0],"y":[75.0,75.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data33 = {"type":"scatter","x":[-25.0,-25.0],"y":[150.0,175.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data34 = {"type":"scatter","x":[-0.0,-0.0],"y":[100.0,200.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data35 = {"type":"scatter","x":[-0.0,-0.0],"y":[200.0,200.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data36 = {"type":"scatter","x":[75.0,75.0],"y":[175.0,150.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data37 = {"type":"scatter","x":[75.0,-0.0],"y":[200.0,200.0],"mode":"lines","marker":{"color":"rgba(175, 175, 175, 0.85)","line":{"color":"rgba(175, 175, 175, 0.85)","width":1.0,"dash":"dot"}}};
  var data38 = {"type":"scatter","x":[0.0,25.0,25.0,50.0,50.0,75.0,100.0,100.0,100.0,75.0,75.0,75.0,100.0,100.0,75.0,75.0,100.0,100.0,75.0,50.0,25.0,25.0,0.0,-0.0,-0.0,-25.0,-25.0,-50.0,-50.0,-25.0,-25.0,-50.0,-50.0,-25.0,-0.0,-0.0,-25.0,-25.0,-0.0,-0.0,0.0],"y":[0.0,0.0,25.0,25.0,50.0,50.0,50.0,75.0,100.0,100.0,125.0,150.0,150.0,175.0,175.0,200.0,200.0,225.0,225.0,225.0,225.0,250.0,250.0,225.0,200.0,200.0,175.0,175.0,150.0,150.0,125.0,125.0,100.0,100.0,100.0,75.0,75.0,50.0,50.0,25.0,0.0],"mode":"markers+lines","marker":{"color":"rgba(194, 33, 10, 0.9)","line":{"color":"rgba(194, 33, 10, 0.9)","width":1.0}}};

  var data = [data0, data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23, data24, data25, data26, data27, data28, data29, data30, data31, data32, data33, data34, data35, data36, data37, data38];
  var layout = {"title":"","width":550,"height":550,"showlegend":false,"xaxis":{"showgrid":true,"showticklabels":false,"zeroline":false},"yaxis":{"showgrid":true,"showticklabels":false,"zeroline":false},"margin":{"l":15,"r":15,"t":15,"b":15},"plot_bgcolor":"rgba(0, 0, 0, 0.0)","paper_bgcolor":"rgba(0, 0, 0, 0.0)","hovermode":"closest"};

  Plotly.plot('chart', data, layout);
})();
</script>
</body>

Documentation
-------------


Features
--------


Installation
------------


Bugs and issues
---------------


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
