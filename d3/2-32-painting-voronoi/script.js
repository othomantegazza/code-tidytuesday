///////// Otho Mantegazza - 9-8-19 

// Some lines of code are taken from:
// https://gist.github.com/nbremer/c0ffc07b23b1c556a66b

// and most from:
// https://bl.ocks.org/tophtucker/26b8e7e35da08ec30d5a47dc8fe6aaba

// set the dimensions and margins of the graph --------------------


var margin = {top: 10, right: 30, bottom: 30, left: 40},
    width = 900 - margin.left - margin.right,
    height = 1200 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg = d3.select("#my_dataviz")
  .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");


// Axis ------------------------------------------------------------          

// X axis: scale and draw:
var x = d3.scaleLinear()
    .domain([0, 450])   
    .range([0, width]);

// Y axis: generate scale and draw
var y = d3.scaleLinear()
         .domain([0, 600])
         .range([0, height]);


// Draw the Voronoi grid ---------------------------------------------

var voronoiGroup = svg.append("g")
                       .attr("class", "voronoiWrapper");

var voronoiTess = d3.voronoi()
                      .x(d => x(d.x))
                      .y(d => y(d.y))
                      .extent([[0, 0], [width, height]]);

var scale = .1

var polygon = svg.append("defs")
  .selectAll("clipPath")
  .data(voronoiTess.polygons(hexpix))
  .enter().append("clipPath")
    .attr("id", function(d,i) { return "clip" + i; })
    .append("path")
    .attr("d", function(d, i) { return "M" + d.join("L") + "Z"; });

var site = svg.append("g")
    .selectAll("circle")
    .data(hexpix)
    .enter().append("circle")
      .attr("r", 2.5)
      .attr("cx", function(d) { return x(d.x); })
      .attr("cy", function(d) { return y(d.y); })
      .attr("fill", function(d) { return d.hexval; })
      .attr("clip-path", function(d,i) { return "url(#clip" + i + ")"; })
    .transition()
      .delay(2000)
      .duration(9000)
      .attr("r", getMaxLinkDistance(voronoiTess.links(hexpix)));               

// get max voronoi dist to center ----------------------------------------

function getMaxLinkDistance(links) {
  return d3.max(links.map(function(d) {
    return Math.sqrt(
      Math.pow(d.target['x'] - d.source['x'],2) +
      Math.pow(d.target['y'] - d.source['y'],2)
    );
  }));
}