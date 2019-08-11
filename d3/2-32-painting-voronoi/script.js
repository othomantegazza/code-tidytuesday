///////// Otho Mantegazza - 9-8-19 

// Some lines of code are taken from:
// https://gist.github.com/nbremer/c0ffc07b23b1c556a66b

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
    .domain([0, 450])     // can use this instead of 1000 to have the max of data: d3.max(data, function(d) { return +d.price })
    .range([0, width]);

/*
get max of array  column with:
    d3.max(hexpix, d => d.x)
*/

svg.append("g")
    .attr("transform", "translate(0," + height + ")")
    .call(d3.axisBottom(x));

// Y axis: generate scale and draw
var y = d3.scaleLinear()
         .domain([0, 600])
         .range([0, height]);

svg.append("g")
    .call(d3.axisLeft(y));
    


// Draw the points --------------------------------------------------


//Initiate a group element for the circles	
var circleGroup = svg.append("g")
                        .attr("class", "circleWrapper"); 
                        
circleGroup.selectAll()
             .data(hexpix)
             .enter().append("circle")
             .attr("cx", d => x(d.x))
             .attr("cy", d => y(d.y))
             .attr("r", "3px")
		     .style("fill", d => d.hexval);


// Draw the Voronoi grid ---------------------------------------------

var voronoiGroup = svg.append("g")
                       .attr("class", "voronoiWrapper");

var voronoiTess = d3.voronoi()
                      .x(d => x(d.x))
                      .y(d => y(d.y))
                      .extent([[0, 0], [width, height]]);

voronoiGroup.selectAll()
              .data(voronoiTess(hexpix).polygons())
              .enter().append("path")
              .attr("d", function(d, i) { return "M" + d.join("L") + "Z"; })
              .attr("class", function(d, i) { return "pos" + [d.data.x] + [d.data.y]; })
              .datum(function(d, i) { return d.data; })
              .style("stroke", "#FFF") 
              .style("stroke-width", .5)
              .style("opacity", .5)
              .style("fill", d => d.hexval)
              .on("mouseover", enlarge);


/* .on("mouseover", function(d) { return d.style("opacity", 1); }) */

// Function on hover --------------------------------------------------

function enlarge(tile) {
  var cls = "pos" + [tile.x] + [tile.y]

  console.log(cls)

  var element = d3.select("." + cls)

  console.log(element)
  
  element.style("opacity", 1)
}