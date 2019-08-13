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
                .style("fill", d => d.hexval)
              /* .transition().duration(10000)
                .attr("r", "10px")
                .style("opacity", ".5") */;


// Draw the Voronoi grid ---------------------------------------------

var voronoiGroup = svg.append("g")
                       .attr("class", "voronoiWrapper");

var voronoiTess = d3.voronoi()
                      .x(d => x(d.x))
                      .y(d => y(d.y))
                      .extent([[0, 0], [width, height]]);

var scale = .1

voronoiGroup.selectAll()
              .data(voronoiTess(hexpix).polygons())
              .enter().append("path")
                .attr("d", function(d, i) { return "M" + d.join("L") + "Z"; })
                .attr("class", function(d, i) { return "pos" + [d.data.x] + [d.data.y]; })
              .datum(function(d, i) { return d.data; })
                .style("stroke", "#FFF") 
                .style("stroke-width", .5)
                //.style("opacity", .5)
                .style("fill", d => d.hexval)
                .attr("transform", function(d) { var xin = x(d.x)
                                                  yin = y(d.y);

                                               return "translate(" + xin + "," + yin + ")"
                                                       + "scale(" + scale + ")"
                                                       + "translate(" + -xin + "," + -yin + ")";
                                              })
              .transition().duration(7000)
              .attr("transform", function(d) { var xin = x(d.x)
                                                yin = y(d.y);

                                             return "translate(" + xin + "," + yin + ")"
                                                     + "scale(" + 1 + ")"
                                                     + "translate(" + -xin + "," + -yin + ")";
                                            });




/* 
.on("mouseover", enlarge) */

/* .on("mouseover", function(d) { return d.style("opacity", 1); }) */

// Function on hover --------------------------------------------------

function enlarge(tile) {
  var cls = "pos" + [tile.x] + [tile.y]

  console.log(cls)

  var element = d3.select("." + cls)

  console.log(element)
  
  element.style("opacity", 1)
}

/* d3.select("#changeDensity").on("click", function() {
  console.log("ciao")
  voronoiGroup.datum(function(d, i) { return d.data; })
    .transition()
    .duration(3000)
    .attr("transform", function(d) {
      return "translate(" + d.x + "," + d.y + ")"
        + "scale(" + 0.1 + ")"
        + "translate(" + -d.x + "," + -d.y + ")";
  });
}); */

/* var cntrds = d3.selectAll("#voronoiWrapper path")
  .each(function (d, i) {
     var centroid = path.centroid(d);
     alert('Centroid at: ' + centroid[0] + ', ' + centroid[1]);
  }); */

/* var projection = d3.geoMercator()
var path = d3.geoPath()
                .projection(projection)

var state = d3.select(".pos149545")._groups[0];
var centroid = path.centroid(state); */