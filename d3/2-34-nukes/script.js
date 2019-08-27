


var margin = {top: 10, right: 30, bottom: 30, left: 40},
width = 1700 - margin.left - margin.right,
height = 1200 - margin.top - margin.bottom;

// append the svg object to the body of the page --------------------------
var svg = d3.select("#my_dataviz")
.append("svg")
.attr("width", width + margin.left + margin.right)
.attr("height", height + margin.top + margin.bottom)
.append("g")
.attr("transform",
      "translate(" + margin.left + "," + margin.top + ")");


// Function that renders world borders -----------------------------

const render = world => {
console.log(world)

// set projection
var projection = d3.geoAirocean()
                 .fitExtent([[0, 0], [width, height]], world);

console.log(projection.tree(4))

// Set SVG generator on projection
var geoGenerator = d3.geoPath()
                   .projection(projection);


// set graticule
var graticule = d3.geoGraticule();

console.log(geoGenerator(graticule()))

// Join the FeatureCollection's features array to path elements
svg.append("g")
.attr("class", "countries")
.selectAll('path')
.data(world.features)
// Create path elements and update the d attribute using the geo generator
.enter()
.append('path')
.attr('d', geoGenerator)
.attr("fill", "white")
.attr("stroke", "#3752C3")
.attr("stroke-width", "1px");


svg.append("g")
    .attr("class", "graticule")
    .append("path")
    .attr('d', geoGenerator(graticule()))
    .attr('stroke', '#E8EDEF')
    .attr('fill', 'transparent')
    .attr("stroke-width", "0.1mm");

/* svg.append("g")
.selectAll('path')
.data(world.features)
.enter().append("path")
.attr('d', geoGenerator(graticule()))
.attr('stroke', '#E8EDEF')
.attr('fill', 'transparent')
.attr("stroke-width", "0.005mm"); */

// This extracts centroids and folds from the projection object -----------------------
// from: https://observablehq.com/@fil/airocean-projection

// run the tree of faces to get all sites and folds
var sites = [], folds = [], i = 0;
function recurse(face) {
  var site = d3.geoCentroid({type:"MultiPoint", coordinates:face.face});
  // console.log(face.face)
  // console.log(site)
  site.id = face.id || i++;
  sites.push(site);
  if (face.children) {
    face.children.forEach(function(child) {
      folds.push({
        type:"LineString",
        coordinates: child.shared.map(
          e => d3.geoInterpolate(e, face.centroid)(1e-5)
        )
      });
      recurse(child);
    });
  }
  // console.log(site)
}
recurse(projection.tree());

console.log(sites)
console.log(folds)

const folds2 = []

/* folds.forEach(fd => {
  fd.coordinates.forEach(fc => {
    folds2.push({
      p1: projection(fc[0]),
      p2: projection(fc[1])
    });
  });
}); */

folds.forEach(fd => {
  folds2.push({
    p1: projection(fd.coordinates[0]),
    p2: projection(fd.coordinates[1])
  });
});

console.log(folds2)

const folds3 = {type: "FeatureCollection",
                            features: folds}
console.log(folds3) 

console.log(geoGenerator)

// var folds_flat = folds.forEach

svg.append("g")
    .attr("class", "foldline")
    //.attr("translate(" + margin.left + "," + margin.top + ")")
    .selectAll()
    .data(folds)
    .enter().append("line")
    .attr("x1", d => d.coordinates[0][0] + 300)
    .attr("y1", d => d.coordinates[0][1] + 300)
    .attr("x2", d => d.coordinates[1][0] + 300)
    .attr("y2", d => d.coordinates[1][1] + 300)
    .attr("stroke", "red")
    .attr("stroke-width", "4px");
   


/* svg.append("g")
    .attr("class", "foldline")
    .selectAll()
    .data(folds2)
    .enter().append("line")
    .attr("x1", d => d.p1[0])
    .attr("y1", d => d.p1[1])
    .attr("x2", d => d.p2[0])
    .attr("y2", d => d.p2[1][1])
    .attr("stroke", "#ffffff")
    .attr("stroke-width", "4px"); */

}

// Render world borders

d3.json("https://enjalot.github.io/wwsd/data/world/world-110m.geojson").then(world => {

render(world)

})
