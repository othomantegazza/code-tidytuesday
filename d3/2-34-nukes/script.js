
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

// Polyhedral projections expose their structure as projection.tree()
  // To draw them we need to cancel the rotate
  
 var rotate = projection.rotate();
 projection.rotate([0,0,0]);


console.log("rotate")
console.log(rotate)
console.log(projection.rotate())

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
        coordinates: child.shared/* .map(
          e => d3.geoInterpolate(e, face.centroid)(1e-5)
        ) */
      });
      recurse(child);
    });
  }
  // console.log(site)
}
recurse(projection.tree());

console.log(sites)
console.log("folds")
console.log(folds)

// reproject the folds???? ----------------------------

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

var fold_feature = []

folds.forEach(fd => {
  fold_feature.push({type: "Feature",
                     geometry: fd})
})

/* fold_feature.forEach(ff => {
  ff.geometry.type = "Polygon"
})
 */
console.log("fold_feature")
console.log(fold_feature)

const folds3 = {type: "FeatureCollection",
                features: fold_feature}

/* folds.forEach(fd => {
  folds3.features.push({feature: fd})
}) */
                            
console.log("folds3")
console.log(folds3)

console.log("geogen folds3")
console.log(geoGenerator(folds3))
console.log(geoGenerator(world))

// var folds_flat = folds.forEach

/* svg.append("g")
    .attr("class", "foldline")
    //.attr("translate(" + margin.left + "," + margin.top + ")")
    .selectAll()
    .data(folds)
    .enter().append("line")
    .attr("x1", d => d.coordinates[0][0])
    .attr("y1", d => d.coordinates[0][1])
    .attr("x2", d => d.coordinates[1][0])
    .attr("y2", d => d.coordinates[1][1])
    .attr("stroke", "red")
    .attr("stroke-width", "4px"); */
   


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

  svg.append("g")
    .attr("class", "foldline")
    .selectAll('path')
    .data(folds3.features)
// Create path elements and update the d attribute using the geo generator
    .enter()
    .append('path')
    .attr('d', geoGenerator)
    .attr("fill", "transparent")
    .attr("stroke", "#100089")
    .attr("stroke-dasharray", "10,10")
    .attr("stroke-width", "2px");


// restore the projection’s rotate
projection.rotate(rotate);

console.log("path-sphere")
console.log(geoGenerator({type: "Sphere"}))

// draw map borders
svg.append("g")
  .attr("class", "mapborder")
  .append("path")
  .attr('d', geoGenerator({type: "Sphere"}))
  .attr("fill", "transparent")
  .attr("stroke", "white")
  .attr("stroke-width", "10px");


}

// Render world borders

d3.json("https://enjalot.github.io/wwsd/data/world/world-110m.geojson").then(world => {

render(world)

})
