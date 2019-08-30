
// Set page parameters -------------------------------------------------

var margin = { top: 10, right: 30, bottom: 30, left: 40 },
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


// Function that renders world borders and data --------------------

const render = (world, nukes) => {

  // set projection
  var projection = d3.geoAirocean()
    .fitExtent([[0, 0], [width, height]], world);

  // Set SVG generator on projection
  var geoGenerator = d3.geoPath()
    .projection(projection);


  // set graticule
  var graticule = d3.geoGraticule();

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

  // This extracts centroids and folds from the projection object -----------------------
  // from: https://observablehq.com/@fil/airocean-projection

  // Polyhedral projections expose their structure as projection.tree()
  // To draw them we need to cancel the rotate

  var rotate = projection.rotate();
  projection.rotate([0, 0, 0]);

  // run the tree of faces to get all sites and folds
  var sites = [], folds = [], i = 0;
  function recurse(face) {
    var site = d3.geoCentroid({ type: "MultiPoint", coordinates: face.face });

    site.id = face.id || i++;
    sites.push(site);
    if (face.children) {
      face.children.forEach(function (child) {
        folds.push({
          type: "LineString",
          coordinates: child.shared
        });
        recurse(child);
      });
    }
  }
  recurse(projection.tree());

  // reproject the folds???? ----------------------------

  var fold_feature = []

  folds.forEach(fd => {
    fold_feature.push({
      type: "Feature",
      geometry: fd
    })
  })

  

  const folds3 = {
    type: "FeatureCollection",
    features: fold_feature
  }

  // clean misplaced fold line
  folds3.features = folds3.features.filter(d => (d.geometry.coordinates[1][0] > -36 || d.geometry.coordinates[1][1] > -30));

  // 0: Array [ 0, -26.56505117707799 ]
  // 1: Array [ -36, -31.717474411461016 ]


  // plot fold lines
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

  // draw map borders --------------------------------

  svg.append("g")
    .attr("class", "mapborder")
    .append("path")
    .attr('d', geoGenerator({ type: "Sphere" }))
    .attr("fill", "transparent")
    .attr("stroke", "white")
    .attr("stroke-width", "3px");

  // dots for nukes
  svg.selectAll("nukeDots")
    .data(nukes)
    .enter().append("circle")
    .attr("cx", d => projection([d.longitude, d.latitude])[0])
    .attr("cy", d => projection([d.longitude, d.latitude])[1])
    .attr("r", "4")
    .style("fill", "#E9E95C")
    .attr("fill-opacity", .4)


}


// Read data and render everything -------------------------------

// Multiple CSV/JSON files at once

const nukes_url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv"


Promise.all([
  d3.json("https://enjalot.github.io/wwsd/data/world/world-110m.geojson"),
  d3.csv(nukes_url)
]).then(files => {

  var world = files[0];
  var nukes = files[1];

  // tidy array ----------------------------
  var parseTime = d3.timeParse("%Y%m%d");

  nukes.forEach(d => {
    d.date_long = parseTime(d.date_long);
  })

  render(world, nukes);

});