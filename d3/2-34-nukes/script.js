
// Set page parameters -------------------------------------------------

var margin = { top: 10, right: 30, bottom: 30, left: 20 },
  width = 1700 - margin.left - margin.right,
  height = 1000 - margin.top - margin.bottom,
  offset = width * 0.05;

var bluefill = "#3752C3",
  violetfill = "#B9239B";

// country name mapping

var country_names = {
  USA: "USA",
  RUS: "USSR",
  FRA: "FRANCE",
  CHN: "CHINA",
  IND: "INDIA",
  PAK: "PAKISTAN",
  AUS: "AUSTRALIA",
}


// append the svg object to the body of the page --------------------------

var svg = d3.select("#my_dataviz")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform", `translate(${margin.left + offset}, ${margin.top})`);


// Function that renders world borders and data --------------------

const render = (world, nukes) => {

  // set projection
  var projection = d3.geoAirocean()
    .fitExtent([[0, 0], [width - offset, height]], world);

  // Set SVG generator on projection
  var geoGenerator = d3.geoPath()
    .projection(projection);


  // set graticule
  var graticule = d3.geoGraticule();

  // This extracts centroids and folds from the projection object -----------------------
  // from: https://observablehq.com/@fil/airocean-projection

  // Polyhedral projections expose their structure as projection.tree()

  // iterate on the tree of faces to get all sites and folds
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

  // clean misplaced fold line -----------------------

  folds3.features = folds3.features.filter(d => (d.geometry.coordinates[1][0] > -36 || d.geometry.coordinates[1][1] > -30));

  // 0: Array [ 0, -26.56505117707799 ]
  // 1: Array [ -36, -31.717474411461016 ]


  //select nuclear countries for centroids
  //var countries_in = ["USA", "RUS", "FRA", "IND", "CHN", "PAK", "AUS"];
  /*   var countries_in = ["USA"]
  
    var countries_in = world.features.filter((d, i) => countries_in.indexOf(d.id) >= 0);
  
    // Make new JSON only with those features
    var country_centr = {
      type: "FeatureCollection",
      features: countries_in
    };
  
    console.log(world)
    console.log(country_centr)
    console.log(country_centr.features.map(geoGenerator.centroid))
  
    var usa_centroid = country_centr.features.map(geoGenerator.centroid)
  
    // select only USA detonations
    var nukes_usa = nukes.filter(d => d.country == "USA")
    console.log(nukes_usa) */

  // draw map borders --------------------------------
  svg.append("g")
    .attr("class", "mapborder")
    .append("path")
    .attr('d', geoGenerator({ type: "Sphere" }))
    .attr("fill", "#263A89")
    .attr("stroke", "white")
    .attr("stroke-width", "1px");

  // Render Graticule -------------------------------------
  svg.append("g")
    .attr("class", "graticule")
    .append("path")
    .attr('d', geoGenerator(graticule()))
    .attr('stroke', '#E8EDEF')
    .attr('fill', 'transparent')
    .attr("stroke-width", "0.1mm");


  // plot fold lines -----------------------------------

  // To draw them we need to cancel the rotate
  var rotate = projection.rotate();

  projection.rotate([0, 0, 0]);

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

  // Render Countries -------------------------------------
  svg.append("g")
    .attr("class", "countries")
    .selectAll('path')
    .data(world.features)
    // Create path elements and update the d attribute using the geo generator
    .enter()
    .append('path')
    .attr('d', geoGenerator)
    .attr("fill", bluefill)
    .attr("stroke", "#263A89")
    .attr("stroke-width", "1px")
    .attr("id", d => d.id)
    .on("mouseover", showlink)
    .on("mouseout", hidelink);

  // dots for nukes ---------------------------------
  svg.append("g")
    .attr("class", "nukedots")
    .selectAll()
    .data(nukes)
    .enter().append("circle")
    .attr("cx", d => projection([d.longitude, d.latitude])[0])
    .attr("cy", d => projection([d.longitude, d.latitude])[1])
    .attr("r", "2")
    .style("fill", "#E9E95C")
    .attr("fill-opacity", .8);

  function showlink(country) {

    console.log(country)

    var id_in = country.id

    console.log(id_in)

    // color selected country violet
    d3.select("path#" + id_in)
      .attr("fill", violetfill)


    // find centroid of select country
    country_centroid = geoGenerator.centroid(country)

    // select detonations by selecte country
    var country_nukes = nukes.filter(d => d.country == country_names[id_in])
    console.log(country_nukes)

    // line connecting to origin country ---------------
    svg.append("g")
      .attr("class", "nukelink")
      .selectAll()
      .data(country_nukes)
      .enter().append("line")
      .attr("x1", country_centroid[0])
      .attr("y1", country_centroid[1])
      .attr("x2", d => projection([d.longitude, d.latitude])[0])
      .attr("y2", d => projection([d.longitude, d.latitude])[1])
      .attr("stroke", "#ffffff")
      .attr("fill", "transparent")
      .attr("stroke-width", "0.6px");

  }


  function hidelink(country) {

    var id_in = country.id

    console.log(id_in)

    // color selected country blue
    d3.select("path#" + id_in)
      .attr("fill", bluefill)


    d3.select(".nukelink").remove()


  }

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

  console.log(nukes)

  // tidy array ----------------------------
  var parseTime = d3.timeParse("%Y%m%d");

  nukes.forEach(d => {
    d.date_long = parseTime(d.date_long);
  })

  render(world, nukes);

});