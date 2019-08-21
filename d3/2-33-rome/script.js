
/* d3.min(emps, function(d) { return +d.reign_start; }) */

// svg area ---------------------------------------------

var margin = {top: 10, right: 30, bottom: 30, left: 40},
    width = 900 - margin.left - margin.right,
    height = 2200 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg = d3.select("#my_dataviz")
  .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// data ---------------------------------------------------

var parseTime = d3.timeParse("%Y-%m-%d");


/* d3.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv").then(emps => { */

    emps.forEach(d => {
        d.reign_start = parseTime(d.reign_start);
        d.reign_end = parseTime(d.reign_end);
    });
    
    // y axis ------------------------------------
    var y = d3.scaleTime()
                .domain([d3.min(emps, d => d.reign_start), d3.max(emps, d => d.reign_end)])
                .range([0, height]);


    // colors
    var colorz = ["#4C63C3", "#DE1288", "#F1C232"]
    /* var colorz2 = colorz.fill(colorz, 0, 68) */
    for (var i = 0; i < 5; i++) {
      var colorz = colorz.concat(colorz)
    };
    
    console.log(colorz)
    /* console.log(colorz2) */

    	//Set the color for each region
      var color = d3.scaleOrdinal()
                      .range( ["#4C63C3", "#DE1288", "#F1C232"])
                      .domain([0, 2]);

    // y values in data --------------------------

    

    emps.map(d => {
        d.ystart = y(d.reign_start);
        d.yend = y(d.reign_end);
        d.r = (y(d.reign_end) - y(d.reign_start))/2;
        d.y = (y(d.reign_end) + y(d.reign_start))/2;})
        d.index2 = d.index%3;
        d.texty = d.index*(height/68);
        d.texty2 = ((d.texty)*3+d.y)/4
    })

    console.log(emps)

    // points (white background) ----------------
    svg.append("g")  
        .selectAll(".circlecolor")
            .data(emps)
            .enter().append("circle")
                .attr("cx", "500")
                .attr("cy", d => d.y)
                .attr("r", d => d.r)
                .attr("fill", "#FFF");

    // points ------------------------------------
    svg.append("g")  
        .selectAll(".circlecolor")
            .data(emps)
            .enter().append("circle")
                .attr("cx", "500")
                .attr("cy", d => d.y)
                .attr("r", d => d.r)
                .attr("fill", d => color(d.index2))
                .attr("fill-opacity", ".6")
                .attr("class", d => d.name);


    // Names -------------------------------------
    svg.append("g")
      .selectAll("empnames")
        .data(emps)
        .enter().append("text")
            .attr("x", "200")
            .attr("y", d => d.texty2)
            .text(d => d.name)
            .attr("font-size", "8px")
            .attr("fill", "#ffffff")
            .attr("class", d => d.name);

    // Lines --------------------------------------
    svg.append("g")
      .selectAll("blines")
      .data(emps)
      .enter().append("line")
        .attr("x1", "250")
        .attr("x2", "450")
        .attr("y1", d => d.texty2)
        .attr("y2", d => d.y)
        .attr("stroke", "#ffffff")
        .attr("width", "3px")
        .attr("class", d => d.name);

// });