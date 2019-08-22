
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
    

    /* var time_aug = emps[0].reign_start */
    // ugly fix
    emps[0].reign_start = d3.timeYear.offset(emps[0].reign_start, -26*2);

    console.log(emps[0].reign_start.getFullYear())

    /* d3.timeYear.offset( */

    // y axis ------------------------------------
    var y = d3.scaleTime()
                .domain([d3.min(emps, d => d.reign_start), d3.max(emps, d => d.reign_end)])
                .range([0, height]);


    // colors
    var colorz = ["#4C63C3", "#F1C232", "#DE1288"]
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
        d.y = (y(d.reign_end) + y(d.reign_start))/2;
    })

    var emps2 = emps.sort(function(a,b) { return +a.y - +b.y });
    

    for (i = 0; i < emps.length; i++) {
      emps2[i].index = i + 1;
    }

    emps2.map(d => {
      d.index2 = d.index%3;
      d.texty = d.index*(height/68);
      d.texty2 = ((d.texty)*1+d.y)/2;
      d.name_label = d.name.replace(/ /g, "_") //Not a good idea to use text with whitespace as id
    })

    console.log(emps)
    console.log(emps2)


    // more parameters ---------------------------

    // circle x
    var circle_x = 500;
    console.log(circle_x)

    // circle x
    var text_x = 300;
    console.log(text_x)
 
    // max circle radius
    var max_r = d3.max(emps, d => +d.r);
    console.log(max_r)

    // circle opacity
    var c_opacity = .6

    // line and path size
    var l_size = .2

    // text size
    var t_size = "14px"

    // additional info y
    var text2_x

    // points (white background) ----------------
    svg.append("g")
        .attr("class", "circlebackground")  
        .selectAll()
            .data(emps2)
            .enter().append("circle")
                .attr("cx", `${circle_x}`)
                .attr("cy", d => d.y)
                .attr("r", d => d.r)
                .attr("fill", "#FFF");

    // points ------------------------------------
    svg.append("g")  
        .attr("class", "circlecolor")  
        .selectAll()
            .data(emps2.sort(function(a,b) { return b.r > a.r; })) // smaller circles on top
            .enter().append("circle")
                .attr("cx", `${circle_x}`)
                .attr("cy", d => d.y)
                .attr("r", d => d.r)
                .attr("fill", d => color(d.index2))
                .attr("fill-opacity", c_opacity)
                .attr("id", d => d.name_label)
                .on("mouseover", highlight)
                .on("mouseout", downlight);


    // Names -------------------------------------
    svg.append("g")
      .attr("class", "empnames")
      .selectAll()
        .data(emps2)
        .enter().append("text")
            .attr("x", `${text_x}`)
            .attr("y", d => d.texty2)
            .text(d => d.name + " | " + d.reign_start.getFullYear() + " - " +  d.reign_end.getFullYear())  
            .attr("font-size", t_size)
            .attr("fill", "#ffffff")
            .attr("text-anchor", "end")
            .attr("dominant-baseline", "middle")
            /* .style("text-align", "right") */
            .attr("id", d => d.name_label)
            .on("mouseover", highlight)
            .on("mouseout", downlight);

    // Lines --------------------------------------
    svg.append("g")
      .attr("class", "linkbezier")
      .selectAll()
      .data(emps2)
      .enter().append("path")
        .attr("d", d => {
          return `M 
              ${text_x + 5} ${d.texty2} 
           C 
              ${(circle_x - max_r + text_x)/2} ${d.texty2} 
              ${(circle_x - max_r + text_x)/2} ${d.y} 

              ${circle_x - max_r} ${d.y}`
        })
        .attr("stroke", "#ffffff")
        .attr("fill", "transparent")
        .attr("stroke-width", l_size)
        .attr("id", d => d.name_label)
        .on("mouseover", highlight)
        .on("mouseout", downlight);

     // connection
     svg.append("g")
      .attr("class", "linkline")
      .selectAll()
      .data(emps2)
      .enter().append("line")
        .attr("x1", circle_x - max_r)
        .attr("x2", d => circle_x - d.r)
        .attr("y1", d => d.y)
        .attr("y2", d => d.y)
        .attr("stroke", "#ffffff")
        .attr("fill", "transparent")
        .attr("stroke-width", l_size)
        .attr("id", d => d.name_label)
        .on("mouseover", highlight)
        .on("mouseout", downlight);

/* }); */

// highlight info on selected emperor -----------------------

function highlight(emperor) {

  // Each element has respective emperor name as id
  var id_in = emperor.name_label;
  console.log(id_in)

  // modify circles
  var elements_in = d3.select("circle#" + id_in);
  console.log(elements_in)
  /* elements_in.style("fill-opacity", 1); */
  elements_in
    .attr("fill-opacity", 1)
    .attr("r", d => d.r + 2);


  // thicker lines
  d3.select("line#" + id_in)
      .attr("stroke-width", 1);

  // thicker bezier
  d3.select("path#" + id_in)
  .attr("stroke-width", 1);

  // bigger text
  d3.select("text#" + id_in)
    .attr("font-weight", "bold");
}

function downlight(emperor) {

  // Each element has respective emperor name as id
  var id_in = emperor.name_label;
  console.log(id_in)
  console.log(id_in.replace(" ", "_"))

  // modify circles
  var elements_in = d3.select("circle#" + id_in);
  console.log(elements_in)
  /* elements_in.style("fill-opacity", 1); */
  elements_in
    .attr("fill-opacity", c_opacity)
    .attr("r", d => d.r);

  // thinner lines
  d3.select("line#" + id_in)
      .attr("stroke-width", l_size);
  
  // thinner bezier
  d3.select("path#" + id_in)
      .attr("stroke-width", l_size);

  // bigger text
  d3.select("text#" + id_in)
    .attr("font-weight", "normal");
}
