Plotly.d3.csv("https://raw.githubusercontent.com/cieb-unibas/female_inventors/main/Report/graph_gender_techgroup/female_inventors_graduates_techgroup_USPTO.csv", function(err, rows){
function unpack(rows, key) {
        return rows.map(function(row) { return row[key]; });
    }

var     allTech = unpack(rows, 'tech_group'),
        femGrad = unpack(rows, 'female_share_graduates'),
        femInv = unpack(rows, 'female_share_inventors'),
        invCtry = unpack(rows, 'inv_ctry'),
        allcountryName = unpack(rows, 'country'),
        listofCountries = [],
        currentCountry = [],
        currentFemInv = [],
        currentFemGrad = [],
        currentInv = [],
        currentcountryName = [];


    for (var i = 0; i < allTech.length; i++ ){
        if (listofCountries.indexOf(allTech[i]) === -1 ){
            listofCountries.push(allTech[i]);
        }
    }

    function getCountryData(chosenCountry) {
        currentFemInv = [];
        currentFemGrad = [];
        currentInv = [];
        currentcountryName = [];
        for (var i = 0 ; i < allTech.length ; i++){
            if ( allTech[i] === chosenCountry ) {
                currentFemInv.push(femInv[i]);
                currentFemGrad.push(femGrad[i]);
                currentInv.push(invCtry[i]);
                currentcountryName.push(allcountryName[i]);
                }
        }
    };

    // Default Country Data
    setBubblePlot('Overall');

    function setBubblePlot(chosenCountry) {
        getCountryData(chosenCountry);

        var trace1 = {
            x: currentFemGrad,
            y: currentFemInv,
            type: 'scatter',
            mode: 'markers+text',
            hovertemplate:  '%{y}' + '<extra></extra>',
            marker: { color:  ['#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         'red',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f']},
            text: currentInv,
            textposition: 'bottom center'
        };
        
        var trace2 = {
            x: currentFemGrad,
            y: currentFemInv,
            mode: 'markers',
            hovertemplate: '<i>Country</i>: %{text}' + '<extra></extra>',
            marker: { color:  ['#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         'red',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f']},
                         text: currentcountryName,
            textposition: 'bottom center',
            type: 'scatter'
        };

        var data = [trace1, trace2];

        var layout = {
          showlegend: false,
          scrollZoom: false,
    xaxis: {fixedrange: true,
            zeroline: false,
            tickformat: ',.1%', 
            title: {text: '<b>Female Graduate Share in STEM Fields</b>'}},
    yaxis: {fixedrange: true,
            zeroline: false,
            tickformat: ',.1%',
            title: {text: '<b>Female Inventor Share</b>'}},
    shapes: [
{
      type: 'line',
      x0: math.mean([currentFemGrad]),
      x1: math.mean([currentFemGrad]),
      y0: math.min([currentFemInv]),
      y1: math.max([currentFemInv]),
      line: {dash: 'dot'}
    },
    {
      type: 'line',
      x0: math.min([currentFemGrad]),
      y0: math.mean([currentFemInv]),
      x1: math.max([currentFemGrad]),
      y1: math.mean([currentFemInv]),
      line: {dash: 'dot'}
    }
    ]
};


Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});
    };

    var innerContainer = document.querySelector('[data-num="0"'),
        countrySelector = innerContainer.querySelector('.countrydata');

    function assignOptions(textArray, selector) {
        for (var i = 0; i < textArray.length-1;  i++) {
            var currentOption = document.createElement('option');
            currentOption.text = textArray[i];
            selector.appendChild(currentOption);
        }
    }

    assignOptions(listofCountries, countrySelector);

    function updateCountry(){
        setBubblePlot(countrySelector.value);
    }

    countrySelector.addEventListener('change', updateCountry, false);
});    



var inputData = {
  A: [69, 69, 65, 61, 59, 61, 53, 68],
  B: [70, 70, 65, 60, 57, 65, 52, 68],
  C: [66, 66, 64, 59, 57, 59, 57, 66],
  D: [61, 59, 59, 61, 61, 61, 57, 62],
  E: [59, 56, 57, 61, 61, 59, 57, 60],
  F: [56, 57, 55, 57, 55, 52, 54, 55],
  G: [49, 47, 49, 53, 53, 53, 59, 48],
  H: [68, 68, 64, 61, 60, 62, 60, 68],
  I: [
    "Agnostic female",
    "Atheist female",
    "Buddhist female",
    "Catholic female",
    "Protestant female",
    "Hindu female",
    "Muslim female",
    "Jewish female"
  ],
  J: [
    "Agnostic male",
    "Atheist male,Buddhist male",
    "Catholic male",
    "Protestant male",
    "Hindu male",
    "Muslim male",
    "Jewish male"
  ]
};

// add options to select
$.each(inputData, function(key, value) {
  var option = '<option value="' + key + '">' + key + "</option>";
  $("select").append(option);
});

$("select").selectpicker();

$("select").change(function() {
  var that = $(this);
  var id = that.attr("id");
  var val = that.val();
  var data;
  if ($.isArray(val)) {
    data = [];
    $.each(val, function() {
      data.push(inputData[this]);
    });
  } else {
    data = inputData[val];
  }
  var update = {};
  update[id] = [data];
  Plotly.restyle("myDiv", update, 0);
});
//
// init Plotly
var data = [{ type: "heatmap" }];
Plotly.newPlot("myDiv", data, {},{displayModeBar: false});