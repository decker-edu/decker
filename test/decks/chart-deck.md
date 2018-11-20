---
chart: true
---

# Bar Chart One

<canvas data-chart="bar">
<!--
{
 "data": {
  "labels": ["January"," February"," March"," April"," May"," June"," July", "August", "September", "October", "November", "December"],
  "datasets": [
   {
    "data":[65,59,80,81,56,55,40,45,49,58,68,70],
    "label":"James Smith",
    "backgroundColor":"rgba(20,220,220,.8)"
   },
   {
    "data":[28,48,40,19,86,27,90,65,60,45,40,35],
    "label":"Derek Jones",
    "backgroundColor":"rgba(220,120,120,.8)"
   }
  ]
 },
 "options": { "responsive": "true" }
}
-->
</canvas>

# Bar Chart Two

<canvas data-chart="bar">
<!--
{
  "data": {
    "labels": ["January","February","March","April","May","June"],
    "datasets": [
    {
      "label": "First Half Sales Report",
      "data":[65,59,80,81,56,55],
      "fill":false,
      "backgroundColor":[
        "rgba(255, 99, 132, 0.2)",
        "rgba(255, 159, 64, 0.2)",
        "rgba(255, 205, 86, 0.2)",
        "rgba(75, 192, 192, 0.2)",
        "rgba(54, 162, 235, 0.2)",
        "rgba(153, 102, 255, 0.2)"
      ],
      "borderColor": [
        "rgb(255, 99, 132)",
        "rgb(255, 159, 64)",
        "rgb(255, 205, 86)",
        "rgb(75, 192, 192)",
        "rgb(54, 162, 235)",
        "rgb(153, 102, 255)"
      ],
      "borderWidth":1
    }]
  },
  "options":
    {
      "scales":
      {
        "yAxes": [
          { "ticks": {"beginAtZero":true} }
        ]
      }
    }
}
-->
</canvas>

# Line Chart One

<canvas data-chart="line">
<!--
{
 "data": {
  "labels": ["January"," February"," March"," April"," May"," June"," July"],
  "datasets": [
   {
    "data":[65,59,80,81,56,55,40],
    "label":"My first dataset","backgroundColor":"rgba(20,220,220,.8)"
   },
   {
    "data":[28,48,40,19,86,27,90],
    "label":"My second dataset","backgroundColor":"rgba(220,120,120,.8)"
   }
  ]
 },
 "options": { "responsive": "true" }
}
-->
</canvas>

# Line Chart 2 - Partial Data

<canvas class="stretch" data-chart="line">My first dataset, 65, 59, 80, 81, 56, 55, 35   
My second dataset, 28, 48, 40, 19, 86, 27, 90
<!--
{
 "data" : {
  "labels": ["January"," February"," March"," April"," May"," June"," July"],
  "datasets" : [{ "borderColor": "#0f0", "borderDash": ["5","10"] }, { "borderColor": "#0ff" } ]
 }
}
-->
</canvas>

# Only Data Example

<canvas data-chart="line">January,February,March,April,May,June,July,August,September,October,November,December

James,65,59,80,81,56,55,40,45,49,58,68,70

Dereck,28,48,40,19,86,27,90,65,60,45,40,35

</canvas>
