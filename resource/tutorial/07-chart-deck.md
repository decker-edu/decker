---
title: Chart Deck
subtitle: Usage of the Chart Plugin
chart: true
---


# Charts from JSON Strings

A chart can be included in a slide by adding a canvas element to the `*-deck.md` file  with the data-chart attribute set to the desired chart type.

The chart can be configured within the canvas body by a JSON string embedded into an HTML comment.

For further information see: [https://github.com/rajgoel/reveal.js-plugins/tree/master/chart](https://github.com/rajgoel/reveal.js-plugins/tree/master/chart)


# Bar Chart

``` bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```


# Bar Chart with Error Bars

``` bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
    "data": {
        "datasets":[
        {
            "errorBars": {
                "January": { "plus": 15, "minus":  3 },
                "April":   { "plus":  5, "minus": 24 },
                "May":     { "plus":  3, "minus": 14 },
                "August":  { "plus": 10, "minus":  4 }
            }
        },    {
            "errorBars": {
                "February": { "plus": 15, "minus":  3 },
                "March":    { "plus":  5, "minus": 24 },
                "May":      { "plus":  3, "minus": 14 },
                "June":     { "plus": 10, "minus":  4 }
            }
        }  ]
    }
}
-->
```


# Specify Color Schemes

``` bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
    "options": {
        "plugins": { 
            "colorschemes": { "scheme": "brewer.SetOne9" }
        }
    }
}
-->
```


# Specify Individual Colors

``` bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
  "data": {
    "datasets":[
    {
      "borderColor": "rgba(100,0,0,1.0)",
      "backgroundColor": "rgba(255,0,0,0.7)"
    },    
    {
      "borderColor": "rgba(0,100,0,1.0)",
      "backgroundColor": "rgba(0,255,0,0.7)"
    } ]
  }
}
-->
```


# Stacked Bar Chart

``` bar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
<!--
{
    "options": {
        "scales": {
            "xAxes": [{ "stacked": true }],
            "yAxes": [{ "stacked": true }]
        }
    }
}
-->
```


# Horizontal Bar Chart

``` horizontalBar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```


# Filled Line Chart

``` line-chart
January, February, March, April, May, June, July, August, September, October, November, December
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```


# Empty Line Chart

``` line-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, -52.0,59.0,-61.0,-80.0,56.0,-75.0,-40.0,45.0,-49.0,58.0,-68.0,70.0
Derek Jones, 98.0,-38.0,82.0,-54.0,-34.0,27.0,90.0,-36.0,60.0,-45.0,40.0,35.0
<!--
{
  "data": {
    "datasets":[
    {
      "backgroundColor": "rgba(255,255,255,0)"
    },    
    {
      "backgroundColor": "rgba(255,255,255,0)"
    } ]
  }
}
-->
```


# Radar Chart

``` radar-chart
January, February, March, April, May, June, July, August, September, October, November, December
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0, 40.0, 45.0, 49.0, 58.0, 68.0, 70.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0, 90.0, 65.0, 60.0, 45.0, 40.0, 35.0
```


# Doughnut Chart

``` doughnut-chart
January, February, March, April, May, June
James Smith, 65.0, 59.0, 80.0, 81.0, 56.0, 55.0
Derek Jones, 28.0, 48.0, 40.0, 19.0, 86.0, 27.0
```


# Pie Chart

``` pie-chart
January, February, March, April, May
James Smith, 25.0, 9.0, 18.0, 20.0, 27.0
Derek Jones, 15.0, 33.0, 8.0, 34.0, 10.0
```


# Polar Area Chart

``` polarArea-chart
January, February, March, April, May
James Smith, 25.0, 9.0, 18.0, 20.0, 27.0
```
