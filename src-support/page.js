import $ from 'jquery';
require('@fortawesome/fontawesome-free/css/all.css');
require('@fortawesome/fontawesome-free/js/all');
require('bootstrap/dist/css/bootstrap.css');
require("./page.scss");

document.addEventListener('load', () => {
  $('table').addClass("table table-striped table-bordered table-hover table-condensed table-responsive");
});
