library(data.table)
library(rMaps)
library(dplyr)
library(magrittr)
library(countrycode)
library(pings)
library(xts)
load("tarde.data")
str(x)
xs <- x %>% 
  mutate(YearMonth=as.yearmon(Date)) %>% 
  group_by(YearMonth, ISO3C) %>% 
  summarize(Amount=floor(sum(Amount)/10^4))
xs <- xs %>% 
  tally %>% 
  select(YearMonth) %>%
  mutate(Counter=row_number(YearMonth)) %>%
  inner_join(y=xs)
head(xs)
min.date <- xs %>% 
  use_series(YearMonth) %>% 
  as.Date %>% 
  min %>% 
  as.character
max.counter <- xs %>% 
  use_series(Counter) %>%
  max
min.date
max.counter
d <- ichoropleth(log(Amount) ~ ISO3C, data=as.data.frame(xs), animate="Counter", map="world")
d$setTemplate(chartDiv = sprintf("
  <div class='container'>
    <button ng-click='animateMap()'>Play</button>
    <span ng-bind='date_show'></span>
    <div id='{{chartId}}' class='rChart datamaps'></div>
  </div>
  <script src='http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js'></script>
  <script src='http://ajax.googleapis.com/ajax/libs/jqueryui/1/jquery-ui.min.js'></script>
  <script>
    function rChartsCtrl($scope, $timeout){
      $scope.counter = 1;
      $scope.date = new Date('%s');
      $scope.date_show = $.datepicker.formatDate('yy-mm', $scope.date);
      $scope.animateMap = function(){
        if ($scope.counter > %s){
          return;
        }
        map{{chartId}}.updateChoropleth(chartParams.newData[$scope.counter]);
        $scope.counter += 1;
        $scope.date.setMonth($scope.date.getMonth()+1);
        $scope.date_show = $.datepicker.formatDate('yy-mm', $scope.date);
        $timeout($scope.animateMap, 1000)
      }
    }
  </script>", min.date, max.counter)
)
d
