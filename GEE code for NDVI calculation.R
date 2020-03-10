//cronstruction of a time series for a given pixel



//image collection Sentinel 2

var collection = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2018-03-01', '2018-07-30')
.filterBounds(test_area)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 0.1)
.select('B[4,8]');



print(ui.Chart.image.series(collection, point_ex1, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection, point_ex2, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection, point_ex3, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection, point_ex4, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection, point_ex5, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection, point_ex6, ee.Reducer.mean(), 1));







var addNDVI = function(image) {
  return image.addBands(image.normalizedDifference(['B8', 'B4']));
};


var collection2 = collection.map(addNDVI).select('nd');
//just positiv values!!
  
  //var bandNames = collection2.bandNames();
//print('Band names: ', bandNames); 


print(ui.Chart.image.series(collection2, point_ex1, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_ex2, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_ex3, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_ex4, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_ex5, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_ex6, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_mjo1, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_mjo2, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_mjo3, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_mjo4, ee.Reducer.mean(), 1));


print(ui.Chart.image.series(collection2, point_south1, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_south2, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_south3, ee.Reducer.mean(), 1));
print(ui.Chart.image.series(collection2, point_south4, ee.Reducer.mean(), 1).setOptions({title: 'test'}));
print(ui.Chart.image.series(collection2, point_south_bar, ee.Reducer.mean(), 1).setOptions({title: 'bar'}));    




/////////////////////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //here starts the relevant script

//two time intervalls 
//1: 10 mars to 15 April
//2: 1 Juni to 30 Juni

// NDVI mean for both periods



var collection1 = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2018-03-01', '2018-04-15')
.filterBounds(test_area)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 1)
.select('B[4,8]');

var collection2 = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2018-06-05', '2018-07-15')
.filterBounds(test_area)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 1)
.select('B[4,8]');





var addNDVI = function(image) {
  return image.addBands(image.normalizedDifference(['B8', 'B4']));
};


var collection1_ndvi = collection1.map(addNDVI).mean();                   
var collection2_ndvi = collection2.map(addNDVI).mean();                         

var ndvi_diff=collection2_ndvi.select('nd').subtract(collection1_ndvi.select('nd'));

Map.addLayer(collection1_ndvi,{min:-1, max:1}, 'ndvi1');
Map.addLayer(collection2_ndvi,{min:-1, max:1}, 'ndvi2');
Map.addLayer(ndvi_diff,{min:-2, max:1}, 'ndvi_diff');


//laser scannings data as mask for "forest"
var tree_laser=tree_hight.gt(50);//.clip(test_ruta_1);
Map.addLayer(tree_hight,{min:-2, max:400}, 'tree hight');
Map.addLayer(tree_laser,{min:0, max:1}, 'tree mask');


var collection1_ndvi_c=collection1_ndvi.multiply(tree_laser);
var collection2_ndvi_c=collection2_ndvi.multiply(tree_laser);
var ndvi_diff_c=ndvi_diff.multiply(tree_laser);
var tree_hight_c=tree_hight.multiply(tree_laser);

Map.addLayer(collection1_ndvi_c,{min:-1, max:1}, 'ndvi1_c');

//EXPORT


var result = collection1_ndvi_c.select('nd');
Export.image.toDrive({
  image: result,
  description: 'ndvi1',
  scale: 10,
  region: test_area,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});

var result = collection2_ndvi_c.select('nd');
Export.image.toDrive({
  image: result,
  description: 'ndvi2',
  scale: 10,
  region: test_area,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});      

var result = ndvi_diff_c.select('nd');
Export.image.toDrive({
  image: result,
  description: 'ndvi_diff',
  scale: 10,
  region: test_area,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});    


var result = tree_hight_c.select('b1');
Export.image.toDrive({
  image: result,
  description: 'tree_hight',
  scale: 10,
  region: test_area,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});    

