
var collection = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2018-03-01', '2018-07-30')
.filterBounds(south_sweden)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 0.1)
.select('B[6]');

var collection2 = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2018-07-01', '2018-7-30')
.filterBounds(south_sweden)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 5)
//.select('B[8a]')
.mean();   



var collection3 = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2019-08-01', '2019-9-30')
.filterBounds(south_sweden)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 7)
//.select('B[8a]')
.mean();        


var collection4 = ee.ImageCollection('COPERNICUS/S2')
.filterDate('2018-10-01', '2018-10-31')
.filterBounds(south_sweden)
.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 5)
//.select('B[8a]')
.mean();   


//print(ui.Chart.image.series(collection, bok_1, ee.Reducer.mean(), 1).setOptions({title: 'bok'}));
//print(ui.Chart.image.series(collection, bok_2, ee.Reducer.mean(), 1).setOptions({title: 'bok'}));
//Map.addLayer(test_data,{color: 'FF0000'},'pyswe')
//Map.addLayer(collection2)
//Map.addLayer(collection3)
//Map.addLayer(collection4)


//var test1=ee.FeatureCollection(test_data)

//var grid_data=collection2.sampleRegions({
  //  collection:test1,
  //  properties:['LC'],
  //  scale:2
  
  //});

//print(grid_data)



//Export.table.toDrive({
  //  collection: grid_data,
  //  description: 'adel8',
  //  fileFormat: 'CSV'
  //});





Export.image.toDrive({
  image: collection2,
  description: 'adellov_juni',
  scale: 10,
  region: south_sweden,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


Export.image.toDrive({
  image: collection3,
  description: 'adellov_juni',
  scale: 10,
  region: south_sweden,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});

Export.image.toDrive({
  image: collection4,
  description: 'adellov_juni',
  scale: 10,
  region: south_sweden,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});
