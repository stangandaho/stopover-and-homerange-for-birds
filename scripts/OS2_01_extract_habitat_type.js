// Load Land Use Land Cover collection
var lulc  = ee.ImageCollection("GOOGLE/DYNAMICWORLD/V1")
          .filter(ee.Filter.date("2024-01-01", "2024-06-01"))
          .select("label");
         
// Get unique individual
var hr = ee.FeatureCollection("projects/ee-vulture/assets/all_hr");
//var allInd =  hr.aggregate_array('individual').distinct()

// Extract lulc per polygon
var extracVal = function(feature){
  var image = lulc.reduce(ee.Reducer.mode()).rename('LULC');
  
  var value = image.reduceRegion({
    reducer: ee.Reducer.mode(),
    geometry: feature.geometry(),
    scale: 10,
    maxPixels:1e16
    }
    
    )
  return feature.set({"LULC": ee.Number(value.get('LULC')).int()})
}

var allHrLULC = hr.map(extracVal)

// Export LULC
Export.table.toDrive({
  collection: allHrLULC,
  description: 'ExportLULC',
  folder: 'Vulture Danger Zone',
  fileNamePrefix: 'hrLULC',
  fileFormat: "CSV"
});

print(allHrLULC)

var VIS_PALETTE = [
    '419bdf', '397d49', '88b053', '7a87c6', 'e49635', 'dfc35a', 'c4281b',
    'a59b8f', 'b39fe1'];

Map.addLayer(lulc, {min:0, max:8, palette: VIS_PALETTE}, "LULC");
Map.addLayer(hr, {color: "red"}, "HR");
