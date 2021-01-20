
/**
 * This code was used to caclulate forest change over time in Myanmar.
 */

// 0. Imports
/**
 * The first three are raster files containing forest establishment, gain, or loss.
 * The rest of the files are polygons or points of Myanamar Administrative Areas.
 */

var establishmentYear = ee.Image("users/jessbros/forest_esta_mmr_fixed"),
    gainYear = ee.Image("users/jessbros/forest_gain_mmr_fixed"),
    lossYear = ee.Image("users/jessbros/forest_loss_mmr_fixed"),
    state = ee.FeatureCollection("users/jessbros/myanmar_state_region_boundaries"),
    township = ee.FeatureCollection("users/jessbros/myanmar/mmr_adm3_township_2020"),
    VT_tni = ee.FeatureCollection("users/jessbros/myanmar/mmr_adm4_VT_tni_unhcr_mimu_2020"),
    VT_kch = ee.FeatureCollection("users/jessbros/myanmar/mmr_kch_polbnda_adm4_250k_mimu"),
    VT_kyn = ee.FeatureCollection("users/jessbros/myanmar/mmr_kyn_polbnda_adm4_250k_unhcr_mimu"),
    VT_mon = ee.FeatureCollection("users/jessbros/myanmar/mmr_adm4_VT_mon_unicef_mimu_2020"),
    Village_kachin = ee.FeatureCollection("users/jessbros/myanmar/village_pts_kachin_state"),
    Village_karen = ee.FeatureCollection("users/jessbros/myanmar/village_pts_karen_state"),
    Village_tanin = ee.FeatureCollection("users/jessbros/myanmar/village_pts_tanintharyi_region");

// 1. Pre-processing data
var lossYear = lossYear.unmask(0);
var gainYear = gainYear.unmask(0);

// 2. OPTIONS TO CHANGE
var min_year = 1989 - 1970;
var max_year = 2018 - 1970;

/**
 * 3. Creating Base Line Map (min_year - 1)
 *
 * Base Line Map (min_year - 1) to establish the first year's changes
 *
 * This is...
 * forest established before the minimum year+
 * forest gain before minimum year +
 * forest loss after minimum year
 */

// Persistent or Establishment pre-min_year
var esta_min_year = establishmentYear.lt(min_year);
//Map.addLayer(esta_min_year, {}, "Established before min_year", false);

// Gain pre-min_year
var gain_min_year = gainYear.gt(0).and(gainYear.lt(min_year));
//Map.addLayer(gain_min_year, {}, "Gain before min_year", false);

// Loss post-min_year
var loss_post_min_year = lossYear.gt(min_year);
//Map.addLayer(loss_post_min_year, {}, "Loss after min_year", false);

// Combine it all together
var image_base_mask = esta_min_year.or(gain_min_year).or(loss_post_min_year);
//Map.addLayer(image_base_mask, {}, "Forest at min_year - 1");



/**
 * How we create our base year maps
 *
 * From our base line map, we loop our maps through a function through all the years
 * so that the result of the output is the input of the next output.
 *
 * The function is, for each year:
 * output = input + establishment + gain - loss
 *
 */
// List of years for use in mapping
var year_list = ee.List.sequence(min_year, max_year, 1);

// List of base forest images
var base_forest_list = ee.List([image_base_mask.set({
    year_processed: min_year - 1
})]);

// Function to generate the rest of the base images
var addYearBaseForest = function(year, image_list) {
    year = ee.Number(year);
    image_list = ee.List(image_list);
    var image_pre = ee.Image(image_list.get(-1));
    var image_current = image_pre.and(lossYear.eq(year).not()).or(establishmentYear.eq(year)).or(gainYear.eq(year));
    image_current = image_current.set({
        year_processed: year
    });
    image_list = image_list.add(image_current);
    return image_list;
};

// We're iterating over the years and adding each newly created image to the base_forest_list
base_forest_list = ee.List(year_list.iterate(addYearBaseForest, base_forest_list));
print("Base Forest List", base_forest_list);



// Function used to analyze forest change at a given year
var addYearAnalysis = function(feat) {
    var year = ee.Number.parse(feat.get("year_processed"));
    var image_base_mask = ee.Image(base_forest_list.filter(ee.Filter.eq("year_processed", year)).get(0));
    var year_LossArea = lossYear.eq(year).multiply(ee.Image.pixelArea());
    var year_GainArea = gainYear.eq(year).multiply(ee.Image.pixelArea());
    var year_BaseArea = ee.Image(base_forest_list.filter(ee.Filter.eq("year_processed", min_year)).get(0)).multiply(ee.Image.pixelArea());
    // Area = Base area - Loss Area + Gain Area
    var year_ForestArea = image_base_mask.multiply(ee.Image.pixelArea());
    return feat.set({
        totalForestArea: year_ForestArea.reduceRegion({
            reducer: ee.Reducer.sum(),
            geometry: feat.geometry(), // plug in the region ROI of interest
            scale: 30,
            maxPixels: 1e9
        }).get('b1')
    }).set({
        totalForestLoss: year_LossArea.reduceRegion({
            reducer: ee.Reducer.sum(),
            geometry: feat.geometry(), // plug in the region ROI of interest
            scale: 30,
            maxPixels: 1e9
        }).get('b1')
    }).set({
        totalForestGain: year_GainArea.reduceRegion({
            reducer: ee.Reducer.sum(),
            geometry: feat.geometry(), // plug in the region ROI of interest
            scale: 30,
            maxPixels: 1e9
        }).get('b1')
    }).set({
        totalForestBase: year_BaseArea.reduceRegion({
            reducer: ee.Reducer.sum(),
            geometry: feat.geometry(), // plug in the region ROI of interest
            scale: 30,
            maxPixels: 1e9
        }).get('b1')
    });
};

/**
 * 4. Village Processing
 * 
 * For village points, we create circular buffers of 1000m, 3000m, and 5000m 
 * We then find the forest area loss and gain.
 */

var village_processing = function(points) {
    points = ee.FeatureCollection(points);

    var buffer_1k = points.map(function(pt) {
        return pt.buffer(1000).set({
            bufferSize: 1000
        });
    });

    var buffer_3k = points.map(function(pt) {
        return pt.buffer(3000).set({
            bufferSize: 3000
        });
    });

    var buffer_5k = points.map(function(pt) {
        return pt.buffer(5000).set({
            bufferSize: 5000
        });
    });

    var combine_buffer = buffer_1k.merge(buffer_3k).merge(buffer_5k);
    var combine_buffer_year = ee.FeatureCollection(year_list.map(
        function(year) {
            return combine_buffer.map(function(feat) {
                return feat.set({
                    year_processed: year
                });
            });
        })).flatten();
    var combine_buffer_result = combine_buffer_year.map(addYearAnalysis);
    return combine_buffer_result;

};
var vill_kachin_processed = village_processing(Village_kachin);
var vill_karen_processed = village_processing(Village_karen);
var vill_tanin_processed = village_processing(Village_tanin);
//print("vill_kachin_pt", vill_kachin_pt, "vill_karen_pt", vill_karen_pt, "vill_tanin_pt", vill_tanin_pt);




// Exports properties we want into a feature.
var export_as_list_village = function(element) {
    element = ee.Feature(element);

    // Weird bug that makes it so that the string 'village_name' fails to get item.
    // Work around is to directly grab the property name and use that instead.
    var village_name = element.propertyNames().get(-1);
    return ee.Feature(null, ee.Dictionary({
        "year": ee.Number.parse(element.get('year_processed')).add(1970),
        "village": element.get(village_name),
        "buffer_size": element.get('bufferSize'),
        "total_forest": element.get('totalForestArea'),
        "forest_loss": element.get('totalForestLoss'),
        "forest_gain": element.get('totalForestGain'),
        "total_forest_base": element.get('totalForestBase'),
        "percent_forest_loss": ee.Number.parse(element.get('totalForestLoss')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100),
        "percent_forest_gain": ee.Number.parse(element.get('totalForestGain')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100)
    }));
};

var vill_kachin_pt_result = vill_kachin_processed.map(export_as_list_village);
var vill_karen_pt_result = vill_karen_processed.map(export_as_list_village);
var vill_tanin_pt_result = vill_tanin_processed.map(export_as_list_village);
//print("Karen pt result", vill_karen_pt_result);

Export.table.toDrive({
    collection: vill_kachin_pt_result,
    description: 'timeseries_village_forest_change_kachin',
    fileNamePrefix: 'timeseries_village_forest_change_kachin',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: vill_karen_pt_result,
    description: 'timeseries_village_forest_change_karen',
    fileNamePrefix: 'timeseries_village_forest_change_karen',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: vill_tanin_pt_result,
    description: 'timeseries_village_forest_change_tanintharyi',
    fileNamePrefix: 'timeseries_village_forest_change_tanintharyi',
    fileFormat: 'CSV'
});


/**
 * 5. Polygon Processing
 * 
 * The village tract, township, and state processing are all the same as they are polygons.
 * 
 * We find the overlapping polygons between the village and the administrative area.
 * Then find the forest area loss and gain for those areas.
 */
var polygon_processing = function(FC) {
    FC = ee.FeatureCollection(FC);

    // Putting years to each feature.
    var FC_year = ee.FeatureCollection(year_list.map(
        function(year) {
            return FC.map(function(feat) {
                return feat.set({
                    year_processed: year
                });
            });
        })).flatten();

    var FC_result = FC_year.map(addYearAnalysis);
    return (FC_result);
};


//Village Tract
var export_as_list_VT = function(element) {
    element = ee.Feature(element);

    return ee.Feature(null, ee.Dictionary({
        "year": ee.Number.parse(element.get('year_processed')).add(1970),
        "village_tract": element.get("VT"),
        "village_tract_PCODE": element.get("VT_PCODE"),
        "total_forest": element.get('totalForestArea'),
        "forest_loss": element.get('totalForestLoss'),
        "forest_gain": element.get('totalForestGain'),
        "total_forest_base": element.get('totalForestBase'),
        "percent_forest_loss": ee.Number.parse(element.get('totalForestLoss')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100),
        "percent_forest_gain": ee.Number.parse(element.get('totalForestGain')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100)
    }));
};

var VT_tni_filter = VT_tni.filterBounds(Village_tanin);
var VT_kch_filter = VT_kch.filterBounds(Village_kachin);
var VT_kyn_filter = VT_kyn.filterBounds(Village_karen);
var VT_mon_filter = VT_mon.filterBounds(Village_karen);

var VT_tni_processed = polygon_processing(VT_tni_filter);
var VT_kch_processed = polygon_processing(VT_kch_filter);
var VT_kyn_processed = polygon_processing(VT_kyn_filter);
var VT_mon_processed = polygon_processing(VT_mon_filter);

var VT_tni_result = VT_tni_processed.map(export_as_list_VT);
var VT_kch_result = VT_kch_processed.map(export_as_list_VT);
var VT_kyn_result = VT_kyn_processed.map(export_as_list_VT);
var VT_mon_result = VT_mon_processed.map(export_as_list_VT);


Export.table.toDrive({
    collection: VT_tni_result,
    description: 'timeseries_VT_forest_change_tanintharyi',
    fileNamePrefix: 'timeseries_VT_forest_change_tanintharyi',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: VT_kch_result,
    description: 'timeseries_VT_forest_change_kachin',
    fileNamePrefix: 'timeseries_VT_forest_change_kachin',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: VT_kyn_result,
    description: 'timeseries_VT_forest_change_karen',
    fileNamePrefix: 'timeseries_VT_forest_change_karen',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: VT_mon_result,
    description: 'timeseries_VT_forest_change_mon',
    fileNamePrefix: 'timeseries_VT_forest_change_mon',
    fileFormat: 'CSV'
});



// Township
var export_as_list_TS = function(element) {
    element = ee.Feature(element);

    return ee.Feature(null, ee.Dictionary({
        "year": ee.Number.parse(element.get('year_processed')).add(1970),
        "township": element.get("TS"),
        "township_PCODE": element.get("TS_PCODE"),
        "total_forest": element.get('totalForestArea'),
        "forest_loss": element.get('totalForestLoss'),
        "forest_gain": element.get('totalForestGain'),
        "total_forest_base": element.get('totalForestBase'),
        "percent_forest_loss": ee.Number.parse(element.get('totalForestLoss')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100),
        "percent_forest_gain": ee.Number.parse(element.get('totalForestGain')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100)
    }));
};
var TS_tni_filter = township.filterBounds(Village_tanin);
var TS_kch_filter = township.filterBounds(Village_kachin);
var TS_kyn_filter = township.filterBounds(Village_karen);

var TS_tni_processed = polygon_processing(TS_tni_filter);
var TS_kch_processed = polygon_processing(TS_kch_filter);
var TS_kyn_processed = polygon_processing(TS_kyn_filter);

var TS_tni_result = TS_tni_processed.map(export_as_list_TS);
var TS_kch_result = TS_kch_processed.map(export_as_list_TS);
var TS_kyn_result = TS_kyn_processed.map(export_as_list_TS);

Export.table.toDrive({
    collection: TS_tni_result,
    description: 'timeseries_TS_forest_change_tanintharyi',
    fileNamePrefix: 'timeseries_TS_forest_change_tanintharyi',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: TS_kch_result,
    description: 'timeseries_TS_forest_change_kachin',
    fileNamePrefix: 'timeseries_TS_forest_change_kachin',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: TS_kyn_result,
    description: 'timeseries_TS_forest_change_karen',
    fileNamePrefix: 'timeseries_TS_forest_change_karen',
    fileFormat: 'CSV'
});

// State
var export_as_list_State = function(element) {
    element = ee.Feature(element);

    return ee.Feature(null, ee.Dictionary({
        "year": ee.Number.parse(element.get('year_processed')).add(1970),
        "state": element.get("ST"),
        "state_PCODE": element.get("ST_PCODE"),
        "total_forest": element.get('totalForestArea'),
        "forest_loss": element.get('totalForestLoss'),
        "forest_gain": element.get('totalForestGain'),
        "total_forest_base": element.get('totalForestBase'),
        "percent_forest_loss": ee.Number.parse(element.get('totalForestLoss')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100),
        "percent_forest_gain": ee.Number.parse(element.get('totalForestGain')).divide(ee.Number.parse(element.get('totalForestBase'))).multiply(100)
    }));
};

var State_tni_filter = state.filterBounds(Village_tanin);
var State_kch_filter = state.filterBounds(Village_kachin);
var State_kyn_filter = state.filterBounds(Village_karen);

var State_tni_processed = polygon_processing(State_tni_filter);
var State_kch_processed = polygon_processing(State_kch_filter);
var State_kyn_processed = polygon_processing(State_kyn_filter);

var State_tni_result = State_tni_processed.map(export_as_list_State);
var State_kch_result = State_kch_processed.map(export_as_list_State);
var State_kyn_result = State_kyn_processed.map(export_as_list_State);

Export.table.toDrive({
    collection: State_tni_result,
    description: 'timeseries_state_forest_change_tanintharyi',
    fileNamePrefix: 'timeseries_state_forest_change_tanintharyi',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: State_kch_result,
    description: 'timeseries_state_forest_change_kachin',
    fileNamePrefix: 'timeseries_state_forest_change_kachin',
    fileFormat: 'CSV'
});
Export.table.toDrive({
    collection: State_kyn_result,
    description: 'timeseries_state_forest_change_karen',
    fileNamePrefix: 'timeseries_state_forest_change_karen',
    fileFormat: 'CSV'
});
