#!/usr/bin/env python

import random
import geopandas as gpd
from shapely.affinity import rotate, translate
from shapely.geometry import Point, box, MultiPolygon

__author__ = "Jesse Wong"
__email__ = "jwong18@gmu.edu"

"""
This code was created in order to facilitate the generation of randomly located polygons that have a random rotation.
"""


def rotate_poly(poly):
    """Rotates a Polygon by a random amount.
    :type poly: shapely.geometry.Polygon or shapely.geometry.MultiPolygon.
    :param poly: Polygon to rotate.
    :rtype: shapely.geometry.Polygon or shapely.geometry.MultiPolygon
    :return: Rotated Polygon.
    """
    return rotate(poly, random.uniform(0.0, 360.0), "centroid")


def random_point(poly):
    """Generates a random point within the polygon.
    :type poly: geopandas.GeoSeries
    :param poly: Polygon of which the point with generate within.
    :return: Random point within polygon.
    :rtype: shapely.geometry.Point
    """

    min_x, min_y, max_x, max_y = poly.total_bounds

    # Randomly selects a point within the bounds. Then checks if its within the poly.
    while True:
        point = Point([random.uniform(min_x, max_x),
                       random.uniform(min_y, max_y)])
        if gpd.GeoSeries([point]).within(poly).any():
            break

    return point


def move(poly, point):
    """Moves a polygon to a new location based on the offset between the polygon's centroid and a point.
    :type poly: shapely.geometry.Polygon or shapely.geometry.MultiPolygon
    :param poly: Polygon to shift
    :type point: shapely.geometry.Point
    :param point: The location of the new centroid
    :rtype: shapely.geometry.Polygon or shapely.geometry.MultiPolygon
    :return: Polygon of a different location
    """
    centroid = poly.centroid

    # Calculates Delta
    delta = Point(point.x - centroid.x, point.y - centroid.y)

    new_poly = translate(poly, delta.x, delta.y, 0.0)

    return new_poly


def shift(polygon, base_region):
    """Moves and rotates a polygon randomly within the base region.
    :type polygon: shapely.geometry.Polygon or shapely.geometry.MultiPolygon
    :param polygon: Polygon to be shifted.
    :type base_region: geopandas.GeoSeries
    :param base_region: Region to constrain the new Polygon
    :rtype: shapely.geometry.Polygon or shapely.geometry.MultiPolygon
    :return: Polygon that has a random location and rotation.
    """
    counter = 0
    point = random_point(base_region)

    # Result should be a shapely Polygon or MultiPolygon
    result = move(polygon, point)
    result = rotate_poly(result)
    result = within_extent(result, base_region)

    # If the polygon is not within the base region, it randomizes until it is.
    while (not contains(result, base_region)) and counter < 1000:
        counter += 1
        point = random_point(base_region)
        result = move(polygon, point)
        result = rotate_poly(result)
        result = within_extent(result, base_region)

    if counter >= 1000:
        print("Polygon failed to be within base region. Please verify that this polygon is correct.")
    return result


def within_extent(polygon, base_region):
    """Checks to see if the polygon or individual components of a multipolygon are within the base region extent
    :type polygon: shapely.geometry.Polygon or shapely.geometry.MultiPolygon
    :param polygon: A polygon to check to see if it is within the extent
    :type base_region: geopandas.GeoSeries
    :param base_region: The region of which the extent of the geometry should be within
    :rtype: shapely.geometry.Polygon
    :returns: The geometry if it is within the extent. Otherwise, it returns a new geometry with its extent in the correct location. If input is a Multipolygon, it will return a Multipolygon
    """

    # If the polygon is a MultiPolygon, goes thorugh each polygon individually.
    if polygon.geom_type == "MultiPolygon":
        multipolygon = []
        for poly in polygon:
            multipolygon.append(within_extent(poly, base_region))
        return MultiPolygon(multipolygon)

    min_x, min_y, max_x, max_y = base_region.total_bounds

    # Bounds is a rectangular polygon with the bounds of the base region.
    bounds = box(min_x, min_y, max_x, max_y)

    # Returns polygon if it is already contained within the region
    if bounds.contains(polygon):
        return polygon
    else:

        # Otherwise, it warps the polygon to be within the bounds of the region
        width = max_x - min_x
        height = max_y - min_y
        geo_x, geo_y = polygon.centroid.x, polygon.centroid.y

        # X-Shift
        if geo_x < min_x:
            geo_x += width
        elif geo_x > max_x:
            geo_x -= width
        # Y-Shift
        if geo_y < min_y:
            geo_y += height
        elif geo_y > max_y:
            geo_y -= height

        # Uses move to move the polygon to the shifted location.
        result = move(polygon, Point(geo_x, geo_y))
        return result


def shift_all(geometry, base_region):
    """Randomly moves and rotates all polygons in geometry with the guarenttee that it will be within the base region.
    :type geometry: geopandas.GeoSeries
    :param geometry: GeoSeries of polygons
    :type base_region: geopandas.GeoSeries
    :param base_region: Region to constrain the new Polygon
    :rtype: geopandas.GeoSeries
    :return: A series of polygons that have a new location and rotation.
    """
    new_geom = []
    print("Number of geometries: {}".format(len(geometry)))

    # Iterates through the list of polygons in geometry
    for polygon in geometry:
        new_geom.append(shift(polygon, base_region))
        print("Geometries finished: {}".format(len(new_geom)))

    return gpd.GeoSeries(new_geom)


def contains(polygon, base_region):
    """Checks to see if the polygon is completely within the base region.
    :type polygon: shapely.Polygon
    :param polygon: Polygon to check
    :type base_region: geopandas.GeoSeries
    :param base_region: Region to check within
    :rtype: Boolean
    :return: True if the polygon is within the base region.
    """
    # GeoSeries.contains() return a series
    # Series.any() returns True if there is at least 1 True is within the series
    return gpd.GeoSeries([polygon]).within(base_region).any()


def save(gdf, location):
    """Saves GeoDataFrame to location as a ShapeFile. Will override files that are present.
    :type gdf: geopandas.GeoDataFrame
    :param gdf: GeoDataFrame of geometries
    :type location: String
    :param location: Save Directory and file name
    """
    gdf.to_file(location)


def main(ws, shp, basemap, save_loc):
    shp_gdf = gpd.read_file(ws + shp)          # ShapeFile to move/rotate
    basemap_gdf = gpd.read_file(ws + basemap)  # Base Map as a constraint.

    shp_gdf['geometry'] = shift_all(
        shp_gdf['geometry'], basemap_gdf['geometry'])
    save(shp_gdf, workspace + save_loc)



if __name__ == '__main__':
    workspace = ""  # Workspace Directory
    region = ""     # Shapefile to move/rotate
    basemap = ""    # Base Map to act as the constraint
    save_loc = "test_1.shp"   # File name + extension of processed file

    main(workspace, region, basemap, save_loc)
