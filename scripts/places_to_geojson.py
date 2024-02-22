
from sys import argv
from os.path import exists
import simplejson as json 

# script, in_file, out_file = argv

data = json.load(open("../data/Points_of_interest/points_of_interest.json"))

geojson = {
    "type": "FeatureCollection",
    "features": [
    {
        "type": "Feature",
        "geometry" : {
            "type": "Point",
            "coordinates": [d["geometry"]["location"]["lat"], d["geometry"]["location"]["lng"]],
            },
        "properties" : d,
     } for d in data]
}

output = open("../data/Points_of_interest/points_of_interest.geojson", 'w')
json.dump(geojson, output)