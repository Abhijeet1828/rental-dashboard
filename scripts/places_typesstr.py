import json
import time
f = open('places_data1665528847.json')
data = json.load(f)

for place in data:
    place["types str"] = ', '.join(place["types"])

for place in data[0:5]:
    print(place)

with open('places_data_fix' + str(int(time.time())) + '.json', 'w') as f:
    json.dump(data, f)