import requests
import json
import time
api_key = 'AIzaSyAlEl7yt8E7Y7tuui21xhJr-UPIqxrW67M'
url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?radius=1000&key=" + api_key #location=-37.49%2C144.58
next_url = url + "&pagetoken=" # "http&pagetoken="s://maps.googleapis.com/maps/api/place/nearbysearch/json?
payload = {}
headers = {}
pagetoken = None

places = [] # list of places from http response
place_ids = set() # set of visited place ids
# types = ['place_of_worship', 'food', 'landmark', 'health', 'establishment', 'point_of_interest']
radius = 1000

loc_centres = [
    ["-37.840518", "144.980600"], 
    ["-37.843036", "144.975621"],
    ["-37.837275", "144.973647"],
    ["-37.828736", "144.977508"],
    ["-37.823541", "144.978364"],
    ["-37.817575", "144.972069"],
    ["-37.818568", "144.958142"],
    ["-37.815057", "144.957893"],
    ["-37.814650", "144.968448"],
    ["-37.811546", "144.967792"],
    ["-37.808412", "144.959872"],
    ["-37.804417", "144.958793"],
    ["-37.801883", "144.957648"]
]

# making internal categories ref: https://developers.google.com/maps/documentation/places/web-service/supported_types 
# roughly ordered from most specific to least
types = { 
    'Bar/Nightlife': ['bar', 'night_club'],
    'Gym': ['gym'],
    'Cafe/bakery': ['cafe', 'bakery'],
    'Culture/attractions': ['art_gallery', 'casino', 'city_hall', 'movie_theater', 'zoo','aquarium', 'amusement_park' ],
    'Library': ['library'],
    'Bank': ['bank'],
    'Restaurant': ['restaurant', 'food'],
    'Religious institution': ['place_of_worship'],
    'Beauty Salon': ['beauty_salon', ],
    'Liquor Store': ['liquor_store'],
    'Shopping': ['clothing_store', 'shopping_mall'],
    'Convenience store/supermarket': ['convenience_store', 'supermarket', 'department_store', 'drugstore', 'pharmacy'],
    'University': ['university'],
    'Transport': ['subway_station', 'train_station', 'transit_station']
}

for primary_type in types:                  # eg. Shopping,
    for sub_type in types[primary_type]:    # eg. ["clothing_store", "shopping_mall"]
        for loc in loc_centres:             # each central coordinate to search around
            locstr = loc[0] + "%2C" + loc[1] # location str in format for url param
            time.sleep(2)
            curr_url = url + "&location=" + locstr + "&type=" + sub_type
            print(curr_url)
            response = requests.request("GET", curr_url, headers=headers, data=payload)
            data = json.loads(response.text)
            # print(data['results'])
            print("Loc:", loc, "Type:", sub_type, "Status:", data['status'], "Len so far:", len(places))
            if data['status']!='OK':
                print("Invalid req", data)
            newplaces = [p for p in data["results"] if p['place_id'] not in place_ids]
            for newplace in newplaces: 
                newplace["primary_type"] = primary_type
            places += newplaces # add only those not already present
            place_ids.add(p['place_id'] for p in data['results']) # store visited place ids in place_ids

            while 'next_page_token' in data: 
                time.sleep(2) # need time between requests
                page_url = curr_url + "&pagetoken=" + data["next_page_token"]
                response = requests.request("GET", page_url, headers=headers, data=payload)
                data = json.loads(response.text)
                if data['status']!='OK':
                    print("Invalid req", data, "\n", page_url)

                newplaces = [p for p in data["results"] if p['place_id'] not in place_ids]
                for newplace in newplaces: 
                    newplace["primary_type"] = primary_type
                places += newplaces # add only those not already present
                place_ids.add(p['place_id'] for p in data['results']) # store visited place ids in place_ids
                print("Next page. Status:", data['status'], "Len so far:", len(places))

print(len(places))
with open('places_data' + str(int(time.time())) + '.json', 'w') as f:
    json.dump(places, f)