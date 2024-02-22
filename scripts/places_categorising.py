
# import simplejson as json 
import json 

data = json.load(open("../data/Points_of_interest/points_of_interest.json"))

categories = {
    "Cafe": ["cafe"],
    "Food & Drinks": ["food", "restaurant", "bar", "night_club"],
    "Shops & Supermarkets": ["supermarket", "clothing_store", "convenience_store", "department_store", "liquor_store"],
    "Gym": ["gym"],
    "Health": ["health"],
    "Places of Worship": ["place_of_worship"],
    "Tourism & Entertainment": ["point_of_interest", "movie_theater", "tourist_attraction", "bowling_alley", "casino", "zoo", "museum"],
}

categorised_places = {}

for place in data: 
    place['popup'] = '<h4><b>' + place["name"] + '</b></h4>' + place["vicinity"] + '<br><i>' + place["types str"].replace('_', ' ') + '</i><br><br>' 
    if place.get('rating', False): 
        place['popup'] += '<font color="gray"><b>' + str(place["rating"]) + '★</b> (' + str(place["user_ratings_total"]) + ')</font>'
    if place.get('price_level', False):
        place['popup'] += '<font color="green"><span style="float:right">' + "$"*place["price_level"] + '</font></span>'
    place['long'] = place['geometry']['location']['lng']
    place['lat'] = place['geometry']['location']['lat']
    assigned = False
    for type in place['types']:
        for cname in categories: 
            # if [i for i in place["types"] if i in categories[cname]]:
            if type in categories[cname]:
                categorised_places[cname] = categorised_places.get(cname, [])
                categorised_places[cname].append(place)
                assigned = True
                break
        if assigned:
            break   

for cat in categorised_places:
    with open("../data/Points_of_interest/categorised_data/" + cat + ".json", 'w') as f:
        json.dump(categorised_places[cat], f)