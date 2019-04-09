# Module 5, DATA 608
# Leland Randles

import pandas as pd
from flask import Flask

app = Flask(__name__)

# This Flask API lets you enter any borough / species combination and 
# then returns a json blob of the tree_id, boroname, spc_common, health 
# and steward columns of the NYC tree census data set for the borough / 
# species combination in the URL. Visit Localhost:5000/search_bs/borough/species
# to see what gets returned. Replace borough and species with one of the 
# borough and species combinations from the NYC Tree Census web dataset.

# Example 1:  http://localhost:5000/search_bs/Bronx/arborvitae
# Example 2:  http://localhost:5000/search_bs/Staten Island/black maple

@app.route('/search_bs/<string:borough>/<string:species>')
def return_bs(borough, species):
    
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=700000&' +\
    '$offset=0&$select=tree_id,boroname,spc_common,health,steward&' +\
    '$where=health!=%27NaN%27%20and%20spc_common!=%27nan%27%20and%20' +\
    'boroname=%27' + borough + '%27%20and%20spc_common=%27' + species + '%27').replace(' ', '%20')
    trees_by_bs  = pd.read_json(url)
    trees_by_bs_json = trees_by_bs.to_json(orient = "records")
    #return jsonify(trees_by_bs)
    return trees_by_bs_json

if __name__ == '__main__':
    app.run(debug=True)
