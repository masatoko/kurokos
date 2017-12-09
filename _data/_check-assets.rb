require 'yaml'
require 'pp'

assets = YAML.load_file('assets.yaml')

pp assets
