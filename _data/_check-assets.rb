require 'yaml'
require 'pp'

assets1 = YAML.load_file('assets1.yaml')
pp assets1

assets2 = YAML.load_file('assets1.yaml')
pp assets2
