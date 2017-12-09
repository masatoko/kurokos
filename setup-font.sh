set -eu

mkdir __temp/
wget https://ja.osdn.net/projects/robotoj-font/downloads/66649/RobotoJ_20161105.tar.gz/ -O __temp/roboto.tar.gz
cd __temp
tar -zxvf roboto.tar.gz
cd ..
mv __temp/RobotoJ/TTF/ _data/font/
