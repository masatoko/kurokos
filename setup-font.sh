set -eu

mkdir -p __temp/
wget https://ja.osdn.net/projects/mplus-fonts/downloads/62344/mplus-TESTFLIGHT-063.tar.xz/ -O __temp/mplus.tar.xz

cd __temp
tar -Jxvf mplus.tar.xz
cd ..

mkdir -p _data/font/
mv __temp/mplus-TESTFLIGHT-063/mplus-1p-regular.ttf _data/font/mplus-1p-regular.ttf
mv __temp/mplus-TESTFLIGHT-063/mplus-1p-medium.ttf _data/font/mplus-1p-medium.ttf
mv __temp/mplus-TESTFLIGHT-063/mplus-1p-bold.ttf _data/font/mplus-1p-bold.ttf

echo "Remove __temp directory."
