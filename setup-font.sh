set -eu

open https://ipafont.ipa.go.jp/node26#jp

echo "IPAフォントライセンス をお読みください。"
read -p "進みますか? (y/N): " yn
case "$yn" in [yY]*) ;; *) echo "セットアップを中止しました。" ; exit ;; esac

# Download
mkdir -p __temp/
curl https://oscdl.ipa.go.jp/IPAexfont/ipaexg00301.zip -o __temp/ipa.zip

# Unzip
cd __temp
unzip ipa.zip
cd ..

# Move font file
mkdir -p _data/font/
mv __temp/ipaexg00301/ipaexg.ttf _data/font/ipaexg.ttf
