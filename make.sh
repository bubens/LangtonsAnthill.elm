#!/bin/bash

SOURCE_ELM="./src/Main.elm";
SOURCE_JS="./js/main.js";
SOURCE_HTML="./html/index.html";

TMP_FOLDER="./tmp_$(date +%s)";

RELEASE_FOLDER="./rel";

echo "Step 0: Prepare build process..."
# Create tmp-directory
if [ ! -d $TMP_FOLDER ]; then
    echo "making TMP-Folder..."
    mkdir $TMP_FOLDER
fi

if [ ! -d $RELEASE_FOLDER ]; then
    echo "making release-folder (rel/)..."
    mkdir $RELEASE_FOLDER
else
    echo "cleaning up release-folder (rel/*)..."
    rm "$RELEASE_FOLDER/*"
fi

# compile
echo "Step 1: Copying html..." &&
cp -v $SOURCE_HTML $RELEASE_FOLDER &&
echo "Step 2: Compiling elm..." &&
elm make --output=$TMP_FOLDER/elm.js $SOURCE_ELM &&
echo "Step 3: Bundling up with native parts..." &&
cp $SOURCE_JS $TMP_FOLDER &&
browserify -o $TMP_FOLDER/app.js $TMP_FOLDER/main.js &&
cp $TMP_FOLDER/app.js $RELEASE_FOLDER &&
echo "Done building app"

echo "Cleaning up..." &&
rm -r $TMP_FOLDER &&
echo "Success"


