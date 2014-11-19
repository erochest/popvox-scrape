
MAPLIGHT_APIKEY=
CURL_OPTS=--max-time 90

all: run

init:
	cabal sandbox init
	make deps
	data

test: test-json test-csv

run: test transform rank-bills

transform:
	cabal run -- transform --data-dir data --output maplight-data.csv

rank-bills:
	cabal run -- rank-bills --score-dir=junkord --bill-dir=bills --legislator-index=ids --output=bill-ranks.csv

test-json:
	cabal run -- test-json

test-csv:
	cabal run -- test-csv

data: unzip-contributions maplight-api govtrackdata junkord id-index

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

hlint:
	hlint *.hs src specs

tags: Main.hs src/PopVox/Bills.hs src/PopVox/Fields.hs src/PopVox/Legislators.hs src/PopVox/MapLight.hs src/PopVox/Output.hs src/PopVox/Ranks.hs src/PopVox/Types.hs src/PopVox/Types/Bills.hs src/PopVox/Types/Common.hs src/PopVox/Types/Contrib.hs src/PopVox/Types/Orgs.hs src/PopVox/Types/Output.hs src/PopVox/Types/Position.hs src/PopVox/Utils.hs
	hasktags --ctags Main.hs src/

clean:
	cabal clean
	-rm -f tags

cleandata:
	-rm -rf data
	-rm -rf maplight-data.csv bill-ranks.csv

distclean: clean cleandata
	cabal sandbox delete

configure: clean
	cabal configure --enable-tests

deps: clean
	cabal install --only-dependencies --allow-newer --enable-tests
	cabal configure --enable-tests

build:
	cabal build

rebuild: clean configure build

maplight-data.csv: unzip-contributions maplight-data
	cabal run -- transform --data-dir data --output $@

bill-ranks.csv: govtrackdata junkord id-index
	cabal run -- rank-bills --score-dir=junkord --bill-dir=bills --legislator-index=ids --output=$@

zips/contributions-2008.zip:
	mkdir -p zips
	curl ${CURL_OPTS} -o $@ http://data.maplight.org/US/2008/records/cand.zip

zips/contributions-2010.zip:
	mkdir -p zips
	curl ${CURL_OPTS} -o $@ http://data.maplight.org/US/2010/records/cand.zip

zips/contributions-2012.zip:
	mkdir -p zips
	curl ${CURL_OPTS} -o $@ http://data.maplight.org/US/2012/records/cand.zip

zips/contributions-2014.zip:
	mkdir -p zips
	curl ${CURL_OPTS} -o $@ http://data.maplight.org/US/2014/records/cand.zip

download-contributions: zips/contributions-2008.zip zips/contributions-2010.zip zips/contributions-2012.zip zips/contributions-2014.zip

data/2008_cand.csv: zips/contributions-2008.zip
	mkdir -p data
	unzip -d data zips/contributions-2008.zip

data/2010_cand.csv: zips/contributions-2010.zip
	mkdir -p data
	unzip -d data zips/contributions-2010.zip

data/2012_cand.csv: zips/contributions-2012.zip
	mkdir -p data
	unzip -d data zips/contributions-2012.zip

data/2014_cand.csv: zips/contributions-2014.zip
	mkdir -p data
	unzip -d data zips/contributions-2014.zip

unzip-contributions: data/2008_cand.csv data/2010_cand.csv data/2012_cand.csv data/2014_cand.csv

clean-contributions:
	sed -i '.bak' -e 's/\\"/""/g' data/2008_cand.csv
	sed -i '.bak' -e 's/\\"/""/g' data/2010_cand.csv
	sed -i '.bak' -e 's/\\"/""/g' data/2012_cand.csv
	sed -i '.bak' -e 's/\\"/""/g' data/2014_cand.csv

maplight-cache/109.json:
	mkdir -p maplight-cache
	curl ${CURL_OPTS} -o $@ 'http://maplight.org/services_open_api/map.bill_list_v1.json?apikey=${MAPLIGHT_APIKEY}&jurisdiction=us&session=109&include_organizations=1&has_organizations=0'

maplight-cache/110.json:
	mkdir -p maplight-cache
	curl ${CURL_OPTS} -o $@ 'http://maplight.org/services_open_api/map.bill_list_v1.json?apikey=${MAPLIGHT_APIKEY}&jurisdiction=us&session=110&include_organizations=1&has_organizations=0'

maplight-cache/111.json:
	mkdir -p maplight-cache
	curl ${CURL_OPTS} -o $@ 'http://maplight.org/services_open_api/map.bill_list_v1.json?apikey=${MAPLIGHT_APIKEY}&jurisdiction=us&session=111&include_organizations=1&has_organizations=0'

maplight-cache/112.json:
	mkdir -p maplight-cache
	curl ${CURL_OPTS} -o $@ 'http://maplight.org/services_open_api/map.bill_list_v1.json?apikey=${MAPLIGHT_APIKEY}&jurisdiction=us&session=112&include_organizations=1&has_organizations=0'

maplight-api: maplight-cache/109.json maplight-cache/110.json maplight-cache/111.json maplight-cache/112.json 

clear-cache:
	-rm -rf maplight-cache

bills/109/bills:
	mkdir -p bills/109
	rsync -avz --delete --delete-excluded --exclude **/text-versions/ govtrack.us::govtrackdata/congress/109/bills bills/109

bills/110/bills:
	mkdir -p bills/110
	rsync -avz --delete --delete-excluded --exclude **/text-versions/ govtrack.us::govtrackdata/congress/110/bills bills/110

bills/111/bills:
	mkdir -p bills/111
	rsync -avz --delete --delete-excluded --exclude **/text-versions/ govtrack.us::govtrackdata/congress/109/bills bills/111

bills/112/bills:
	mkdir -p bills/112
	rsync -avz --delete --delete-excluded --exclude **/text-versions/ govtrack.us::govtrackdata/congress/112/bills bills/112

bills/113/bills:
	mkdir -p bills/113
	rsync -avz --delete --delete-excluded --exclude **/text-versions/ govtrack.us::govtrackdata/congress/113/bills bills/113

bills/114/bills:
	mkdir -p bills/114
	rsync -avz --delete --delete-excluded --exclude **/text-versions/ govtrack.us::govtrackdata/congress/114/bills bills/114

govtrackdata: bills/109/bills bills/110/bills bills/111/bills bills/112/bills bills/113/bills

junkord/HL01112D21_PRES_BSSE.DAT:
	mkdir -p junkord
	curl ${CURL_OPTS} -o $@ ftp://voteview.com/junkord/HL01112D21_PRES_BSSE.DAT

junkord/SL01112D21_BSSE.dat:
	mkdir -p junkord
	curl ${CURL_OPTS} -o $@ ftp://voteview.com/junkord/SL01112D21_BSSE.dat

junkord: junkord/SL01112D21_BSSE.dat junkord/HL01112D21_PRES_BSSE.DAT

id-index: ids/legislators-current.yaml ids/legislators-historical.yaml

ids/legislators-current.yaml:
	mkdir -p ids
	curl ${CURL_OPTS} -o $@ https://raw.githubusercontent.com/unitedstates/congress-legislators/master/legislators-current.yaml

ids/legislators-historical.yaml:
	mkdir -p ids
	curl ${CURL_OPTS} -o $@ https://raw.githubusercontent.com/unitedstates/congress-legislators/master/legislators-historical.yaml


.PHONY: all init test run clean distclean configure deps build rebuild hlint
