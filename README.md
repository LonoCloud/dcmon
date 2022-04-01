# dcmon: docker-compose monitor

## Running it

* Run with nbb (interpreted):

```
npm install
./node_modules/.bin/nbb -cp src/ -m dcmon.core PROJECT ./checks.yaml
```

* Compile and run with shadow-cljs:

```
npm install
./node_modules/.bin/shadow-cljs compile dcmon
chmod +x build/dcmon.js
./build/dcmon.js PROJECT ./checks.yaml
```

* Compile and package in docker image and then run:

```
docker build -t dcmon .
docker run -it --rm -v /var/run/docker.sock:/var/run/docker.sock -v $(pwd)/checks.yaml:/app/checks.yaml dcmon PROJECT /app/checks.yaml
```
