# dcmon: docker-compose monitor

Monitor docker-compose services.

## Usage

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

## Example:

The compose file at `examples/docker-compose.yaml` containers several
compose services that behave in different ways. The checks file at
`examples/checks.yaml` has example check definitions for each of the
services in the compose file.

In one terminal, launch dcmon as described above using a PROJECT value
of `${USER}-test`. For example, to use nbb directly:

```
./node_modules/.bin/nbb -cp src/ -m dcmon.core ${USER}-test ./examples/checks.yaml
```


In a second terminal, launch the compose file:
```
docker-compose -p ${USER}-test -f ./examples/docker-compose.yaml up --force-recreate
```

In the dcmon terminal a visual representation of the service
containers and the checks will be shown.

## Copyright & License

This software is copyright Viasat, Inc and is released under the terms
of the Eclipse Public License version 2.0 (EPL.20). A copy of the
license is located at in the LICENSE file at the top of the
repository.
