FROM node:20 AS build

RUN apt-get -y update && \
    apt-get -y install default-jdk-headless

# Separate npm and clojure deps from main app build
RUN mkdir -p /app
ADD shadow-cljs.edn package.json package-lock.json /app/
RUN cd /app && npm --unsafe-perm install
RUN cd /app && ./node_modules/.bin/shadow-cljs info

# main app build
ADD src/ /app/src/
#ADD test/ /app/test/
RUN cd /app && \
    ./node_modules/.bin/shadow-cljs compile dcmon && \
    chmod +x build/*.js


FROM node:20-slim AS run

COPY --from=build /app/ /app/

ENTRYPOINT ["/app/build/dcmon.js"]
