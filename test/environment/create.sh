#!/bin/bash

cp ../../stack.yaml .
cp ../../package.yaml .

docker build -t gitlab2.informatik.uni-wuerzburg.de:4567/decker/decker/testcontainer .

docker login gitlab2.informatik.uni-wuerzburg.de:4567
docker push gitlab2.informatik.uni-wuerzburg.de:4567/decker/decker/testcontainer
