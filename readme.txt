
# Docker commands to build and run the container

docker build -t plumber-api . & docker run -p 8080:8080 -ti plumber-api

docker stop $(docker ps -a -q)
