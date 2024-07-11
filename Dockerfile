FROM ubuntu:latest
LABEL authors="crawfordcollins"

ENTRYPOINT ["top", "-b"]