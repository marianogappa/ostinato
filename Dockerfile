FROM anapsix/alpine-java:8

RUN mkdir -p /lib

WORKDIR /

ADD /target/pack/lib /lib
ADD /target/pack/ostinato.jar /lib/
ADD /target/pack/bin/main /ostinato

EXPOSE 51234

ENTRYPOINT ["/ostinato"]
