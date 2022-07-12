FROM haskell:8

RUN mkdir /coordinates
COPY . /coordinates
# set working directory
RUN cd /coordinates && stack build --system-ghc

RUN chmod +x "/coordinates/start.sh"
WORKDIR /coordinates
EXPOSE 9999 
