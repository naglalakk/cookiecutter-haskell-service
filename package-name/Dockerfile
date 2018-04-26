FROM kottur/haskell-stack

RUN apt-get update -y
RUN apt-get install libpq-dev -y

RUN mkdir -p /code
RUN mkdir -p /code/logs

ADD registration-service.cabal /code
ADD stack.yaml /code
WORKDIR /code

RUN stack install --only-dependencies -j4

ADD . /code
RUN chmod +x config/wait-for-it.sh

RUN stack install
