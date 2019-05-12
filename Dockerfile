FROM haskell

COPY . .

RUN stack setup && stack build

CMD stack run
