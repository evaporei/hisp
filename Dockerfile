FROM haskell

COPY . .

CMD ["./compile_and_run.sh"]
