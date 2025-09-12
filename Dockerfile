FROM debian:13.1-slim

COPY _build/default/bin/type_driven_search.exe /app/type_driven_search.exe

WORKDIR /app

ENTRYPOINT ["/app/type_driven_search.exe"]
