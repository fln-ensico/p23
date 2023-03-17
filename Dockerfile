#FROM gibiansky/ihaskell
FROM ghcr.io/ihaskell/ihaskell-notebook:master@sha256:7105b6519a07515babc254b7d3582f7e631149bc9b8ae5f2df0a215d4143cec6
COPY --chown=jovyan notebooks/ /home/jovyan/src/
COPY --chown=jovyan config/ /home/jovyan/.jupyter/
EXPOSE 8888
CMD jupyter-lab --ip=0.0.0.0
