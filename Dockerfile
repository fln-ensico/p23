FROM gibiansky/ihaskell
COPY --chown=jovyan notebooks/ /home/jovyan/src/
COPY --chown=jovyan config/ /home/jovyan/.jupyter/
RUN stack build split-0.2.3.5
EXPOSE 8888
CMD jupyter-lab --ip=0.0.0.0