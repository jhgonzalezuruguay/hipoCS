FROM rocker/shiny:latest

# Instalar librerías necesarias (una por línea)
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('tidyr')"

# Copiar tu aplicación al contenedor
COPY app.R /srv/shiny-server/

# Render asigna el puerto en la variable de entorno PORT
ENV PORT=8080

# Comando de inicio de la app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/hipoCS.R', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]