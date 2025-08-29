# ---------------------------
# Dockerfile for dashboard_femenil (fixed textshaping build)
# ---------------------------

# 1. Base image
FROM rocker/shiny:4.4.1

# 2. Install system dependencies for R packages (graphics, geospatial, compilation)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libv8-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gfortran \
    g++ \
    default-jdk \
    pkg-config \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libglib2.0-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Configure Java for R
RUN R CMD javareconf

# 4. Set working directory
WORKDIR /home/dashboard_femenil

# 5. Copy project files
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# 6. Set PKG_CONFIG_PATH so harfbuzz and fribidi are found
ENV PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig

# 7. Install renv
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# 8. Pre-install compilation-heavy packages
RUN R -e "install.packages(c('sf','units','V8','systemfonts','textshaping'), repos='https://cloud.r-project.org')"

# 9. Restore project library using renv
RUN R -e "options(renv.verbose=TRUE); renv::restore(prompt=FALSE)"

# 10. Default command to deploy the app
CMD ["Rscript", "deploy.R"]
