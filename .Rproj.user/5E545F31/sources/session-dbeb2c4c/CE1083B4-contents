# Dockerfile for dashboard_femenil

# ---------------------------
# 1. Base image
# ---------------------------
FROM rocker/shiny:4.4.1

# ---------------------------
# 2. Install system dependencies commonly needed by R packages
# ---------------------------
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
    && rm -rf /var/lib/apt/lists/*

# ---------------------------
# 3. Set working directory
# ---------------------------
WORKDIR /home/dashboard_femenil

# ---------------------------
# 4. Copy project files
# ---------------------------
COPY dashboard_femenil.Rproj renv.lock ./
COPY app.R dashboard_femenil.R deploy.R ./
COPY data data
COPY micros micros
COPY www www
COPY rsconnect rsconnect

# ---------------------------
# 5. Install renv
# ---------------------------
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# ---------------------------
# 6. Pre-install heavy or compilation-heavy packages
# ---------------------------
RUN R -e "install.packages(c('sf', 'V8', 'units'), repos='https://cloud.r-project.org', type='binary')"

# ---------------------------
# 7. Restore project library using renv with binary packages
# ---------------------------
RUN R -e "options(renv.verbose=TRUE); renv::restore(prompt=FALSE, type='binary')"

# ---------------------------
# 8. Default command to deploy the app
# ---------------------------
CMD ["Rscript", "deploy.R"]
