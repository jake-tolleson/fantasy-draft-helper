# Use the official R base image
FROM r-base:4.3.1

# Install necessary libraries
RUN R -e "install.packages(c('shiny', 'DT', 'dplyr', 'ggplot2', 'plotly', 'bslib'))"

# Copy your app's code into the container
COPY . /app

# Set the working directory
WORKDIR /app

# Expose the port Shiny will listen on
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app')"]
