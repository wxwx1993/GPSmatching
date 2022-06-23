# Docker and Singularity Images

This folder documents the required steps in working with Docker and Singularity images.

## Singularity

Topics related to the Singularity image are documented under [Singularity](https://fasrc.github.io/CausalGPS/articles/Singularity-Image.html) vignette. 

## Docker

### Option 1: Create an image from a recipe

- Step 1: Make sure the Docker Desktop is installed on your system.  
- Step 2: Make sure you have the [Dockerfile](Dockerfile)   
- Step 3: Build the image

```s
docker build -t causalgps_dev . 
```
Step 5: Get the path for the project on your system   
Step 6: Update the path in the following command and spin up the container.

```s
docker run -it --rm \
        -p 8230:8787 \
        -e USER=rstudio \
        -e PASSWORD=pass \
        -v "/path/to/your/folder/on/host:/home/rstudio/Project" causalgps_dev
```


### Option 2: Use the available image from Docker Hub

All NSAPH-Software images are located at [NSAPH Docker Hub](https://hub.docker.com/u/nsaphsoftware) repository.

Run the following code to download and spin up the image.

```s
docker run -it --rm \
        -p 8230:8787 \
        -e USER=rstudio \
        -e PASSWORD=pass \
        -v "/path/to/your/folder/on/host:/home/rstudio/Project" nsaphsoftware/causalgps_dev

```