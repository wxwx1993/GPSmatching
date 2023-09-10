# Reproducing the examples

You can replicate the examples on any operating system as long as you install all the necessary dependencies. However, by following the steps outlined below, you can seamlessly set up the same environment in just a few minutes.

## Option 1: Getting Image from Dockerhub (recommended)

The image is located at [NSAPH Docker Hub](https://hub.docker.com/u/nsaphsoftware) repository. 

```s
docker run -it --rm \
        -p 8230:8787 \
        -e USER=rstudio \
        -e PASSWORD=pass \
        -v "/path/to/your/folder/on/host:/home/rstudio/Project" nsaphsoftware/causalgps_dev

```

Open your browser and put http://localhost:8230/. Username and password is `rstudio` and `pass`, pass respectively. Make sure to change the path to your folder on host. 

## Option 2: Build Image 

_TBD_
