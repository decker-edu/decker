# Usage:
Note: On some systems docker may requires super user previliges.
1. Install [Docker](https://docs.docker.com/install/) on your host-OS.
2. Download the `ServerWithCustomSlides`.
3. Change the directory to this one: `cd <path/to/dir/ServerWithCustomSlides>`
4. Put your sildes in the `servedFiles` directory.
5. Run: `docker build -t decker .` to build the image.
6. Run: `docker run -d -p 80:8888 -v "$PWD"/served-files:/served-files --name my-decker decker` to run the image in a container.

Now you have the slides on your [localhost](http://localhost).

## How it works:
Decker with all dependencies runs in a Docker-container.  
The `served-files` is mounted as a volume in the Docker-container.
