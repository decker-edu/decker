# Usage:
Note: On some systems docker may requires super user previliges.
1. Install [Docker](https://docs.docker.com/install/) on your host-OS.
2. Download the `Dockerfile`.
3. Change the directory to the Dockerfile `cd <path/to/Dockerfile>`
4. Run in your command-line: `docker build -t decker-example .` to build the image.
5. Run `docker run -d -p 80:8888 --name my-decker-example decker-example` to run the container from the image.

Now you have an example on your [localhost](http://localhost)

Keep in mind this is just an example-app.  
For serving your own slides use the ´ServerWithCustomFiles´ in this repo.
