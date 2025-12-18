# Docker Instructions

The docker image is built through GitHub Actions and pushed to GitHub Container Registry. So this is mainly for local testing and manual builds.

```bash
# run in the root directory of this repo
docker build --platform=linux/amd64 -t shiny-flyer:latest -f docker/dockerfile .
docker run --platform=linux/amd64 --rm -p 3838:3838 shiny-flyer:latest
```
