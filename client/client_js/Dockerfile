FROM nginx:alpine
WORKDIR /usr/share/nginx/html
RUN rm -rf ./*
ENTRYPOINT ["nginx", "-g", "daemon off;"]
