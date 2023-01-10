FROM node:19-alpine

WORKDIR /usr/src/app
COPY package*.json ./

RUN npm ci
COPY . .

EXPOSE 500
CMD [ "node", "index.js" ]