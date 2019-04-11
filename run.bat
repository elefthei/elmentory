@echo off
elm make src\Main.elm --output=elm.js & npm install & npm start
