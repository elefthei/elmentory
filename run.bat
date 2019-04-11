@echo off
elm make src\Main.elm --output=elm.js
npm install
echo "Executing elmentory..."
npm start
pause
