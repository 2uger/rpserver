let canvas = document.getElementById("map");
let ctx = canvas.getContext("2d");

let apiURL = '0.0.0.0:8000';
let wsURL = '0.0.0.0:9999';

// constants
let ballRadius = 5;
let dx = 1;
let dy = 1;

let rightPressed = false;
let leftPressed = false;
let upPressed = false;
let downPressed = false;

class Rider {
    constructor(name = '', uid='') {
        this.name = name;
        this.uid = uid;
        this.coordinates = {long: 0.0,  // x
                            latt: 0.0}; // y
    }
    moveUp() { this.coordinates.latt -= dy; }
    moveDown() { this.coordinates.latt += dy; }
    moveRight() { this.coordinates.long += dx; }
    moveLeft() { this.coordinates.long -= dx; }

    get x() { return this.coordinates.long; }
    set x(new_x) { this.coordinates.long = new_x; }

    get y() { return this.coordinates.latt; }
    set y(new_y) { this.coordinates.latt = new_y; }
}

class Spot {
    constructor(name='', coordinates=[0, 0]) {
        this.name = name;
        this.coordinates = {long: coordinates[0],
                            latt: coordinates[1]};
    }
    get x() { return this.coordinates.long; }
    set x(new_x) { this.coordinates.long = new_x; }

    get y() { return this.coordinates.latt; }
    set y(new_y) { this.coordinates.latt = new_y; }
}

function keyDownHandler(e) {
    if(e.key == "Right" || e.key == "ArrowRight") {
        rightPressed = true;
    }
    else if(e.key == "Left" || e.key == "ArrowLeft") {
        leftPressed = true;
    }
    else if(e.key == "Up" || e.key == "ArrowUp") {
        upPressed = true;
    }
    else if(e.key == "Down" || e.key == "ArrowDown") {
        downPressed = true;
    }
}

function keyUpHandler(e) {
    if(e.key == "Right" || e.key == "ArrowRight") {
        rightPressed = false;
    }
    else if(e.key == "Left" || e.key == "ArrowLeft") {
        leftPressed = false;
    }
    else if(e.key == "Up" || e.key == "ArrowUp") {
        upPressed = false;
    }
    else if(e.key == "Down" || e.key == "ArrowDown") {
        downPressed = false;
    }
}

function drawRider(rider) {
    ctx.beginPath();
    ctx.arc(rider.x, rider.y, ballRadius, 0, Math.PI * 2, false);
    ctx.fillStyle = "green";
    ctx.fill();
    ctx.closePath();

    ctx.beginPath();
    ctx.fillText(rider.name, rider.x - 10, rider.y - 10);
    ctx.closePath();
}

function updateMyLocation(rider) {
    if (rightPressed) {
        rider.x += dx;
    }
    if (leftPressed) {
        rider.x -= dx;
    }
    if (upPressed) {
        rider.y -= dy;
    }
    if (downPressed) {
        rider.y += dy;
    }
}

function updateMyInfo(rider) {
    let name = document.getElementById('name');
    let uid = document.getElementById('uid');
    let latt = document.getElementById('latt');
    let long = document.getElementById('long');

    name.textContent = rider.name;
    uid.textContent = rider.uid;
    latt.textContent = rider.x;
    long.textContent = rider.y;
}

let riders = new Map();
let spots = [];

/*
 * Functions to work with Spots
 */

function drawSpots(spots) {
    drawSpot = function(spot) {
        ctx.beginPath();
        ctx.arc(spot.x, spot.y, ballRadius, 0, Math.PI * 2, false);
        ctx.fillStyle = "black";
        ctx.fill();
        ctx.closePath();

        ctx.beginPath();
        ctx.fillText(spot.name, spot.x - 10, spot.y - 10);
        ctx.closePath();
    }
    spots.forEach(drawSpot);
}

function fetchSpots() {
    return fetch('http://' + apiURL + '/api/spots')
    .then(resp => resp.json());
}

function createSpots() {
   fetchSpots().then((resp) => {
       resp.resp.map((spot) => {
           coordinates = JSON.parse(spot.coordinates.replace('(', '[').replace(')', ']'));
           spots.push(new Spot(spot.title, coordinates));
       });
   }); 
}

/*
 * Functions to work with Riders
 */

function updateRiders() {
    fetchRiders().then((resp) => {
        resp.resp.map((rider) => {
            riders.set(rider.uuid, new Rider(rider.nickname, rider.uuid));
        });
    });
}

function fetchRiders() {
    return fetch('http://' + apiURL + '/api/riders')
    .then(resp => resp.json());
}

function updateRidersInformation() {
    let ridersList = document.getElementById('riders');
    ridersList.innerHTML = "";

    for (let rider of riders.values()) {
        listItem = document.createElement('li');
        listItem.innerHTML = rider.name + ': ' + rider.uid;
        
        ridersList.appendChild(listItem);
    }
}

function drawRiders() {
    for (let rider of riders.values()) {
        if (rider.uid != me.uid) {
            drawRider(rider);
        }
    }
}

let myUid = prompt("Insert your uid", "");
let me = new Rider("", myUid);

let socket = new WebSocket('ws://' + wsURL + '/coord/' + me.uid);

socket.onopen = function(e) {
    console.log(e);
}

socket.onmessage = function(event) {
    [riderUid, riderCoordinates] = event.data.split(':');
    if (riderCoordinates) {
        [riderX, riderY] = riderCoordinates.split(';');
    }

    rider = riders.get(riderUid.trim());
    if (rider) {
        rider.x = parseFloat(riderX);
        rider.y = parseFloat(riderY);

        console.log(riderUid);
        console.log(riderCoordinates);
        console.log(event.data);
    }
}

socket.onclose = function(event) {
    alert("Connection to websocket got closed" + event.code);
}

socket.onerror = function(error) {
    alert(error);
}

function sendMyCoordinates() {
    let formattedCoordinates = `(${me.x}.0;${me.y}.0)`;
    socket.send(formattedCoordinates);
}

function draw() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    updateMyInfo(me);
    updateMyLocation(me);
    drawRider(me);
    drawSpots(spots);
    drawRiders();
}

document.addEventListener("keydown", keyDownHandler, false);
document.addEventListener("keyup", keyUpHandler, false);
updateRiders();
updateRidersInformation();
createSpots();


setInterval(draw, 20);
setInterval(sendMyCoordinates, 200);
setInterval(updateRiders, 10000);
setInterval(updateRidersInformation, 2000);
