let canvas = document.getElementById("map");
let ctx = canvas.getContext("2d");

let serverUri = '0.0.0.0:8000';

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
    }
    coordinates = {long: 0,  // x
                   latt: 0}; // y
    moveUp() {
        this.coordinates.latt -= dy;
    }
    moveDown() {
        this.coordinates.latt += dy;
    }
    moveRight() {
        this.coordinates.long += dx;
    }
    moveLeft() {
        this.coordinates.long -= dx;
    }
    get x() {
        return this.coordinates.long;
    }
    set x(new_x) {
        this.coordinates.long = new_x;
    }
    get y() {
        return this.coordinates.latt;
    }
    set y(new_y) {
        this.coordinates.latt = new_y;
    }
}

class Spot {
    constructor(name='', coordinates=[0, 0]) {
        this.name = name;
        this.coordinates = {long: coordinates[0],
                            latt: coordinates[1]};
    }
    get x() {
        return this.coordinates.long;
    }
    set x(new_x) {
        this.coordinates.long = new_x;
    }
    get y() {
        return this.coordinates.latt;
    }
    set y(new_y) {
        this.coordinates.latt = new_y;
    }
}

function drawRider(rider) {
    ctx.beginPath();
    ctx.arc(rider.x, rider.y, ballRadius, 0, Math.PI * 2, false);
    ctx.fillStyle = "red";
    ctx.fill();
    ctx.closePath();
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

let riders = [];
let spots = [];

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
    return fetch('http://' + serverUri + '/api/spots')
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

function createRiders() {
    fetchRiders().then((resp) => {
        resp.resp.map((rider) => {
            riders.push(new Rider(rider.nickname));
        });
    });
}

function fetchRiders() {
    return fetch('http://' + serverUri + '/api/riders')
    .then(resp => resp.json());
}

function updateRidersInformation() {
    listContainer = document.createElement('div');
    listElement = document.createElement('ul');

    document.getElementById('riders').appendChild(listContainer);
    listContainer.appendChild(listElement);

    for (let rider of riders) {
        listItem = document.createElement('li');
        listItem.innerHTML = rider.name;
        
        listElement.appendChild(listItem);
    }
}

/*
 * Fetch new riders coordinates and update them
 */
function updateRidersCoordinates() {

}
let me = new Rider('Oleg', 'uid');

function draw() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    updateMyInfo(me);
    updateMyLocation(me);
    drawRider(me);
    drawSpots(spots);
}

function init() {
    document.addEventListener("keydown", keyDownHandler, false);
    document.addEventListener("keyup", keyUpHandler, false);
    createRiders();
    createSpots();
    updateRidersInformation();

}

let interval = setInterval(draw, 20);
init();
