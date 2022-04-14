let apiURL = '0.0.0.0:8000';
let wsURL = '0.0.0.0:9999';

// constants
let ballRadius = 5;
let dx = 0.7;
let dy = 0.7;

let rightPressed = false;
let leftPressed = false;
let upPressed = false;
let downPressed = false;

let me = null;
let riders = new Map();
let spots = [];
let socket = null;

class Rider {
    constructor(name = '', uid='') {
        this.name = name;
        this.uid = uid;
        this.coordinates = {long: 0.1,  // x
                            latt: 0.1}; // y
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
    if(e.key == 'Right' || e.key == 'ArrowRight') {
        rightPressed = true;
    }
    else if(e.key == 'Left' || e.key == 'ArrowLeft') {
        leftPressed = true;
    }
    else if(e.key == 'Up' || e.key == 'ArrowUp') {
        upPressed = true;
    }
    else if(e.key == 'Down' || e.key == 'ArrowDown') {
        downPressed = true;
    }
}

function keyUpHandler(e) {
    if(e.key == 'Right' || e.key == 'ArrowRight') {
        rightPressed = false;
    }
    else if(e.key == 'Left' || e.key == 'ArrowLeft') {
        leftPressed = false;
    }
    else if(e.key == 'Up' || e.key == 'ArrowUp') {
        upPressed = false;
    }
    else if(e.key == 'Down' || e.key == 'ArrowDown') {
        downPressed = false;
    }
}

function drawRider(ctx, rider) {
    ctx.beginPath();
    ctx.arc(rider.x, rider.y, ballRadius * 1.2, 0, Math.PI * 2, false);
    ctx.fillStyle = 'red';
    ctx.fill();
    ctx.closePath();

    ctx.beginPath();
    ctx.font = '12px serif';
    ctx.fillText(rider.name, rider.x - 10, rider.y - 10);
    ctx.closePath();
}

function updateMyLocation(rider) {
    let moved = false;

    rightPressed ? (rider.x += dx, moved = true) : null;
    leftPressed ? (rider.x -= dx, moved = true) : null;
    upPressed ? (rider.y -= dy, moved = true) : null;
    downPressed ? (rider.y += dy, moved = true) : null;

    moved ? sendMyCoordinates() : null;
}

/*
 * Functions to work with Spots
 */
function drawSpots(ctx) {
    drawSpot = function(spot) {
        ctx.beginPath();
        ctx.arc(spot.x, spot.y, ballRadius * 1.5, 0, Math.PI * 2, false);
        ctx.fillStyle = 'black';
        ctx.fill();
        ctx.closePath();

        ctx.beginPath();
        ctx.font = '16px serif';
        ctx.fillText(spot.name, spot.x - 10, spot.y - 10);
        ctx.closePath();
    }
    spots.forEach(drawSpot);
}

function fetchSpots() {
    return fetch('http://' + apiURL + '/api/spots')
    .then(resp => resp.json());
}

async function createSpots() {
    let resp = await fetchSpots();
    resp.resp.forEach((spot) => {
            let coordinates = JSON.parse(spot.coordinates.replace('(', '[').replace(')', ']'));
            spots.push(new Spot(spot.title, coordinates));
        });
}

/*
 * Functions to work with Riders
 */
function fetchRiders() {
    return fetch('http://' + apiURL + '/api/riders')
    .then(resp => resp.json());
}

async function createRiders() {
    riders.clear();
    let resp = await fetchRiders();
    resp.resp.forEach((rider) => {
        riders.set(rider.uuid, new Rider(rider.nickname, rider.uuid));
    })
}

function updateRidersInformation() {
    let ridersList = document.getElementById('riders');
    ridersList.innerHTML = '';

    for (let rider of riders.values()) {
        listItem = document.createElement('li');
        listItem.innerHTML = rider.name + ': ' + rider.uid;
        
        ridersList.appendChild(listItem);
    }
}

function drawRiders(ctx) {
    for (let rider of riders.values()) {
        if (rider.uid != me.uid) {
            drawRider(ctx, rider);
        }
    }
}

async function registration() {
    let nickname = prompt('Your nickname', '');
    let password = prompt('Your password', '');

    const resp = await fetch('http://' + apiURL + '/auth/sign-up',
        {method: 'POST',
         headers: { 'Content-Type': 'application/json' },
         body: JSON.stringify({'nickname': nickname, 
                               'password': password})})
        .catch(err => { console.log(err); });

    if (resp && resp.status != 200) {
        alert(resp.status);
        throw new Error('Bad response while sign up');
    }
    if (!resp || resp.status != 200) {
        alert('Fetch error while sign up');
        throw new Error('Bad response while sign up');
    }

    const respJson = await resp.json();

    return new Rider(nickname, respJson.resp.uuid);
}

async function login() {
    let nickname = prompt('Your nickname', '');
    let password = prompt('Your password', '');

    const resp = await fetch('http://' + apiURL + '/auth/sign-in',
        {method: 'POST',
         headers: { 'Content-Type': 'application/json' },
         body: JSON.stringify({'nickname': nickname, 
                               'password': password})})
        .catch(err => { console.log(err); });

    if (resp && resp.status != 200) {
        alert(resp.status);
        throw new Error('Bad response while sign in');
    }
    if (!resp || resp.status != 200) {
        throw new Error('Bad response while sign in');
    }

    const respJson = await resp.json();
    let uid = respJson.resp.uid;

    return new Rider(nickname, respJson.resp.uid);
}

function setupWS() {
    socket = new WebSocket('ws://' + wsURL + '/coord/' + me.uid);

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
        }
    }

    socket.onclose = function(event) {
        alert('Connection to websocket got closed: ' + event.code);
    }

    socket.onerror = function(error) {
        alert(error);
    }
}

function updateMyInfo() {
    let name = document.getElementById('name');
    let uid = document.getElementById('uid');
    let latt = document.getElementById('latt');
    let long = document.getElementById('long');

    name.textContent = me.name;
    uid.textContent = me.uid;
    latt.textContent = me.x;
    long.textContent = me.y;
}

function sendMyCoordinates() {
    let formattedCoordinates = `(${me.x};${me.y})`;
    socket.send(formattedCoordinates);
}

function draw(canvas, ctx) {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    updateMyInfo(me);
    updateMyLocation(me);
    drawRider(ctx, me);
    drawSpots(ctx);
    drawRiders(ctx);
}

async function main() {
    let canvas = document.getElementById('map');
    let ctx = canvas.getContext('2d');

    while (true) {
        try {
            let isReg = prompt('Sig-in: 1, Sign-up: 2', 1);
            me = isReg == '1' ? await login() : await registration();
            break;
        } catch(err) {
            alert(err.message);
        }
    }

    // add handlers for updating spots and riders data
    let updateRidersButton = document.getElementById('updateRiders');
    updateRidersButton.addEventListener('click', createRiders);
    let updateSpotsButton = document.getElementById('updateSpots');
    updateSpotsButton.addEventListener('click', createSpots);

    setupWS();

    setInterval(() => { draw(canvas, ctx); }, 20);
    setInterval(() => { updateRidersInformation(); }, 2000);

    document.addEventListener('keydown', keyDownHandler, false);
    document.addEventListener('keyup', keyUpHandler, false);
}

main();
