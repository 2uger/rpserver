import requests
import asyncio
import websockets
from threading import Thread

from coordinates import (
    parse_recv_msg, 
    start_moving, 
    sendCoordinates, 
    recvCoordinates,
)
import config


async def recv(ws, my_id):
    while True:
        recv = await ws.recv()
        print('Recv message:', recv)
        r_id, coords = parse_recv_msg(recv)
        if r_id == my_id:
            continue
        if coords == [0.0,0.0]:
            continue
        recvCoordinates.append(recv)


async def send(ws):
    while True:
        await asyncio.sleep(0.1)
        while True:
            if len(sendCoordinates):
                send = sendCoordinates.pop(0)
                break
            else:
                await asyncio.sleep(0.1)
        await ws.send(send)


async def client():
    resp = requests.get('http://0.0.0.0:8000/api/riders')
    if resp.status_code != 200:
        print('Bad status code')
        return
    riders = resp.json()['resp']
    if not riders:
        print('No riders choice')
        return
    for rider in riders:
        print(rider['nickname'], ' ', rider['hometown'])
    rider_choice = input(str)
    flw_uid = riders[int(rider_choice)]['uuid']
    config.UID = flw_uid
    print('You uid is:', config.UID)

    uri = f"ws://localhost:9999/coord/{config.UID}"
    while True:
        await asyncio.sleep(1)
        try:
            async with websockets.connect(uri, max_queue=1) as websocket:
                _ = await websocket.recv()
                await asyncio.gather(recv(websocket, config.UID), send(websocket))
        except Exception as e:
            print(e)


def run_client():
    asyncio.run(client())


def run_map():
    print('Run map to send and recv coordinates from server')
    start_moving()


if __name__ == '__main__':
    t1 = Thread(target=run_client)
    t1.start()
    # pygame should run in main thread
    run_map()
