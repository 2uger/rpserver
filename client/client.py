import random
import time
from threading import Thread
import asyncio
import websockets

from coordinates import parse_recv_msg, start_moving, sendCoordinates, recvCoordinates
import config


async def recv(ws, my_id):
    while True:
        recv = await ws.recv()
        r_id, coords = parse_recv_msg(recv)
        if r_id == my_id:
            continue
        if coords == [0.0,0.0]:
            continue
        if len(recvCoordinates) == 1:
            recvCoordinates.pop(0)
        print(recv)
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
    uid = random.choice([1, 23, 5, 51, 90, 54, 77])
    uri = f"ws://localhost:9999/coord/{uid}"
    try:
        async with websockets.connect(uri, max_queue=1) as websocket:
            config.UID = await websocket.recv()
            print(config.UID)
            await asyncio.gather(recv(websocket, config.UID), send(websocket))
    except Exception as e:
        raise e


# Simple websocket client to communicate with server to send and receive
# new coordinates
def run_client():
    asyncio.run(client())

def run_map():
    print('Run map to send and recv coordinates from server')
    start_moving()

if __name__ == '__main__':
    t1 = Thread(target=run_client)
    t1.start()
    run_map()
