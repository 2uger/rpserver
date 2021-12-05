import time
from threading import Thread
import asyncio
import websockets

from coordinates import parse_recv_msg, start_moving, sendCoordinates, recvCoordinates

async def recv(ws, my_id):
    while True:
        recv = await ws.recv()
        r_id, coords = parse_recv_msg(recv)
        if r_id == my_id:
            continue
        if coords == [0,0]:
            continue
        if len(recvCoordinates) == 1:
            recvCoordinates.pop(0)
        print(recv)
        recvCoordinates.append(recv)

async def send(ws):
    last_send = '(0;0)'
    while True:
        await asyncio.sleep(0.1)
        try:
            send = sendCoordinates.pop(0)
            last_send = send
        except Exception as e:
            send = last_send
        await ws.send(send)


async def client():
    uri = "ws://localhost:9999/coord"
    try:
        async with websockets.connect(uri, max_queue=1) as websocket:
            my_id = await websocket.recv()
            my_id = int(my_id)
            t2 = Thread(target=run_map, args=(my_id,))
            t2.start()
            await asyncio.gather(recv(websocket, my_id), send(websocket))
    except Exception as e:
        raise e


# Simple websocket client to communicate with server to send and receive
# new coordinates
def run_client():
    asyncio.run(client())

def run_map(my_id):
    print('Run map to send and recv coordinates from server')
    start_moving(my_id)

if __name__ == '__main__':
    t1 = Thread(target=run_client)

    t1.start()
