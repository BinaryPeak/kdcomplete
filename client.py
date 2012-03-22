#!/usr/bin/env python
import sys
import zmq

if len(sys.argv) > 1:
    filename = sys.argv[1]   

port = 5556

if len(sys.argv) > 2:
    port = int(sys.argv[2])

context = zmq.Context()
socket = context.socket(zmq.REQ)

socket.connect ("tcp://localhost:%d" % port)

content = open(filename, "r").read()
socket.send(content)
message = socket.recv()
print message
