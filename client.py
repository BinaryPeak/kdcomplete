#!/usr/bin/env python
import sys
import zmq

if len(sys.argv) > 1:
    filename = sys.argv[1]   
else:
    filename = "/maps/dnils/completion/testcases/1.txt"

context = zmq.Context()
socket = context.socket(zmq.REQ)

socket.connect ("tcp://localhost:5556")

content = open(filename, "r").read()
socket.send(content)
message = socket.recv()
print message
