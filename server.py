#!/usr/bin/env python
import zmq
import sys
import completion

run_once = False

if len(sys.argv) > 1 and sys.argv[1] == "--run_once":
    run_once = True

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5556")

print "* Starting DC completion server.."

while True:
    message = socket.recv()
    res = completion.handle_completion_str(message)
    socket.send(res)
    if run_once:
        break
