#!/usr/bin/env python
import zmq
import sys
import completion

run_once = False

if len(sys.argv) > 1 and sys.argv[1] == "--run_once":
    run_once = True

run_once = True
port = 5556

if len(sys.argv) > 2:
    port = int(sys.argv[2])

context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:%d" % port)

print "* Starting DC completion server.."

while True:
    message = socket.recv()
    res = completion.handle_completion_str(message)
    socket.send(res)

    if run_once:
        print "Exiting"
        break


