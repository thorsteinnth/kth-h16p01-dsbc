c(ping).

Start node 1:
erl -name node1@192.168.1.8 -setcookie secret
Start node 2:
erl -name node2@192.168.1.8 -setcookie secret

On pong node:
P = spawn(ping, pong, []).
register(pongproc, P).

On ping node:
ping:ping('pongproc', 'node2@192.168.1.8').