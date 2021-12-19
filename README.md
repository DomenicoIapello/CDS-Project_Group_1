# Distributed Tic Tac Toe

## Description
-----
The project is a Distributed Tic Tac Toe that can be played in a browser. It is developed in Erlang for the backend, and with Electron App for the frontend.

## Getting Ready
-----

<pre>
$ git clone this_repository_http_or_ssh
</pre>

## Project directory structure
-----

<pre>
├── backend
│   ├── ebin
│   │   └── ttt.app
│   ├── src
│   │   ├── controllers
│   │   │   ├── gameserver_process.erl
│   │   │   ├── playerone_process.erl
│   │   │   └── playertwo_process.erl
│   │   ├── middleware
│   │   │   └── ca_cowboy_middleware.erl
│   │   ├── routes
│   │   │   ├── gameserver_route.erl
│   │   │   └── health_route.erl
│   │   ├── ttt_app.erl
│   │   └── ttt_sup.erl
│   ├── README.md
│   ├── rebar.config
│   └── rebar.lock
├── frontend
│   ├── ...
│   ├── README.md
│   ├── ...
│   └── ...
├── frontend_executable.exe
├── backend_tarball.tar.gz
└── README.md
</pre>


## Run
-----
Follow the README respectively for the [backend](./backend/README.md) and the [frontend](./frontend/README.md).

Instructions to use and deploy our code:
- Clone the repository, either one of (HTTPS or SSH): 
  <pre> $git clone https://github.com/DomenicoIapello/CDS-Project_Group_1.git </pre>
  <pre> $git clone git@github.com:DomenicoIapello/CDS-Project_Group_1.git </pre>
- Move to the new directory: 
  <pre> $cd CDS-Project_Group_1 </pre>
- You now are in a folder with
  - subfolder: `backend`
  - subfolder: `frontend`
  - file: `README.md`, (contains these instructions as well)
  - frontend’s executable: `age_of_tictactoe.exe`,
  - backend’s tarball:  `ttt-0.1.0.tar.gz`
- Prepare the backend by either
  - Going into the subfolder `backend` and manually building it with Rebar3 instructions.
  - Or (recommended) use the `ttt-0.1.0.tar.gz`. Unpack where you prefer. A lengthy example would be:
    <pre>$ cd ..
    $ mkdir ttt_backend
    $ cd ttt_backend
    $ tar -zxvf ttt-0.1.0.tar.gz
    $ bin/ttt console</pre>
  - You now should have a running backend, with three processes launched, an HTTP server listening carefully.
 - Once the backend is running and live, launch the frontend executable (you can move it wherever you want as long as it has access to the localhost still). This will launch an Electron app / window. 
 - Enjoy!
