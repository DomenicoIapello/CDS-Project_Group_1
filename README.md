# Distributed Tic Tac Toe

## Description
The project is a Distributed Tic Tac Toe that can be played in a browser. It is developed in Erlang for the backend, and with Electron App for the frontend.

In the current directory, you have access to archived/tarball of both the backend and frontend are available in his directory. You also have access to the source code in the subfolder [backend](./backend/) and [frontend](./frontend/) respectively.

Please see next sections for full instructions.

## Project directory structure

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
│   ├── frontend.html
│   ├── main.js
│   ├── package.json
│   ├── package-lock.json
│   ├── README.md
│   ├── stylesheet.css
│   └── ttt.js
├── AgeofTicTacToe Setup 1.0.0.exe
├── backend_tarball.tar.gz
└── README.md
</pre>


## Run the Application

The backend has been tested on Linux/Ubuntu only. The frontend has been tested on Windows.

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
  - frontend’s executable: `AgeofTicTacToe Setup 1.0.0.exe`,
  - backend’s tarball:  `ttt-backend-0.1.0.tar.gz`
- Prepare the backend by either
  - Going into the subfolder `backend` and manually building it with Rebar3 instructions.
  - Or (recommended) use the `ttt-backend-0.1.0.tar.gz`. Unpack where you prefer. A lengthy example would be:
    <pre>$ cd ..
    $ mkdir ttt_backend
    $ cd ttt_backend
    $ tar -zxvf ttt-0.1.0.tar.gz
    $ bin/ttt console</pre>
  - You now should have a running backend, with three processes launched, an HTTP server listening carefully.
 - Prepare the frontend
   - Install the `AgeOfTicTacToe Setup 1.0.0.exe` on your computer and start the application. 
 - Enjoy!
