{
    application, 
    ttt,
    [
        {description, "A Distributed Tic-Tac-Toe Game"},
        {vsn, "0.1.0"},
        {registered, [ttt_app, ttt_sup,
                       gameserver_process, 
                       gameserver_route]},              % names registered by our app: OTP will know when names clash.
        {mod, {ttt_app, []}},          % the callback module to call when starting our app
        {applications, [               % dependences of our app
                        kernel,
                        stdlib,
                        cowboy,
                        jiffy
                    ]
        },
        {env,[]},                      % list of key/values used as config for our app
        {modules, [ttt_app, ttt_sup, 
                    gameserver_process, gameserver_route]},                 % modules introduced by our app to the system
        {licenses, ["Apache 2.0"]},
        {links, []}
    ]
}.