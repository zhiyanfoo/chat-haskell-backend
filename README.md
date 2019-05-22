# haskell-server

This is the backend for a chat app I developed. To run it locally if you have docker installed
just do the following
```bash
git clone 'https://github.com/zhiyanfoo/chat-haskell-backend.git'
cd chat-haskell-backend
docker-compose up
```

If you are running on mac or windows you might want to change the default ip address
to `0.0.0.0`.

The default port for the server is `8989`. Currently this is just a websocket server

The docker-compose file is not optimized for development, its just used for deployment.
To develop on this on your local, it is recommended you use a local installation of stack.

To run the tests using stack you can do the following

```bash
stack build
stack ghci haskell-server:haskell-server-test
```

The frontend can be found [here](https://github.com/zhiyanfoo/ui)
