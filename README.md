# Example code: Microservice server, each thread is a service, all services run concurrently.

1. To run the code, clone the repo then follow [GHCup](https://www.haskell.org/ghcup/) to install the nesscessary toolchain (this repo uses the stack toolchain).
2. When the installation finishes, fire up your terminal, change direction to your cloned repo then run **stack run** to build and execute the code.
   (You can alternatively run **stack ghci src/DevelMain.hs** to try the interactive repl, everytime the code is modified and saved, run **:r** then **update** in the repl session, to start the code run **update**)

**🦥 There is a front interface for this server too, please checkout 👉 [Client](https://github.com/DickyDicky7/microservice_client_haskell)**
