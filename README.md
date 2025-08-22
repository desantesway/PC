# Concurrent programming / Programação concorrente

## Grade 19/20 ⭐

A third-year academic project to develop a real-time, multiplayer game with a high-performance server architecture. The core technical challenge was designing a client-server system capable of handling massive concurrent requests using multithreading while preventing race conditions and deadlocks.

- Gameplay: Players control a planet orbiting a central sun, using the A, W, S, and D keys to navigate. Collisions with obstacles kills the player, with other players cause a realistic "bounce" effect, altering momentum and trajectory. It can also display rooms available, create, join rooms, create/login accounts, xp/level system and display a leaderboard of all players. 

- Frontend: Developed in Java using the Processing framework for graphics, physics, and user input.

- Backend: Built in Erlang for its renowned concurrency model, ensuring a robust and scalable server to manage real-time player state and collision resolution.

## Installing and running the project

1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/desantesway/PC.git
    cd PC
    ```
2. **Run the server first**
   
   2.1. **Server directory**
   ```bash
    cd server
   ```

   2.2. **Run the server in your desired IDE that supports Erlang**
   
3. **Run the client**
   
   3.1. **Client directory**
   ```bash
    cd client
   ```

   3.2. **Run the client in your processing IDE**

