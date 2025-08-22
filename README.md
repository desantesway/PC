# Concurrent Programming / Programação Concorrente

## 🏆 Grade: 19/20

A third-year academic project focused on building a **real-time, multiplayer game** with a **high-performance server architecture**.  
The main challenge was designing a **client-server system** capable of handling **massive concurrent requests** using **multithreading**, while preventing **race conditions** and **deadlocks**.

---

## 🎮 Gameplay
- Players control a **planet orbiting a central sun**, navigating with `W`, `A`, `S`, `D`.
- **Collisions**:
  - With obstacles → the player dies.  
  - With other players → a **realistic bounce effect** occurs, altering momentum and trajectory.
- Additional features:
  - Display available rooms
  - Create / join rooms
  - Account creation & login system
  - XP / Level system
  - Global leaderboard of all players

---

## 🛠️ Tech Stack
- **Frontend**: Java with **Processing** for graphics, physics, and input handling.  
- **Backend**: **Erlang**, leveraging its concurrency model to manage:
  - Real-time player state  
  - Collision resolution  
  - Scalable, robust server performance  

---

## 🚀 Installation & Running

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

