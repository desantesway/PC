


public class Worker extends Thread {
    public TCP tcp;
    public GameState gameState;
    public ChatRoom chat;

    public Worker(TCP tcp, ChatRoom chat) {
        this.tcp = tcp;
        this.chat = chat;
    }
    public Worker(TCP tcp, GameState gameState) {
        this.tcp = tcp;
        this.gameState = gameState;
    }
}

class PosWorker extends Worker {
    private final String index;

    public PosWorker(TCP tcp, GameState gameState, String index) {
        super(tcp, gameState);
        this.index = index;
    }
    

    public void run() {
        String res;
        try {
            while((res = this.tcp.receive("pos"+ this.index)) != null) {
                String[] pos = res.split("@@@");
                try {
                  this.gameState.l.readLock().lock();
                  String user = pos[0];
                  // pos1@@@Username@@@Boost@@@x@@@y@@@Angle
                  //this.gameState.setBoost();
                  this.gameState.setPos(user, Integer.parseInt(pos[1]), Float.parseFloat(pos[2]), Float.parseFloat(pos[3]), Float.parseFloat(pos[4]));
                } finally {
                    this.gameState.l.readLock().unlock();
                }
            }
        } catch (Exception e) {
          e.printStackTrace();
        }
    }
}

class PlanetWorker extends Worker {
    private final String index;

    public PlanetWorker(TCP tcp, GameState gameState, String index) {
        super(tcp, gameState);
        this.index = index;
    }

    public void run() {
        String res;
        try {
            while((res = this.tcp.receive("p"+ this.index)) != null) {
                String[] pos = res.split("@@@");
                try {
                  this.gameState.l.readLock().lock();
                  // p1@@@x@@@y@@@velx@@@vely
                  this.gameState.setPlanetPos(this.index, Float.parseFloat(pos[0]), Float.parseFloat(pos[1]), Float.parseFloat(pos[2]), Float.parseFloat(pos[3]));
                } finally {
                  this.gameState.l.readLock().unlock();
                }
            }
        } catch (Exception e) {
          e.printStackTrace();
        }
    }
}

class GameWorker extends Worker {
    public GameWorker(TCP tcp, GameState gameState) {
        super(tcp, gameState);
    }

    public void run() {
        String res;
        try {
            while((res = this.tcp.receive("game")) != null) {
                String msg[] = res.split("@@@");
                System.out.println(msg[0]);
                if (msg.length > 1) {
                  if (msg[1].equals("died")) {
                    try {
                      this.gameState.l.readLock().lock();
                      this.gameState.setDeath(msg[0]);
                    } finally {
                      this.gameState.l.readLock().unlock();
                    } 
                }
                } else {
                  
                switch (msg[0]) {
                    case "countdown_start":
                        try{
                          this.gameState.l.readLock().lock();
                          this.gameState.setCountdown(true);
                        } finally {
                          this.gameState.l.readLock().unlock();
                        }
                        break;
                    case "countdown_end":
                        try{
                            this.gameState.l.readLock().lock();
                            this.gameState.setCountdown(false);
                        } finally {
                          this.gameState.l.readLock().unlock();
                        }
                        break;
                    case "won_game":
                        try{
                            this.gameState.l.readLock().lock();
                            this.gameState.setWon();
                        } finally {
                          this.gameState.l.readLock().unlock();
                        }                      
                        break;
                    case "lost_game":
                        try{
                            this.gameState.l.readLock().lock();
                            this.gameState.setLost();
                        } finally {
                          this.gameState.l.readLock().unlock();
                        }                      
                        break;                      
                    default:
                        break;
                    
                }
                }
            }
        } catch (Exception e) {
          e.printStackTrace();
        }
    }
}

class ChatWorker extends Worker {

  public ChatWorker(TCP tcp, ChatRoom chat) {
      super(tcp,chat);
  }

  public void run() {
      String res;
      try {
          while((res = this.tcp.receive("chat")) != null) {
              String[] msg = res.split("@@@");
              try {
                this.chat.l.readLock().lock();
                if (msg[0].equals("msg")) {      // msg@@@username@@@message
                  this.chat.addMessage(msg[1], msg[2]);
                } else if (msg[0].equals("leave")) { // get
                  this.chat.addMessage("", msg[1] + " has left the chat");
                }
                
              } finally {
                this.chat.l.readLock().unlock();
              }
          }
      } catch (Exception e) {
        e.printStackTrace();
      }
  }
}
