
import java.io.IOException;


public class Worker extends Thread {
    public TCP tcp;
    public GameState gameState;
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
                  this.gameState.setBoost(Integer.parseInt(pos[1]));
                  this.gameState.setPos(user, Float.parseFloat(pos[2]), Float.parseFloat(pos[3]), Float.parseFloat(pos[4]));
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
                switch (res) {
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
                    default:
                        break;
                    
                }
            }
        } catch (Exception e) {
          e.printStackTrace();
        }
    }
}
