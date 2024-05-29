import java.util.*;
import java.util.concurrent.locks.*;

public class GameState {
    /*
     * Since GameState will be continuously updated by the server and read by the client, 
     * we need to implement a ReadWriteLock to ensure that the data is consistent.
     */
    private int boost;
    public ReadWriteLock l = new ReentrantReadWriteLock(); 
    public Map<String, float[]> positions;  // <Username, float[2]>
    public Map<String, float[]> planets;
    public boolean countdown;
    
    public GameState() {
        this.boost = 100;
        this.l = new ReentrantReadWriteLock();
        this.positions = new HashMap<>();
        this.planets = new HashMap<>();
        this.countdown = false;
    }
    

    public GameState(int boost, Map<String, float[]> pos, Map<String, float[]> ps, boolean countdown){
        this.boost = boost;
        this.positions = pos;
        this.planets = ps;
        this.countdown = countdown;
    }
    
    public GameState copy() {
      return new GameState(this.boost, this.positions, this.planets, this.countdown);

    }
    
    public Map<String,float[]> getPos() {
      return this.positions;
    }
    
    public void setPos(String user, float x, float y, float angle) {
        this.positions.put(user, new float[]{x, y, angle});
    }

    public void setPlanetPos(String i,float x, float y, float velX, float velY) {
              System.out.println("Inserting new planet pos ("+x+","+y);

        this.planets.put(i, new float[]{x, y, velX, velY});
    }

    public void setCountdown(boolean countdown){
        this.countdown = countdown;
    }
    

    public void setBoost(int booster){
        this.boost = booster;
    }
    
    public boolean example() {
      return this.countdown;
    }
}
