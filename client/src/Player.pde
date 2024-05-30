class Player {
  String username;
  float x,y;
  PVector velocity;
  PVector acceleration;
  float angle;
  // The player's maximum speed
  float topspeed;
  float radius = 25f; // default radius of a player 
  PVector sunPos = new PVector(displayWidth/2, displayHeight/2);
  int booster = 100;
  PImage PlayerImg, ShadowImg;
  boolean registered;
  
  Player() {
    this.x = 200;
    this.y = 200;
    this.username = "";
    velocity = new PVector(0,0);
    this.topspeed = 10;
    this.booster = 100;
    PlayerImg = loadImage("/player/player.png");
    ShadowImg = loadImage("/player/player_shadow.png");
    PlayerImg.resize(50, 50);
    ShadowImg.resize(25, 50);
    this.registered = false;
  }
  
  Player(float x, float y, float a, String user) {
    this.x = x;
    this.y = y;
    this.username = user;
    this.angle = a;
    this.registered = true;
    this.topspeed = 10;
    this.booster = 100;
    PlayerImg = loadImage("/player/player.png");
    ShadowImg = loadImage("/player/player_shadow.png");
    PlayerImg.resize(50, 50);
    ShadowImg.resize(25, 50);
  }

  int getBoost() {
    return this.booster;
  }
  
  void setAngle(float a) {
    this.angle = a;
  }
  
  void setPos(float x, float y) {
    this.x = x;
    this.y = y;
  }
  
  void setUsername(String user) {
    this.username = user;
  }
  
  void setVelocity(PVector vel) { // Possibly unused
    this.velocity = vel;
  }
 
  boolean getStatus() {
    return this.registered;
  }
  
  void display() { //!!!!!!!!!!!!!
    PVector v1 = new PVector(300, displayHeight/2) ;
    pushMatrix();
    textFont(campus, 25);
    translate(17,65);
    text(this.username, x, y);
    popMatrix();
    line(x,y,((x+30)*cos(angle)), ((y+30)*sin(angle)));
    image(PlayerImg, this.x, this.y);
    pushMatrix();
    translate(x+25, y+25);
    PVector direction = PVector.sub(sunPos, new PVector(x,y));
    rotate(PI+atan2(direction.y, direction.x));
    image(ShadowImg, 0, -25); // rotate in relation of sun position
    

    popMatrix();
  }
}
