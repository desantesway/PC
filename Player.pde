class Player {
  PVector location;
  PVector velocity;
  PVector acceleration;
  // The player's maximum speed
  float topspeed;
  float[] playerColors;
  float radius = 25f; // default radius of a player 
  PVector sunPos = new PVector(displayWidth/2, displayHeight/2);
  float booster = 1000f;

  Player() {
    location = new PVector(200,200);
    velocity = new PVector(0,0);
    topspeed = 10;
    playerColors = new float[3];
    for(int i = 0; i < 3 ; i++) {
      playerColors[i] = random(0,255);
    }
  }

  void outOfBoost() {
    PVector sun = new PVector(displayWidth/2, displayHeight/2);
      PVector acceleration = PVector.sub(sun, location);
      
      // All the following values were obtained by pure trial and error, no math was harmed in this experiment
      acceleration.setMag(0.08);
      velocity.add(acceleration);
      // Limit the velocity by topspeed
      velocity.limit(topspeed);
      // Location changes by velocity
      location.add(velocity);

  }
  
  
  void applyGravity(HashMap<Character, Boolean> keyMap) {
    PVector sun = new PVector(displayWidth/2, displayHeight/2);
    PVector acceleration = PVector.sub(sun, location);
      
      // All the following values were obtained by pure trial and error, no math was harmed in this experiment
    acceleration.setMag(0.1);

      // Modify acceleration based on user input
    PVector controlAcceleration = new PVector(0, 0);
    if (keyMap.get('w')) {
        controlAcceleration.add(0, -0.2); // Move up
        booster -= 0.5f;
    }
    if (keyMap.get('a')) {
        controlAcceleration.add(-0.2, 0); // Move left
        booster -= 0.5f;
    }
    if (keyMap.get('s')) {
        controlAcceleration.add(0, 0.2); // Move down
        booster -= 0.5f;
    }
    if (keyMap.get('d')) {
        controlAcceleration.add(0.2, 0); // Move right
        booster -= 0.5f;
    }

    // Combine acceleration vectors
    acceleration.add(controlAcceleration);
    // Velocity changes according to acceleration
    velocity.add(acceleration);
    // Limit the velocity by topspeed
    velocity.limit(topspeed);
    // Location changes by velocity
    location.add(velocity);
  }
  
  boolean checkCollisions() {
    float distance = PVector.dist(location, sunPos);
    if (distance < radius + 120) {
      return true;
    }
    return (location.x < 0 || location.x > displayWidth || location.y < 0 || location.y > displayHeight);  
  }
  
  float getBoost() {
    return booster;
  }
  void display() {
    stroke(255);
    strokeWeight(2);
    fill(playerColors[0], playerColors[1], playerColors[2]);
    ellipse(location.x,location.y,radius,radius);
  }
}
