class Planet {
  PImage[] images;
  int imageCount;
  int counter;
  
  float radius = 100f;
  PVector pos, vel;
  float topspeed = 5f;
  PVector sunPos = new PVector(displayWidth/2,displayHeight/2);
  boolean registered;
  
  Planet() {
    this.registered = false;
  }
  
  Planet(String model, int imageCount, float x, float y, float vx, float vy) {
    this.pos = new PVector(x,y);
    this.vel = new PVector(vx,vy);
    this.imageCount = imageCount;
    images = new PImage[imageCount];

    for (int i = 0; i < imageCount;i++){
      String filename = "planets/"+model + "_" + i + ".png"; //!!!!!!!!!!!!

      images[i] = loadImage(filename);
      images[i].resize((int)radius,(int)radius);
    }
    this.registered = true;
    println("I really made him");
  }
  
  boolean getStatus() {
    return this.registered;
  }
  
  void setPosVel(float x, float y, float vx, float vy) {
    this.pos = new PVector(x,y);
    this.vel = new PVector(vx,vy);
  }
  
  void update() {
     
    PVector acceleration = PVector.sub(sunPos, pos);
    acceleration.setMag(0.03);
    vel.add(acceleration);
    // Limit the velocity by topspeed
    vel.limit(topspeed);
    // Location changes by velocity
    pos.add(vel);
  }
 
  void display() {
    image(images[0],pos.x-5,pos.y-10);
  }
}
