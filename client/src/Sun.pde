class Animation {
  PImage[] images;
  int imageCount;
  int frame;
  float sun_radius = 135f;
  Animation(String model, int imageCount) {
    this.imageCount = imageCount;
    images = new PImage[60];

    for (int i = 0; i < imageCount;i++){
      String filename = "sun/" + model + "_" + nf(i,2)+ ".png";
      images[i] = loadImage(filename);
    }
  }
  
  void display() {
    //frame_counter = (frame_counter+1) % imageCount;
    //if (frame_counter % 2 == 0) frame = (frame+1) % imageCount;
    pushMatrix();
    translate(0,random(-1f,1f));
    float x = displayWidth/2;
    float y = displayHeight/2;
    frame = (frame+1) % imageCount;
    noStroke();
    
    ellipse(x,y, sun_radius, sun_radius);
    image(images[frame], x-215, y-215); // Offset due to .png dimensions. Purely aesthetic, won't be used in game logic.
    popMatrix();
  }
}
