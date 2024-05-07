float sun_radius = 135f;
ArrayList<PShape> rings = new ArrayList<PShape>();
Animation sun;
Player player;
HashMap<Character, Boolean> keyMap = new HashMap<>();
PFont nightcore;
ArrayList<Planet> planets = new ArrayList<Planet>();
int state;
final int LOADING = 0; //LOADING MENU
final int GAME = 1; //The Game state
int counter = 60 * 10; // 60 FPS, 5 seconds total

// Loading variables
PFont campus;
int points;
float i, m, n, p, s, t, x, y;


 

void setup() {
  fullScreen(P3D);
  //state = GAME;
  
  // Create a new Animation of the Sun
  sun = new Animation("sun",60);
  // Create a new instance of Player
  player = new Player();
  Planet planet1 = new Planet("planet1", 1);
  planets.add(planet1);
  nightcore = createFont("nightcore.ttf", 200);
  campus = createFont("asupermario.ttf", 150);
  
  // Initialize the keyMap for player input
  keyMap.put('w', false); // W -> false (not pressed)
  keyMap.put('a', false); // A -> false (not pressed)
  keyMap.put('s', false); // S -> false (not pressed)
  keyMap.put('d', false); // D -> false (not pressed)
  
  smooth(8);
  
  
  /*
  Initialize universe scenery contained within the rings array
  */
  float outerRad = 2000f, increment = 100f;
  for(int ringIndex = 0 ; ringIndex < 50 ; ringIndex++){
    PShape ring = createShape();
    ring.setStrokeWeight(random(1f,8f));
    ring.beginShape(POINTS);
    ring.stroke(random(170,255), random(170,255), random(170,255));
    for(int starIndex = 0 ; starIndex < 100 ; starIndex++){
      float a = random(0f, 1f) * TWO_PI;
      float r = sqrt(random(sq(sun_radius)+100, sq(outerRad)*10));
      ring.vertex(r * cos(a), r* sin(a),  60 * random(-increment, increment));
    }
    ring.endShape();
    rings.add(ring);
  }
  
  
  ellipseMode(RADIUS);
  frameRate(60);
}


  // Key pressed method
  void keyPressed() {
    if (keyMap.containsKey(key)) {
      keyMap.put(key, true); // Update key state to pressed
    }
  }

  // Key released method
  void keyReleased() {
    if (keyMap.containsKey(key)) {
      keyMap.put(key, false); // Update key state to released
    }
  }
  

  
void drawRings() {
  pushMatrix();
  translate(displayWidth/2, displayHeight/2);
  rotateZ(PI/1000 * frameCount);
  rotateY(-PI/500 * frameCount);
  rotateX(PI/1000 * frameCount);
  for(PShape r : rings){
    shape(r);    
  }
  popMatrix();
}


void drawSun() {
  pushMatrix();
  translate(0,0,random(-2f,2f));
  fill(random(190,170), 64, 37);
  sun.display();
  popMatrix();
}


void drawPlayer() {
  if (player != null) {
    
    if (player.getBoost() > 0) player.applyGravity(keyMap);
    else {
      player.outOfBoost();
    }
    if (player.checkCollisions()) {
      player = null;
      textFont(nightcore);
      text("YOU LOST", 650, displayHeight/2 - 200); 
    }
    else {
      drawPlayerBoost();
      player.display();
    }
  }
  else {

      textFont(nightcore);
      text("YOU LOST", 650, displayHeight/2-200); 
  }
}

void drawPlanets() {
  for(Planet p: planets) {
    p.display();
  }
}

void drawPlayerBoost() {
  int boost = (int)player.getBoost() / 10;
  textSize(36);
  fill(255);
  if (boost == 0) fill(100);
  text("BOOST:" + boost,displayWidth-200,displayHeight-100);
}

void drawMargins() { // Unused, they looked ugly and out of place
  strokeWeight(3f);
  stroke(255);
  line(0,0,displayWidth,0);                           // horizontal top line
  line(displayWidth,0,displayWidth,displayHeight);    // right vertical line
  line(0,0,0,displayHeight);                          // left vertical line
  line(0,displayHeight, displayWidth, displayHeight); // horizontal bottom line
}

void draw() {
  background(15);
  switch(state) {
    case LOADING:
      textFont(campus);
      if (points < 50) text("LOADING", 580, displayHeight-100);
      else if (points < 100) text("LOADING.", 580, displayHeight-100);
      else if (points < 150) text("LOADING..", 580, displayHeight-100);
      else text("LOADING...", 580, displayHeight-100);
      
      points = (points+1) % 200;
      pushMatrix();
      noStroke();
      fill(255);
      t+=.2;
      translate(850,450);
      for (i=2e3; i>0; i--) {
        p=i%2==0 ? 0 : 1;
        m=t/cos(t/i)+p*(t/2+i%t);
        n=t/9+i*i;
        x=m*2*sin(n)*cos((p==0 ? 1 : 0)*i/t);
        y=m*2*cos(n+p*2);
        s=5-cos(n)*3;
        rect(x, y, s, s);
      }
      popMatrix();
      if (frameCount == counter){
        state = GAME;
        delay(1000);
      }
      break;
    case GAME:
      //drawMargins();
      drawRings();
      drawPlayer();
      //drawPlanets();
      drawSun();
      break;
  }
  //println(frameRate);
}
