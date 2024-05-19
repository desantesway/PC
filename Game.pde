import controlP5.*;
ControlP5 cp5;

enum State {
    MENU,
    LOGIN,
    REGISTER,
    GAME,
    LOADING
}

State state;
Menu startMenu, loginMenu, registerMenu, playMenu;
float sun_radius = 135f;
ArrayList<PShape> rings = new ArrayList<PShape>();
Animation sun;
Player player;
HashMap<Character, Boolean> keyMap = new HashMap<>();
PFont nightcore;
ArrayList<Planet> planets = new ArrayList<Planet>();
int counter = 60 * 10; // 60 FPS, 5 seconds total

// Loading variables
PFont campus;
int points;
float i, m, n, p, s, t, x, y;


 

void setup() {
  fullScreen(P3D); // Set to FullScreen by default, not sure if this can be changed in settings after setup
  cp5 = new ControlP5(this); // Initialize controlP5 for textboxes and user input
  // Get fonts 
  nightcore = createFont("fonts/nightcore.ttf", 200);
  campus = createFont("fonts/asupermario.ttf", 150);
  // Initialize menus 
  startMenu = new Menu();
  initializeStartMenu();
  loginMenu = new Menu();
  initializeLoginMenu();
  
  // State starts at MENU screen
  state = State.MENU;  
  
  // Create a new Animation of the Sun
  sun = new Animation("sun",60);
  // Create a new instance of Player -- TODO - Maybe delay this until player actually gets in the game ? Not sure if it makes a difference
  player = new Player();
  
  // Add players -- TODO
  Planet planet1 = new Planet("planet1", 1);
  planets.add(planet1);
  

  
  // Initialize the keyMap for player input -- this probably will not be used
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

void initializeStartMenu() {
  Button loginBtn = new Button("login", displayWidth/2 - 200, (displayHeight/2) - 300);
  Button registerBtn = new Button("register", displayWidth/2 - 200, (displayHeight/2) - 150);
  Button settingsBtn = new Button("settings", displayWidth/2 - 200, (displayHeight/2));
  Button exitBtn = new Button("exit", displayWidth/2-200, (displayHeight/2) + 150);
  startMenu.addButton(loginBtn);
  startMenu.addButton(registerBtn);
  startMenu.addButton(settingsBtn);
  startMenu.addButton(exitBtn);
}

void initializeLoginMenu() {
  Button loginBtn = new Button("login", displayWidth/2-200, displayHeight * 0.7);
  Button backBtn = new Button("back", displayWidth/2-200, (displayHeight * 0.7) + 150);
  loginMenu.addButton(loginBtn);
  loginMenu.addButton(backBtn);
  
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
  
void mousePressed() {
  switch(state) {
    case MENU:
      checkStartMenuButtons();
      break;
    case LOGIN:
      checkLoginMenuButtons();
      break;
    default:
      break;
  }
}
  
void checkStartMenuButtons() {
  for (Button b: startMenu.getButtons()) {
    if (b.isClicked()) {
      //println("Clicked button");

      switch (b.getName()) {
        case "login":
          state = State.LOGIN;
          drawTextFields();
          //println("Got the login\n");
          break;
        case "register":
          state = State.REGISTER;
          break;
        case "settings":
          break;
        case "exit":
          exit();
          return;
      }
      break;
    }
  }
}
        
void checkLoginMenuButtons() {
  for (Button b: loginMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "login":
          state = State.GAME;
          break;
        case "back":
          state = State.MENU;
          break;
        default:
          break;
      }
    }
  }
}
          

void drawTextFields() {
  // CP5 library is overkill for this - will make my own textfield
  /* Textfield b = cp5.addTextfield("Username")
      .setPosition(displayWidth/2-200, displayHeight/2 - 200)
        .setSize(400, 50)
          .setAutoClear(false)
            .setColorBackground(75)
              .setFocus(true)
                  .setFont(createFont("arial", 30));
                  */
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
  int boost = (int)player.getBoost();
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


void loadingScreen() {
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
}
void draw() {
  background(15);
  drawRings();
  switch(state) {
    case MENU:
      startMenu.drawMenu();
      break;
    case LOGIN:
      loginMenu.drawMenu();
      break;
    case REGISTER:
      break;
    case LOADING:
      loadingScreen();
      if (frameCount == counter){
        state = State.GAME;
        delay(1000);
      }
      break;
    case GAME:
      //drawMargins();
      //drawRings();
      drawPlayer();
      //drawPlanets();
      drawSun();
      break;
  }
  //println(frameRate);
}
