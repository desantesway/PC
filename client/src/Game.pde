import controlP5.*;
import java.util.*;
// CURRENT
ControlP5 cp5;
Textfield usernameField, passwordField, lobbyNameField, chatField;


private static final int CREATE_ACCOUNT = 1;
private static final int LOGIN_ACCOUNT = 2;
private static final int LOGOUT_ACCOUNT = 3;
private static final int JOIN_ROOM = 4;
private static final int LEAVE_ROOM = 5;
private static final int CHANGE_NAME = 6;
private static final int CHANGE_PASS = 7;
private static final int REMOVE_ACCOUNT = 8;
private static final int CREATE_ROOM = 9;
private static final int LIST_ROOMS = 10;
private static final int UP_KEY = 11;
private static final int RIGHT_KEY = 12;
private static final int LEFT_KEY = 13;
private static final int DOWN_KEY = 14;
private static final int CHAT_MESSAGE = 15;
private static final int LEAVE_CHAT = 16;
private static final int RANKING = 17;
private static final int GO = 18;

enum State {
    MENU,
    LOGIN,
    REGISTER,
    PLAY,
    LOBBY,
    ROOM_CREATION,
    GAME,
    LOADING,
    GAME_END,
    RANKING
}

/* SANDBOX CLIENT WITH NO SERVER COMMUNICATION FOR GRAPHICS / MENU FLOW TESTING */

boolean isLoggedIn = false;
TCP tcp;
GameState gameState;
ChatRoom chat;
Worker[] workers;
State state;
Menu startMenu, loginMenu, registerMenu, playMenu, lobbyMenu, roomCreationMenu, gameEnd, rankingMenu;
Lobby roomLobby;
float sun_radius = 200f;
Animation sun;
Player me; // My player :)
Player p1, p2, p3;
Planet pl1,pl2,pl3,pl4;
HashMap<String, Player> players;
//ArrayList<Player> players; // Other players
HashMap<Character, Boolean> keyMap = new HashMap<>();
PFont nightcore;
HashMap<String,Planet> planets;
int counter; // 60 FPS, 5 seconds total
String username;
// Loading variables
PFont campus;
int points;
float i, m, n, p, s, t, x, y;
String success = "success";
String errorMsg="";
String[] temp;
PImage backgroundImg; //!!!!!!!!!!!!!
PImage backgroundGameImg; //!!!!!!!!!!!!!
PImage starsImg; //!!!!!!!!!!!!!
PImage starsBluredImg; //!!!!!!!!!!!!!
PImage starsRedImg; //!!!!!!!!!!!!!
PImage starsRedBluredImg; //!!!!!!!!!!!!!
float translateStars = 0; //!!!!!!!!!!!!!
float translateStarsBlured = 0; //!!!!!!!!!!!!!

boolean gameStarted = false;
boolean goSignal = false;
boolean won;

 

void setup() {
  try {
    tcp = new TCP("localhost", 12345);
    this.tcp.start();
  } catch (Exception e) {
    e.printStackTrace();
    println("Failed");
    exit();
    return;
  }
  
  initializeWorkers();
  fullScreen(P3D); // Set to FullScreen by default, not sure if this can be changed in settings after setup
  cp5 = new ControlP5(this); // Initialize controlP5 for textboxes and user input
  // Get fonts 
  nightcore = createFont("fonts/Satoshi-Variable.ttf", 100);
  campus = createFont("fonts/Satoshi-Variable.ttf", 50);
  // Initialize menus 
  
  startMenu = new Menu();
  initializeStartMenu();
  loginMenu = new Menu();
  initializeLoginMenu();
  registerMenu = new Menu();
  initializeRegisterMenu();
  playMenu = new Menu();
  initializePlayMenu();
  roomCreationMenu = new Menu();
  initializeRoomCreationMenu();
  gameEnd = new Menu();
  initializeGameEndMenu();
  roomLobby = new Lobby();
  rankingMenu = new Menu();
  initializeRankingMenu();
  //println(displayWidth, displayHeight);
  backgroundImg = loadImage("background/bg.png"); //!!!!!!!!!!!!!!!!!!
  backgroundGameImg = loadImage("background/bg_red.png");
  starsImg = loadImage("background/stars.png"); //!!!!!!!!!!!!!!!!!!
  starsBluredImg = loadImage("background/stars_blured.png"); //!!!!!!!!!!!!!!!!!!
  backgroundImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  backgroundGameImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  starsImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  starsBluredImg.resize(displayWidth, displayHeight);
  starsRedImg = loadImage("background/red_stars.png"); //!!!!!!!!!!!!!!!!!!
  starsRedBluredImg = loadImage("background/red_stars_blured.png"); //!!!!!!!!!!!!!!!!!!
  starsRedImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  starsRedBluredImg.resize(displayWidth, displayHeight);
  me = new Player();
  players = new HashMap<>();
  planets = new HashMap<>();
  p1 = new Player();
  p2 = new Player();
  p3 = new Player();
  
  pl1 = new Planet();
  pl2 = new Planet();
  pl3 = new Planet();
  pl4 = new Planet();
  
  
  
  
  // State starts at MENU screen
  state = State.MENU;  
  
  // Create a new Animation of the Sun
  sun = new Animation("sun",91);
  smooth(8);
  ellipseMode(RADIUS);
  frameRate(60);
}
void initializeWorkers() {
  gameState = new GameState();
  chat = new ChatRoom();
  this.workers = new Worker[10]; // 4 players - 4 planets - 1 Game worker
  this.workers[0] = new PosWorker(this.tcp, this.gameState, "1");
  this.workers[0].start();
  this.workers[1] = new PosWorker(this.tcp, this.gameState, "2");
  this.workers[1].start();
  this.workers[2] = new PosWorker(this.tcp, this.gameState, "3");
  this.workers[2].start();
  this.workers[3] = new PosWorker(this.tcp, this.gameState, "4");
  this.workers[3].start();
  this.workers[4] = new PlanetWorker(this.tcp, this.gameState, "1");
  this.workers[4].start();
  this.workers[5] = new PlanetWorker(this.tcp, this.gameState, "2");
  this.workers[5].start();
  this.workers[6] = new PlanetWorker(this.tcp, this.gameState, "3");
  this.workers[6].start();
  this.workers[7] = new PlanetWorker(this.tcp, this.gameState, "4");
  this.workers[7].start();
  this.workers[8] = new GameWorker(this.tcp, this.gameState);
  this.workers[8].start();
  this.workers[9] = new ChatWorker(this.tcp, this.chat);
  this.workers[9].start();
  
}

void initializeRoomCreationMenu() {
  Button createBtn = new Button("create", displayWidth/2 - 1263/2 - 100, displayHeight/2 + 385/2); 
  Button exitBtn = new Button("exit", displayWidth/2 + 100 , displayHeight/2 + 385/2);
  roomCreationMenu.addButton(createBtn);
  roomCreationMenu.addButton(exitBtn);
  
}

void initializeRankingMenu() {
  Button exitBtn = new Button("exit", displayWidth/2 - 1263/4, displayHeight * 0.9 - 50);
  rankingMenu.addButton(exitBtn);
}

void initializeStartMenu() {
  Button loginBtn = new Button("login", displayWidth/2 - 1263/4, 75);
  Button registerBtn = new Button("register", displayWidth/2 - 1263/4, 75 + 50 + 385/2);
  Button settingsBtn = new Button("settings", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*2);
  Button exitBtn = new Button("exit", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*3);
  startMenu.addButton(loginBtn);
  startMenu.addButton(registerBtn);
  startMenu.addButton(settingsBtn);
  startMenu.addButton(exitBtn);
}

void initializeLoginMenu() {
  Button loginBtn = new Button("login", displayWidth/2 - 1263/2 - 100, displayHeight/2 + 385/2);
  Button backBtn = new Button("back", displayWidth/2 + 100, displayHeight/2 + 385/2);
  loginMenu.addButton(loginBtn);
  loginMenu.addButton(backBtn);
  // Create Username Textfield
  usernameField = cp5.addTextfield("Username")
                     .setPosition((displayWidth/2 - 200),(displayHeight/2) - 250)
                     .setSize(400,75)
                     .setColor(255)
                     .setColorBackground(255)
                     .setFont(createFont("arial",42))
                     .setFocus(true)
                     .setVisible(false);

  // Create Password Textfield
  passwordField = cp5.addTextfield("Password")
                     .setPosition((displayWidth/2 - 200),(displayHeight/2) - 100)
                     .setSize(400, 75)
                     .setPasswordMode(true)
                     .setColor(255)
                     .setColorBackground(255)
                     .setFont(createFont("arial",42))
                     .setVisible(false);
  
  lobbyNameField = cp5.addTextfield("Lobby name")
                      .setPosition((displayWidth/2 - 200),(displayHeight/2) - 250)
                      .setSize(400,75)
                      .setColor(255)
                      .setColorBackground(255)
                      .setFont(createFont("arial",42))
                      .setVisible(false);
}


void initializeRegisterMenu() {
  Button registerBtn = new Button("register", displayWidth/2  - 1263/2 - 100, displayHeight/2 + 385/2);
  Button backBtn = new Button("back", displayWidth/2 + 100, displayHeight/2 + 385/2);
  registerMenu.addButton(registerBtn);
  registerMenu.addButton(backBtn);
}

void initializePlayMenu() {
  Button joinBtn = new Button("join", displayWidth/2 - 1263/4, 75);
  Button createBtn = new Button("create", displayWidth/2 - 1263/4, 75 + 50 + 385/2);
  Button logoutBtn = new Button("logout", 200, displayHeight - 250);
  Button rankingBtn = new Button("ranking", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*2);
  Button exitBtn = new Button("exit", displayWidth - 800, displayHeight - 250);
  playMenu.addButton(joinBtn);
  playMenu.addButton(createBtn);
  playMenu.addButton(logoutBtn);
  playMenu.addButton(rankingBtn);
  playMenu.addButton(exitBtn);
  
}

void initializeGameEndMenu() {
  Button exitBtn = new Button("exit", displayWidth / 2 - 1263/4, displayHeight/2);
  gameEnd.addButton(exitBtn);
  chatField = cp5.addTextfield("Chat")
                     .setPosition(150,displayHeight - 150)
                     .setSize(600,60)
                     .setColor(190)
                     .setColorBackground(255)
                     .setFont(createFont("arial",40))
                     .setFocus(true)
                     .setVisible(false);
}

  void send_message(String text) {
    try {
      this.tcp.send(CHAT_MESSAGE,text);
    }catch (Exception e) {
      println("Failed to send chat message.");
    }
  }
  

  // Key pressed method
  void keyPressed() {
    switch(state) {
      case GAME:
        if ((key == CODED && key == UP) || key == 'w' || key == 'W') {
          try {
            this.tcp.send(UP_KEY, "");
          } catch (Exception e) {
            println("Couldn't up.");
          }
        } else if ((key == CODED && key == RIGHT) || key == 'd' || key == 'D') {
          try {
            this.tcp.send(RIGHT_KEY, "");
          } catch (Exception e) {
            println("Couldn't right.");
          }
        } else if ((key == CODED && key == LEFT) || key == 'a' || key == 'A') {
          try {
            this.tcp.send(LEFT_KEY, "");
          } catch (Exception e) {
            println("Couldn't left.");
          }
        } else if ((key == CODED && key == DOWN) || key == 's' || key == 'S') {
          try {
            this.tcp.send(DOWN_KEY, "");
        } catch (Exception e) {
          println("Couldn't down");
        }
        }
        break;
      case LOGIN:
        if (keyCode == TAB) {
          if (usernameField.isFocus()) {
            usernameField.setFocus(false);
            passwordField.setFocus(true);
          }
          else {
            usernameField.setFocus(true);
            passwordField.setFocus(false);
          }
        }
        break;
      case REGISTER:
        if (keyCode == TAB) {
          if (usernameField.isFocus()) {
            usernameField.setFocus(false);
            passwordField.setFocus(true);
          }
          else {
            usernameField.setFocus(true);
            passwordField.setFocus(false);
          }
        }
        break;
      case GAME_END:
        if (key == ENTER || key == RETURN) {
          String txt = chatField.getText();
          println("Sending txt " + txt + " to server.");
          if (!txt.equals("")) {
            send_message(txt);
          }
          chatField.setText("");
        }
      default:
        break;
    }
  }

  // Key released method
  void keyReleased() {
    switch(state) {
      case GAME:
        if ((key == CODED && key == UP) || key == 'w' || key == 'W') {
          try {
            this.tcp.send(UP_KEY, "");
          } catch (Exception e) {
            println("Couldn't up.");
          }
        } else if ((key == CODED && key == RIGHT) || key == 'd' || key == 'D') {
          try {
            this.tcp.send(RIGHT_KEY, "");
          } catch (Exception e) {
            println("Couldn't right.");
          }
        } else if ((key == CODED && key == LEFT) || key == 'a' || key == 'A') {
          try {
            this.tcp.send(LEFT_KEY, "");
          } catch (Exception e) {
            println("Couldn't left.");
          }
        } else if ((key == CODED && key == DOWN) || key == 's' || key == 'S') {
          try {
            this.tcp.send(DOWN_KEY, "");
          } catch (Exception e) {
            println("Couldn't left.");
          }
        }
        break;
      default:
        break;
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
    case REGISTER:
      checkRegisterMenuButtons();
      break;
    case PLAY:
      checkPlayMenuButtons();
      break;
    case ROOM_CREATION:
      checkRoomCreationButtons();
      break;
    case LOBBY:
      roomLobby.getRoomByLocation();
      checkLobbyButtons();
      break;
    case GAME:
      me = new Player();
      break;
    case GAME_END:
      checkGameEndButtons();
      break;
    case RANKING:
      checkRankingButtons();
      break;
    default:
      break;
  }
}
  
  
void toggleUserFields() {
  if(usernameField.isVisible()) usernameField.setVisible(false);
  else usernameField.setVisible(true);
  if(passwordField.isVisible()) passwordField.setVisible(false); 
  else passwordField.setVisible(true);
  usernameField.setText("");
  passwordField.setText("");
}

void checkRoomCreationButtons() {
  for (Button b: roomCreationMenu.getButtons()) {
    if (b.isClicked()) {
      //println("Clicked button");
      switch (b.getName()) {
        case "create":
          if (authCreateRoom()) {
            state = State.LOBBY;
            roomLobby.updateLobby(lobbyNameField.setVisible(false).getText());
          }
          
          break;
        case "exit":
          lobbyNameField.setVisible(false);
          state = State.PLAY;
          break;
      }
      break;
    }
  }
}

void checkGameEndButtons() { // TODO - check this works
  for(Button b: gameEnd.getButtons()) {
    if (b.isClicked()) {
      won = false;
      gameStarted = false;
      goSignal = false;
      chatField.setVisible(false);
      try {
        this.tcp.send(LEAVE_CHAT, "");
        this.tcp.receive("res");
      } catch(Exception e) {
        println("Failed to leave.");
      }
      state = State.PLAY;
    }
  }
}

void checkLobbyButtons() {
  for (Button b: roomLobby.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "join":
          if (roomLobby.isJoined()) {
             errorMsg = "Leave your room first";
          }
          else {
            if (authJoinRoom()){
                roomLobby.updateJoined();
                roomLobby.setJoinedRoom();
            }
          }
          break;
        case "leave":
          if (authLeaveRoom()) {
            roomLobby.leaveRoom();
          }
          break;
        case "exit":
          // Commented because Leave_room is currently not working properly
          if (roomLobby.isJoined()) { 
            if (authLeaveRoom()) {
              roomLobby.leaveRoom();
              state = State.PLAY;
            }
            else {
              println("Failed to leave room => " + roomLobby.getJoinedRoom());
            }
          } else {
            state = State.PLAY;
          }
          break;
        case "refresh":
          authBrowseRooms();
          break;
        default:
          break;
      }
    }
  }
}

void checkRankingButtons() {
  for (Button b: rankingMenu.getButtons()) {
    if (b.isClicked()) {
      state = State.PLAY;
    }
  }
}

void checkStartMenuButtons() {
  for (Button b: startMenu.getButtons()) {
    if (b.isClicked()) {
      //println("Clicked button");
      switch (b.getName()) {
        case "login":
          //delay(10);
          state = State.LOGIN;
          toggleUserFields();
          //println("Got the login\n");
          break;
        case "register":
          //delay(10);
          state = State.REGISTER;
          toggleUserFields();
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

boolean authBrowseRooms() {
  String res = "";
  try {
    this.tcp.send(LIST_ROOMS,"");
    res = this.tcp.receive("res");
    this.tcp.send(RANKING, "");
  } catch (IOException | InterruptedException e) {
    println("Failed");
  }
  if (!res.equals("")) {
    String[] temp = res.split("@@@");
    roomLobby.refresh(temp);  
  }
  else {
    roomLobby.refresh(new String[0]);
    return true;
  }
  
  return res.equals("");  // just in case
}

boolean authLeaveRoom() {
  if (!roomLobby.isJoined()) {
    return false;
  }

  String res = "";
  String room = roomLobby.getJoinedRoom();
  if (room.equals("")) return false;
  
  try {
    this.tcp.send(LEAVE_ROOM, "");
    res = this.tcp.receive("res");
  } catch (Exception e){
    println("Failed to leave room");
    e.printStackTrace();
    return false;
  }
  if (!res.equals(success)) {
    errorMsg = res;
  }
  return res.equals(success);
}  

boolean authCreateRoom() {
  String res = "";
  String lobbyName = lobbyNameField.getText();
  try {
    this.tcp.send(CREATE_ROOM,lobbyName);
    res = this.tcp.receive("res");
  } catch(Exception e) {
    println("Failed room creation due to exception");
  }
  return res.equals(success);
  
}

boolean authJoinRoom() {
  String res = "";
  String room = roomLobby.getSelectedRoom();
  if (room.equals("")) {
    println("No room selected.");
    return false;
  }
  try {
    this.tcp.send(JOIN_ROOM,room);
    res = this.tcp.receive("res");
  } catch (Exception e) {
    System.out.println("Failed message.");
    e.printStackTrace();
    return false;
  }
  return res.equals(success);
}  
    



boolean authLogin() {

  String res = "";
  String user = usernameField.getText();
  String password = passwordField.getText();
  try { 
    this.tcp.send(LOGIN_ACCOUNT, user+ "@@@" + password);
    res = this.tcp.receive("res");
  } catch(Exception e) {
    println("Failed auth");
  }
  if(!res.equals(success)) {
    errorMsg = res;
    return false;
  }
  username = user;

  return res.equals(success);
}

boolean authRegister() {
  String username = usernameField.getText();
  String password = passwordField.getText();
  try {
    this.tcp.send(CREATE_ACCOUNT, username + "@@@" + password);
    String res = this.tcp.receive("res");
  }  catch(Exception e) {
    println("Failed.");
    e.printStackTrace();
    return false;
  }
  return authLogin(); // Immediately login after registration*/
}

boolean authLogoutUser() {
  String res = "";
  try {
    this.tcp.send(LOGOUT_ACCOUNT, "");
    res = this.tcp.receive("res");
  } catch (Exception e) {
    println("Failed");
    return false;
  }
  return res.equals(success);
}

String[] authRanking() {
  String res = "";
  try {
    this.tcp.send(RANKING,"");
    res = this.tcp.receive("res");
  } catch (IOException | InterruptedException e) {
    println("Failed");
  }
  if (!res.equals("")) {
    String[] temp = res.split("@@@");
    return temp;
  }
  else {
    return new String[0];
  }
}

void checkLoginMenuButtons() {
  for (Button b: loginMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "login":
          if (authLogin()) {
            this.username = usernameField.getText();
            toggleUserFields();
            errorMsg = "";
            isLoggedIn = true;
            state = State.PLAY;
          } else {
            isLoggedIn = false;
          }
          
          break;
        case "back":
          toggleUserFields();
          state = State.MENU;
          break;
        default:
          break;
      }
    }
  }
}

void checkPlayMenuButtons() {
  for (Button b: playMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "join":
          if(authBrowseRooms()) {
            state = State.LOBBY;
          }
          state = State.LOBBY;
          break;
        case "create":
          state = State.ROOM_CREATION;
          lobbyNameField.setVisible(true);
          break;
        case "logout":
          if (authLogoutUser()) {
            state = State.MENU;
          }
          break;
        case "ranking":
          temp = authRanking();
          state = State.RANKING;
          break;
        case "exit":
          exit();
          return;
        default:
          break;
      }
    }
  }
}
          
void checkRegisterMenuButtons() {
  for (Button b: registerMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "register":
          if (authRegister()) {
            toggleUserFields();
            state = State.PLAY;
          }
          break;
        case "back":
          toggleUserFields();
          //delay(10);
          state = State.MENU;
          break;
        default:
          break;
      }
    }
  }
}


void drawSun() {
  fill(random(190,170), 64, 37);
  sun.display();
}


void drawPlayer() {
  if (me != null) {
      drawPlayerBoost();
      me.display();
  } else {
      textFont(nightcore);
      text("YOU LOST", 700, 200); 
    }
 players.forEach((Key, player) -> player.display());
}


void drawPlanets() {
  planets.forEach((Key,p) -> {
    p.display();
  });
}

void drawPlayerBoost() {
  int boost = (int)me.getBoost();
  textFont(campus);
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

void drawBackground(){ //!!!!!!!!!!!!!!!!!!
  if (state == State.GAME){
    image(backgroundGameImg, 0, 0); 
  } else{
    image(backgroundImg, 0, 0); 
  }
  
  pushMatrix();
  translate(translateStarsBlured+displayWidth,0);
  if (state == State.GAME){
    image(starsRedBluredImg, 0, 0);
  } else{
    image(starsBluredImg, 0, 0);
  }
  popMatrix();
  
  pushMatrix();
  translate(translateStarsBlured,0);
  if (state == State.GAME){
    tint(255, 127);
    image(starsRedBluredImg, 0, 0);
    tint(255, 255);
  } else{
    image(starsBluredImg, 0, 0);
  }
  popMatrix();
  
  pushMatrix();
  translate(translateStars+displayWidth,0);
  if (state == State.GAME){
    image(starsRedImg, 0, 0);
  } else{
    image(starsImg, 0, 0);
  }
  popMatrix();
  
  pushMatrix();
  translate(translateStars,0);
  if (state == State.GAME){
    image(starsRedImg, 0, 0);
  } else{
    image(starsImg, 0, 0);
  }
  popMatrix();
  if (translateStars <= -displayWidth){
    translateStars=translateStars + displayWidth;
  }
  if (translateStarsBlured <= -displayWidth){
    translateStarsBlured=translateStarsBlured + displayWidth;
  }
  translateStars = translateStars - 1 * 0.125;
  translateStarsBlured = translateStarsBlured - 1 * 0.0625;
 
}

void initializeGameState() {
  println("Initialized");
  gameStarted = true;
  GameState copy = null;
  gameState.l.writeLock().lock();
  try {
  copy = this.gameState.copy();
  } finally {
    gameState.l.writeLock().unlock();
  }
  if (copy != null) {
    println("Successful copy");
    Map<String, float[]> pos = copy.positions;
    Map<String, float[]> pls = copy.planets;
    for (Map.Entry<String, float[]> entry : pos.entrySet()) {
      String name = entry.getKey();  
      float[] posAngle = entry.getValue();
      if (name.equals(username)) {
        me.setUsername(name);
        me.setPos(posAngle[0], posAngle[1], posAngle[2]);
        me.setAngle(posAngle[3]);          
      } else {
        if (!p1.getStatus()){
            p1 = new Player(posAngle[0], posAngle[1], posAngle[2], name);
            p1.setUsername(name);
            players.put(name,p1);
        } else if (!p2.getStatus()) {
            p2 = new Player(posAngle[0], posAngle[1], posAngle[2], name);
            p2.setUsername(name);
            players.put(name,p2);
        } else if (!p3.getStatus()) {
            p3 = new Player(posAngle[0], posAngle[1], posAngle[2], name);
            p3.setUsername(name);
            players.put(name,p3);
        }
      }    
    }
      for (Map.Entry<String, float[]> e : pls.entrySet()) {
        String planetIndex = e.getKey();
        float[] posVel = e.getValue();
        if (!pl1.getStatus()){
            pl1 = new Planet("planet1",1,posVel[0], posVel[1], posVel[2], posVel[3]);
            planets.put(planetIndex,pl1);
        } else if (!pl2.getStatus()) {
            pl2 = new Planet("planet2",1,posVel[0], posVel[1], posVel[2], posVel[3]);
            planets.put(planetIndex,pl2);
        } else if (!pl3.getStatus()) {
            pl3 = new Planet("planet3",1,posVel[0], posVel[1], posVel[2], posVel[3]);
            planets.put(planetIndex,pl3);
        } else if (!pl4.getStatus()) {
            pl4 = new Planet("planet4",1,posVel[0], posVel[1], posVel[2], posVel[3]);
            planets.put(planetIndex,pl4);          
        }
      }
  }
} 


void updateGameState() {
  GameState copy = null;
  gameState.l.writeLock().lock();
  try {
    copy = this.gameState.copy();
  } finally { 
    gameState.l.writeLock().unlock();  // free the lock asap
  }
  if (copy != null) {
    if (copy.won || copy.lost) {
      if(copy.won){
        won = true;
      
      }else{
        won = false;
      }
      chatField.setVisible(true);
      state = State.GAME_END;
    } else {
    Set<String> deaths = copy.deaths;
    if (deaths.isEmpty());
    else {
      for(String death : deaths) {
        if (death.equals(username)) {
          me = null;
        } else if (players.containsKey(death)) {
          players.remove(death);
        }
      }
    }
    Map<String, float[]> pos = copy.positions;
    Map<String, float[]> pls = copy.planets;
    for (Map.Entry<String, float[]> entry : pos.entrySet()) {
      String name = entry.getKey();  
      float[] posAngle = entry.getValue();
      if (name.equals(username)) {
        if (me != null) {
          me.setPos(posAngle[0], posAngle[1], posAngle[2]);
          me.setAngle(posAngle[3]);      
        }
      }else {
         Player e = players.get(name);
         e.setPos(posAngle[0], posAngle[1], posAngle[2]);
         e.setAngle(posAngle[3]); 
       }
      }
   for (Map.Entry<String, float[]> e : pls.entrySet()) {
        String planetIndex = e.getKey();
        float[] posVel = e.getValue();
        Planet pl = planets.get(planetIndex);
        //println("Setting pl"+planetIndex+" with ("+ Float.valueOf(posVel[0]) +","+ Float.valueOf(posVel[1])+"," +Float.valueOf(posVel[2])+","+ Float.valueOf(posVel[3])+")");
        pl.setPosVel(posVel[0], posVel[1], posVel[2], posVel[3]);
      }
  } 
  }
}

void displayResultText() {
  if (!won) {
    textSize(70);
    text("YOU LOST...", 700, 300); 
  } else {
    textSize(70);
    text("YOU WON!", 700, 300); 
  }
  ChatRoom copy = null;
  chat.l.writeLock().lock();
  try {
    copy = this.chat.copy();
  } finally { 
    chat.l.writeLock().unlock();  // free the lock asap
  }
  if (copy != null) {
    List<String> result = copy.getMessages();
    textFont(createFont("arial", 30));
    for(int i = 0 ; i < result.size();i++) {
      text(result.get(i), displayWidth * 0.8, displayHeight/2 + 500 - (i*100));
    }
  }
}

void draw() {
  
  drawBackground(); //!!!!!!!!!!!!!!!!!!
  //drawRings();
  switch(state) {
    case MENU:
      startMenu.drawMenu();
      break;
    case LOGIN:
      loginMenu.drawMenu();
      
      if (!isLoggedIn) {
        text(errorMsg, displayWidth/2-200, 100);
      }
      break;
    case REGISTER:
      registerMenu.drawMenu();
      break;
    case PLAY:
      playMenu.drawMenu();
      break;
    case LOADING:
      loadingScreen();
      if(!gameStarted) {
          initializeGameState();
      }
      if (frameCount > counter){
        
        counter = frameCount + 3*60;
        state = State.GAME;
      }
      break;
    case LOBBY:
      String res;
      try{
        res = this.tcp.waiting("res");
        
        if (res.equals("")){
          roomLobby.drawLobby();
        } else if (res.equals("countdown_started")) {
          
        } else if (res.equals("enter_game")) {
           counter = frameCount + 2*60; // 4 seconds
           //GameState(float posX, float posY,float boost, String[] enemyIndex, float[] enemies, 
           //        String[] planetIndexes, float[] planets, boolean countdown){
           state = State.LOADING;
        }
      } catch (Exception e) {
        println("Failed lobby wait.");
      }
      roomLobby.drawLobby();
      break;
    case ROOM_CREATION:
      roomCreationMenu.drawMenu();
      break;
    case GAME:
      
      if (frameCount > counter) {
        if(!goSignal) {
            try {
                this.tcp.send(GO, "");
            }catch (Exception e) {
                e.printStackTrace();
           }
        }
        updateGameState();
        drawPlayer();
        drawPlanets();
      } 
      
      
      drawSun();
      break;
    case GAME_END:
      gameEnd.drawMenu();
      displayResultText();
      break;
    case RANKING:
      rankingMenu.drawMenu();
      fill(0, 190);
      rect(displayWidth / 2 - 600, displayHeight * 0.1 - 50, displayWidth / 2 + 200, displayHeight * 0.8);
      
      fill(255);
      textSize(40);
      textFont(campus);
    
      // Headers
      text("Name", displayWidth / 2 - 400, displayHeight * 0.1);
      text("Level", displayWidth / 2 + 200, displayHeight * 0.1);
      text("XP", displayWidth / 2 + 400, displayHeight * 0.1);
    
      for (int i = 0; i < temp.length; i++) {
        String[] parts = temp[i].split("_");
        String name = parts[0];
        String level = parts[1];
        String xp = parts[2];
    
        text(name, displayWidth / 2 - 400, (displayHeight * 0.1) + (i + 1) * 100 + 30);
        text(level, displayWidth / 2 + 200, (displayHeight * 0.1) + (i + 1) * 100 + 30);
        text(xp, displayWidth / 2 + 400, (displayHeight * 0.1) + (i + 1) * 100 + 30);
      }
      break;
  }
  //println(frameRate);
}
