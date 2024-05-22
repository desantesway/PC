class Lobby {
  ArrayList<String> lobbies;
  int currentLobbies;
  ArrayList<Button> buttons;
  String selectedRoom;
  boolean joined;
  String joinedRoom;
  Button exit, join, leave, refresh;
  
  Lobby() {
    this.buttons = new ArrayList<>();
    exit = new Button("exit", displayWidth/2, displayHeight * 0.8);
    join = new Button("join", displayWidth/2 - 500, displayHeight * 0.8);
    leave = new Button("leave", displayWidth/2 - 250, displayHeight * 0.9);
    refresh = new Button("refresh", displayWidth/2 + 400, displayHeight * 0.15);
    this.buttons.add(exit);
    this.buttons.add(join);
    this.buttons.add(refresh);
    this.buttons.add(leave);
    this.currentLobbies = 0;
    this.lobbies = new ArrayList<>();
    this.selectedRoom = "";
  }
  void setJoinedRoom() {
    this.joinedRoom = this.selectedRoom;
  }
  
  String getJoinedRoom() {
    return this.joinedRoom;
  }
  
  boolean isJoined() {
    return this.joined;
  }
  
  void leaveRoom() {
    this.joined = false;
    this.joinedRoom = "";
  }
  void updateJoined() {
    this.joined = true;
  }
  
  ArrayList<Button> getButtons() {
    return this.buttons;
  }
  
  void refresh(String[] lobbyList) {
    this.lobbies.clear();
    this.currentLobbies = 0;
    for (int i = 0; i < lobbyList.length; i++) {
      updateLobby(lobbyList[i]);
    }
  }
  
  void updateLobby(String lobby) {
    this.lobbies.add(lobby);
    this.currentLobbies++;
  }
  
  boolean isClicked(float x, float y) {
    return (mouseX >= x && mouseX <= x + displayWidth * 0.75 &&  mouseY >= y && mouseY <= y + 100);
  }
  
  void getRoomByLocation() { // I feel dirty, boss
    String res = "";
    for(int i = 0; i < currentLobbies; i++) {
      float x = 250f;
      float y = 300 + i * 120f;
      if (isClicked(x,y)) {
        //println("Clicked room");
        this.selectedRoom = lobbies.get(i);
        break;
      }
    }
    //println(this.getSelectedRoom());
  }
  
  String getSelectedRoom() {
    return this.selectedRoom;
  }
  
  void drawLobby() {

    fill(0, 190);
    float lobbyX = displayWidth * 0.1;
    float lobbyY = displayHeight * 0.1;
    String selectedRoom = getSelectedRoom();
    rect(lobbyX, lobbyY, displayWidth * 0.8, displayHeight * 0.8);
    fill(255);
    textSize(72);
    text("ROOMS", lobbyX + 70f, lobbyY + 100f);
    textSize(42);
    
    
    for (int i = 0; i < currentLobbies; i++) {
      String temp = lobbies.get(i);
      float x = 250f;
      float y = 300 + i * 120f;
      
      if (temp.equals(selectedRoom)) {
        fill(143,188,143);
      }
      else {
        fill(70);
      }
      rect(x,y, displayWidth * 0.75, 100);
      fill(255);
      text(lobbies.get(i), x+100f, y + 60f);
      if (temp.equals(joinedRoom)) {
  
        ellipse(x + displayWidth * 0.7,y+50, 25,25);
      }
    }
    fill(15);

    for (Button b : this.buttons) {
      b.drawBtn();
    }
    
  }
}
