class Lobby {
  ArrayList<String> lobbies; // <Button, Lobbyname> 
  int lobbiesPerPage;
  int currentLobbies;
  ArrayList<Button> buttons;
  String selectedRoom;
  Button exit, join;
  
  Lobby() {
    this.buttons = new ArrayList<>();
    exit = new Button("exit", displayWidth/2, displayHeight * 0.8);
    join = new Button("join", displayWidth/2 - 500, displayHeight * 0.8);
    this.buttons.add(exit);
    this.buttons.add(join);
    this.lobbiesPerPage = 10;
    this.currentLobbies = 0;
    this.lobbies = new ArrayList<>();
    this.updateLobby("Test1");
    this.updateLobby("Test2");
    this.selectedRoom = "";
  }
  
  ArrayList<Button> getButtons() {
    return this.buttons;
  }
  
  void refresh(ArrayList<String> lobbyList) {
    this.lobbies.clear();
    this.currentLobbies = 0;
    for (int i = 0; i < lobbyList.size(); i++) {
      updateLobby(lobbyList.get(i));
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
      float y = 200 + i * 120f;
      if (isClicked(x,y)) {
        println("Clicked room");
        this.selectedRoom = lobbies.get(i);
        break;
      }
    }
    println(this.getSelectedRoom());
  }
  
  String getSelectedRoom() {
    return this.selectedRoom;
  }
  
  void drawLobby() {
    for (Button b : this.buttons) {
      b.drawBtn();
    }
    fill(0, 190);
    float lobbyX = displayWidth * 0.1;
    float lobbyY = displayHeight * 0.1;
    String selectedRoom = getSelectedRoom();
    rect(lobbyX, lobbyY, displayWidth * 0.8, displayHeight * 0.8);
    fill(255);
    text("ROOMS", lobbyX + 70f, lobbyY + 50f);
    
    for (int i = 0; i < currentLobbies; i++) {
      String temp = lobbies.get(i);
      float x = 250f;
      float y = 200 + i * 120f;
      if (temp.equals(selectedRoom)) {
        fill(143,188,143);
      }
      else {
        fill(70);
      }
      rect(x,y, displayWidth * 0.75, 100);
      fill(255);
      text(lobbies.get(i), x+100f, y + 60f);
    }
  }
}
