class Lobby {
  ArrayList<String> lobbies; // <Button, Lobbyname> 
  int lobbiesPerPage;
  int currentLobbies;
  
  Lobby() {
    this.lobbiesPerPage = 10;
    this.currentLobbies = 0;
    this.lobbies = new ArrayList<>();
    this.updateLobby("Test1");
    this.updateLobby("Test2");
  }
  
  void updateLobby(String lobby) {
    this.lobbies.add(lobby);
    this.currentLobbies++;
  }
  
  void freeLobbies() {
    this.lobbies.clear();
    this.currentLobbies = 0;
  }
  
  void drawLobby() {
    fill(255, 127);
    float lobbyX = displayWidth * 0.1;
    float lobbyY = displayHeight * 0.1;
    rect(lobbyX, lobbyY, displayWidth * 0.8, displayHeight * 0.8);
    
    for (int i = 0; i < currentLobbies; i++) {
      fill(15);
      float x = 220f;
      float y = 210 + i * 120f;
      rect(x,y, 600, 100);
      fill(255);
      text(lobbies.get(i), x+100f, y + 60f);
    }
  }
}
