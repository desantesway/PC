class Lobby {
  ArrayList<String> lobbies; // <Button, Lobbyname> 
  int lobbiesPerPage;
  int currentLobbies;
  
  Lobby() {
    this.lobbiesPerPage = 10;
    this.currentLobbies = 0;
  }
  
  void addLobby(String lobby) {
    this.lobbies.add(lobby);
    this.currentLobbies++;
  }
  
  void freeLobbies() {
    this.lobbies.clear();
    this.currentLobbies = 0;
  }
  
  void drawLobby() {
    fill(255, 127);
    rect(200,200, displayWidth * 0.6, displayWidth * 0.6);
    for (int i = 0; i < currentLobbies; i++) {
      float x = 300f;
      float y = 300f + i * 300f;
      rect( (300 + i*300), 
    }
  }
}
