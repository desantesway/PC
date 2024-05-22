import java.io.*;
import java.net.Socket;
import java.util.List;
import java.util.Objects;
public class TCP {
    // TODO - Fix this before upload
    private static final int NULL = 0;
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

    //Lock l = new ReentrantLock();
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;

    public TCP(String host, int port) throws IOException {
        this.socket = new Socket(host, port);
        this.in = new BufferedReader(new InputStreamReader(this.socket.getInputStream()));
        this.out = new PrintWriter(this.socket.getOutputStream());
    } 

    public String pingpong(int type, String args) throws IOException {
        this.send(type, args);
        return this.receive();
    }
    
    public String register_user(String username, String password) throws IOException {
        this.send(CREATE_ACCOUNT, username + "@@@" + password);
        return this.receive();
    }

    public String login_user(String username, String password) throws IOException {
        this.send(LOGIN_ACCOUNT, username + "@@@" + password);
        return this.receive();
    }
    
    public String create_room(String room) throws IOException {
      this.send(CREATE_ROOM, room);
      return this.receive();
    }
    
    public String join_room(String room) throws IOException {
      this.send(JOIN_ROOM, room);
      return this.receive();
    }
    
    public String leave_room(String room) throws IOException {
      this.send(LEAVE_ROOM, room);
      return this.receive();
    }
  
    public String list_rooms() throws IOException { // Needed a temporary hack. Server is sending Message ++ "\n". And in.readLine() filters '\n'
      this.send(LIST_ROOMS, "Lol");
      StringBuilder line = new StringBuilder();
      String current = in.readLine();
      while (!Objects.equals(current, "!-SVDONE-!")){
        line.append(current);
        line.append('@');
        current = in.readLine();
      }
      System.out.println(line.toString());
      return line.toString();
    }
    
    
    public String pingpong(int type) throws IOException {
        this.send(type);
        return this.receive();
    }
    
    private void send(int type) throws IOException {
        out.println(type + "@@@");
        out.flush();
    }

    private void send(int type, String args) throws IOException {
        String result = type + "@@@" + args;
        out.println(result);
        out.flush();
    }

    public String receive() throws IOException {
        StringBuilder line = new StringBuilder();
        String current = in.readLine();
        while (!Objects.equals(current, "!-SVDONE-!")){
            line.append(current);
            current = in.readLine();
        }
        return line.toString();
    }
}
