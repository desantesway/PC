import java.io.*;
import java.net.Socket;
import java.util.List;
import java.util.Objects;

public class TCP {
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

    public String pingpong(int type) throws IOException {
        this.send(type);
        return this.receive();
    }

    void send(int type) throws IOException {
        out.println(type + "@@@");
        out.flush();
    }

    void send(int type, String args) throws IOException {
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