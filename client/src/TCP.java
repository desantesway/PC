import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class TCP {
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    StringBuilder sb = new StringBuilder();

    public TCP(String host, int port) throws IOException {
        this.socket = new Socket(host, port);
        this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        this.out = new PrintWriter(socket.getOutputStream());
    }

    public void send(String message) {
        out.println(message);
        out.flush();
    }

    public String receive() throws IOException {
        String message = in.readLine();
        return message;
    }
}