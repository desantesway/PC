import java.io.*;
import java.net.Socket;
import java.util.*;

public class TCP {
    //Lock l = new ReentrantLock();
    private final Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    private Thread postman;
    private Map<String,Queue<String>> taskMap;


    private void taskMapper() {
        this.taskMap = new HashMap<>();
        this.taskMap.put("pos1", new LinkedList<>());
        this.taskMap.put("pos2", new LinkedList<>());
        this.taskMap.put("pos3", new LinkedList<>());
        this.taskMap.put("pos4", new LinkedList<>());
        this.taskMap.put("p1", new LinkedList<>());
        this.taskMap.put("p2", new LinkedList<>());
        this.taskMap.put("p3", new LinkedList<>());
        this.taskMap.put("p4", new LinkedList<>());
        this.taskMap.put("res", new LinkedList<>());
        this.taskMap.put("game", new LinkedList<>());
        this.taskMap.put("chat", new LinkedList<>());
        // Add meteor types
    }

    public TCP(String host, int port) throws IOException, IllegalThreadStateException {
        this.socket = new Socket(host, port);
        this.in = new BufferedReader(new InputStreamReader(this.socket.getInputStream()));
        this.out = new PrintWriter(this.socket.getOutputStream());
        taskMapper();
        this.postman = new Thread(() -> {
            String res;
            try {
                while((res = in.readLine()) != null) {
                    String[] task = res.split("@@@",2); // Split into <Task type> , <Task>
                    Queue<String> tasks = this.taskMap.get(task[0]); // get the Queue from the HashMap
                    synchronized (tasks) { // Add the task to the Queue and notify main thread
                        tasks.add(task[1]); 
                        tasks.notify();
                    }
                }
            } catch (IOException e) {
                System.out.println("Failed to get message - postman.");
            }
        });
        this.postman.start();
    }

    public void send(int type, String args) throws IOException {
        String result = type + "@@@" + args;
        out.println(result);
        out.flush();
    }

    public String receive(String task) throws IOException, InterruptedException {
        Queue<String> tasks = this.taskMap.get(task);
        synchronized (tasks) {
            while (tasks.isEmpty()) {
                tasks.wait();
            }
            return tasks.remove();
        }
    }
}
