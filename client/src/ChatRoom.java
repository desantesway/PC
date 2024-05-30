import java.util.*;
import java.util.concurrent.locks.*;

public class ChatRoom {
    private static final int MAX_MESSAGES = 10;
    private Deque<Message> messages;
    public ReadWriteLock l;

    // Inner Message class to store username and message
    public static class Message {
        private String username;
        private String text;

        public Message(String username, String text) {
            this.username = username;
            this.text = text;
        }

        @Override
        public String toString() {
            return username + ": " + text;
        }
    }

    // Constructor
    public ChatRoom() {
        this.l = new ReentrantReadWriteLock();
        this.messages = new LinkedList<>();
    }

    public ChatRoom(Deque<Message> messages) {
        this.messages = messages;
    }

    // Method to add a message to the chat room
    public void addMessage(String username, String text) {
        if (this.messages.size() >= MAX_MESSAGES) {
            this.messages.removeLast(); // remove the oldest message (last in the deque)
        }
        System.out.println("Adding message " + username + ":" + text);
        this.messages.addFirst(new Message(username, text));
    }

    // Method to get all messages in the chat room in the correct order
    public List<String> getMessages() {
        List<String> result = new LinkedList<>();
        for (Message message : this.messages) {
            result.add(message.toString());
        }
        return result;
    }

    public ChatRoom copy() {
        return new ChatRoom(this.messages); 
    }
}
