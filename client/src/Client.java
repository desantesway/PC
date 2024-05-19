
public class Client {
    private static final int NULL = 0;
    private static final int CREATE_ACCOUNT = 1;
    private static final int LOGIN_ACCOUNT = 2;
    private static final int LOGOUT_ACCOUNT = 3;
    private static final int JOIN_ROOM = 4;
    private static final int LEAVE_ROOM = 5;

    public static void main(String[] args) {
        try {

            TCP tcp = new TCP("localhost", 12345);
            System.out.println(tcp.receive());
            System.out.println(tcp.pingpong(LOGOUT_ACCOUNT));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "buendia@@@buedafixe"));
            System.out.println(tcp.pingpong(LEAVE_ROOM));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "buendia@@@buedafixe"));
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "buendia@@@akfgnj"));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "buendia@@@buedafixe"));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            System.out.println(tcp.pingpong(LEAVE_ROOM));
            System.out.println(tcp.pingpong(LOGOUT_ACCOUNT));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
        } catch (Exception e) {
            print(e.getMessage());
        }
    }

    public static void print(String message) {

        System.out.println(message);
    }
}