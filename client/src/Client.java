
public class Client {
    private static final int NULL = 0;
    private static final int CREATE_ACCOUNT = 1;
    private static final int LOGIN_ACCOUNT = 2;
    private static final int LOGOUT_ACCOUNT = 3;
    private static final int JOIN_ROOM = 4;
    private static final int LEAVE_ROOM = 5;
    private static final int CHANGE_NAME = 6;
    private static final int CHANGE_PASS = 7;
    private static final int REMOVE_ACCOUNT = 8;

    public static void main(String[] args) {
        try {

            TCP tcp = new TCP("localhost", 12345);
            System.out.println(tcp.receive());
            /*System.out.println(tcp.pingpong(LOGOUT_ACCOUNT));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "buendia@@@buedafixe"));
            System.out.println(tcp.pingpong(LEAVE_ROOM));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "buendiffdfa@@@buedafixe"));
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "buendia@@@akfgnj"));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "buendia@@@buedafixe"));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            System.out.println(tcp.pingpong(LEAVE_ROOM));
            System.out.println(tcp.pingpong(LOGOUT_ACCOUNT));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));*/
            System.out.println(tcp.pingpong(CHANGE_NAME, "arroz"));
            System.out.println(tcp.pingpong(CHANGE_PASS, "verycool"));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "arrozado@@@cool"));
            System.out.println(tcp.pingpong(CHANGE_NAME, "ei"));
            System.out.println(tcp.pingpong(CHANGE_PASS, "yo"));

        } catch (Exception e) {
            print(e.getMessage());
        }
    }

    public static void print(String message) {

        System.out.println(message);
    }
}