
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
    private static final int CREATE_ROOM = 9;
    private static final int LIST_ROOMS = 10;

    public static void main(String[] args) {
        try {

            TCP tcp = new TCP("localhost", 12345);
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "arroz@@@buedafixe"));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "arroz@@@buedafixe"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "1"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "2"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "3"));
            System.out.println(tcp.pingpong(LIST_ROOMS));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            while(true){
                System.out.println(tcp.receive());
            }
            //Thread.sleep(10000);
        } catch (Exception e) {
            print(e.getMessage());
        }
    }

    public static void print(String message) {

        System.out.println(message);
    }
}