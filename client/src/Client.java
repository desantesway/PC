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
    private static final int UP_KEY = 11;
    private static final int RIGHT_KEY = 12;
    private static final int LEFT_KEY = 13;
    private static final int CHAT_MESSAGE = 14;

    public static void main(String[] args) {
        try {

            TCP tcp = new TCP("localhost", 12345);
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "buendia@@@akfgnj"));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "buendia@@@akfgnj"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "1"));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            System.out.println(tcp.receive());
            System.out.println(tcp.receive());
            System.out.println(tcp.receive());
            System.out.println(tcp.receive());
            Thread.sleep(10000); // 10sec para ganhar, 2sec para perder
            tcp.send(UP_KEY);
            while(true){
                //tcp.send(CHAT_MESSAGE, "eu e os meus tropas, somos bandidos");
                System.out.println(tcp.receive());
            }

            /* for another player
            TCP tcp = new TCP("localhost", 12345);
            System.out.println(tcp.pingpong(CREATE_ACCOUNT, "arroz@@@buedafixe"));
            System.out.println(tcp.pingpong(LOGIN_ACCOUNT, "arroz@@@buedafixe"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "1"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "2"));
            System.out.println(tcp.pingpong(CREATE_ROOM, "3"));
            System.out.println(tcp.pingpong(LIST_ROOMS));
            System.out.println(tcp.pingpong(JOIN_ROOM, "1"));
            System.out.println(tcp.receive());
            System.out.println(tcp.receive());
            tcp.send(UP_KEY); / perder
            while(true){
                System.out.println(tcp.receive());
            }
            */

        } catch (Exception e) {
            print(e.getMessage());
        }
    }

    public static void print(String message) {

        System.out.println(message);
    }
}