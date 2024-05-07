public class Client {
    public static void main(String[] args) {
        try {
            if ( args.length < 2) {
                print("Example usage: java client [host] [port]");
                System.exit(1);
            }

            TCP tcp = new TCP(args[0], Integer.parseInt(args[1]));


        } catch (Exception e) {
            print(e.getMessage());
        }
    }

    public static void print(String message) {
        System.out.println(message);
    }
}