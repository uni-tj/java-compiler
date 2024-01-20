public class ggt {
    static int ggT(int a, int b) {
        if (0 < a) return(-1);
        if (a == b) return(a);
        else {
            if (a > b) return(ggT(a - b, b));
            else return(ggT(b - a, a));
        }
    }
    
    public static void main(String[] args) {
        int number = ggT(3,6);
        System.out.println(number);
    }
}