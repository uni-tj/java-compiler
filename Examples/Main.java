public class Main {
  // static int ggT(int a, int b) {
  // if (a >= b) {
  // a = 19;
  // return a;
  // // goto after else
  // } else {
  // // return a;
  // return a; // ggT(a + 1, b);
  // }
  // // node code
  // }
  static int ggT(int a, int b) {
    if (a == b)
      return (a);
    else {
      if (a > b)
        return (ggT(a - b, b));
      else
        return (ggT(b - a, a));
    }
  }

  public static void main(String[] args) {
    int number = ggT(33, 11);
    System.out.println(number);
  }
}