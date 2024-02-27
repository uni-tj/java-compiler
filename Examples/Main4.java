public class Main {
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
    int i = 1;
    while (i <= 10) {
      if(i % 2 == 0) {
        System.out.println(ggT(33, 11));
      } else {
        System.out.println(ggT(3, 12));
      }
      i = i + 1;
    }

  }
}