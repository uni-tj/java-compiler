class Main {
  Main() {
    return;
  }

  static int add(int a, int b) {
    return a + b;
  }

  public static void main(String[] args) {
    B b = new B();

    System.out.println(add(10,10));
    System.out.println(B.mul(10, 5));
    System.out.println(b.mul(10, 5));
    System.out.println(b.add(10,10));

    return;
  }
}

class A {
  int i;

  A() {
    this.i = 7;
    return;
  }

  public int add(int a, int b) {
    return a + b;
  }

}

class B extends A {
  B() {
    return;
  }

  public int add(int a, int b) {
    return a + b + a + b;
  }

  public static int mul(int a, int b) {
    return a * b;
  }

}