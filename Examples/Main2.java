class Main {
  Main() {
    return;
  }

  static int add(int a, int b) {
    return a + b;
  }

  public static void main(String[] args) {
    B b = new B();
    A a = new A();

    System.out.println(10 + 10 * 55 + 10); // 570
    System.out.println(add(10, 10)); // 10 + 10 = 20
    System.out.println(a.add(10, 10)); // 10 + 10 + 7 = 27
    System.out.println(B.mul(10, 5)); // 10 * 5 = 50
    System.out.println(b.add(10, 10)); // 10 + 10 + 11 = 31
    System.out.println(b.div(77)); // 77 / 7 = 11
    System.out.println(-10); // -10

    return;
  }
}

class A {
  int i;

  A() {
    i = 7;
    return;
  }

  public int add(int a, int b) {
    return a + b + this.i;
  }

  public int div(int a) {
    return a / this.i;
  }

}

class B extends A {
  int i;

  B() {
    i = 11;
    return;
  }

  public int add(int a, int b) {
    return a + b + i;
  }

  public static int mul(int a, int b) {
    return a * b;
  }

}

class Super {
  A x;
}

class Sub extends Super {
  B x;
}