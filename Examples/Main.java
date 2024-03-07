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
      if (i % 2 == 0) {
        System.out.println(ggT(new Sub().thirtythree, 11));
      } else {
        System.out.println(ggT(3, 12));
      }
      i = i + 1;
    }

    // inheritance
    Super s = new Sub();
    s.printSomething();
    new Sub2().printSomething();

    // method overloading
    Overloaded.foo(new Super());
    Overloaded.foo(new Sub());
  }
}

class Super {
  int thirtythree = 33;

  void printSomething() {
    System.out.println(false);
  }
}

class Sub extends Super {
  void printSomething() {
    System.out.println(true);
  }
}

class Sub2 extends Super {
}

class Overloaded {
  static void foo(Super s) {
    System.out.println('g');
  }

  static void foo(Sub s) {
    System.out.println('s');
  }
}