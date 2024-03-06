class Main {
  public static void main(String[] args) {
    Super sup = new Super();
    System.out.println(sup.x);
    new InitializeOnlyOnce(0);
  }
}

class Super {
  B x = new B();
}

class Sub extends Super {
  A x = new A();
}

class A {
}

class B extends A {

}

class InitializeOnlyOnce {
  int i = 0;

  InitializeOnlyOnce() {
    // i should be initialized here
    i = i + 1;
  }

  InitializeOnlyOnce(int dontCare) {
    this();
    // i should now be one, because this constructor should not initialize it again
    System.out.println(i == 1); // test fields initialized only once
  }
}