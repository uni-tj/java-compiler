class Main {
  Main() {
    return;
  }

  public static void main(String[] args) {
    int b = 0;
    Cl2 x = new Cl2();
    System.out.println(x.m2(x.v, b));
    return;
  }

}

class Cl1 {
  Cl1() {
    return;
  }

  char m1() {
    int b = 0;
    Cl2 x = new Cl2();
    return x.m2(x.v, b);
  }
}

class Cl2 {
  Cl2() {
    return;
  }

  Cl3 v;
  char m2(Cl3 v, int w) {
    return 'a';
  }
}

class Cl3 {
  Cl3() {
    return;
  }

}