class Main {
  public static void main(String[] args) {
    int b = 0;
    char a = 'x';
    Cl4 x = new Cl4();
    x.m4();
    Cl5.m5();
    return;
  }
}

class Cl1 {
  char m1() {
    int b = 0;
    Cl2 x = new Cl2();
    return x.m2(x.v, b);
  }
}

class Cl2 {
  int i;
  Cl3 v;

  char m2(Cl3 v, int w) {
    this.i = 10;
    return 'a';
  }
}

class Cl3 {
}

class Cl4 extends Cl2 {
  // char m3(char a) {
  // int accu = 0;
  // while (this.i < 100) {
  // accu = accu + this.i;
  // this.i = this.i + 1;
  // System.out.println(accu);
  // }
  // System.out.println(this.i);
  // return a;

  // }

  void m4() {
    int k = 0;
    while (k != 100) {
      System.out.println(k);
      k = k + 1;
    }
  }
}

class Cl5 {
  static void m5() {
    int x = 5;
    {
      int y = 3;
      System.out.println(x);
      System.out.println(y);
      x = 4;
    }
    System.out.println(x);
  }
}