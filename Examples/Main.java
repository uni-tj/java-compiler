class Main {
  public static void main(String[] args) {
    new InitializeOnlyOnce(0);
  }
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