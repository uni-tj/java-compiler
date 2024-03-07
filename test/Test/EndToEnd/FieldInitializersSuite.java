// package test.Test.EndToEnd;

class FieldInitializersSuite {
  public static void main(String[] args) {
    new InitializeOnlyOnceTest(0);
  }
}

class InitializeOnlyOnceTest {
  int i = 0;

  InitializeOnlyOnceTest() {
    // i should be initialized here
    i = i + 1;
  }

  InitializeOnlyOnceTest(int dontCare) {
    this();
    // i should now be one, because this constructor should not initialize it again
    System.out.println(i == 1); // test fields initialized only once
  }
}