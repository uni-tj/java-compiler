// package test.Test.EndToEnd;

public class NestedVariablesSuite {
  public static void main(String[] args) {
    int i = 1;
    System.out.println(i);
    {
      i = i + 1;
      System.out.println(i);
    }
    System.out.println(i);
  }
}
