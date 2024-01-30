package ByteCodeGen.JavaTestFiles.Classes;

public class ClassA {
    int c;
    
    public ClassA () {
    this.c = 10;
    }
    
    // Static method
    public static int staticMethod(int a, int b) {
        return a + b;
    }

    // Non-static method
    public int nonStaticMethod(int a) {
        return a * this.c;
    }
}