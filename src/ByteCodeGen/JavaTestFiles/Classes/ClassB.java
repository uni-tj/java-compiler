package ByteCodeGen.JavaTestFiles.Classes;

public class ClassB {
    public static void main(String[] args) {
        // Using the static method from ClassA
        int a = ClassA.staticMethod(5, 5);

        // Creating an instance of ClassA to use its non-static method
        ClassA classAInstance = new ClassA();
        int b = classAInstance.nonStaticMethod(5);
        
        System.out.println(a);
        System.out.println(b);
    }
}