package nl.uu.javawlp_edsl;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.EDSL.*;

public class Test {

    public static void int_1(int a) {
        pre(a >= 2);
        a += a;
        post(a >= 4);
    }

    public static void int2_1(int a) {
        pre(a > 2 || a == 2);
        a = a * 2;
        post(a > 4 || a == 4);
    }

    public static void simple_2(int[] a, int i, int j) {
        pre(i >= 0 && j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
    }

    public static void simple_3(int[] a, int i, int j) {
        pre(a.length > 0 && i >= 0 && j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
    }
}
