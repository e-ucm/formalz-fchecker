package nl.uu.javawlp_edsl;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.EDSL.*;

public class Main {

    public static void quickcheck_test1(int[] b) {
        pre(~(-a * c) == (79 & 41));
        pre(a > 0);
        pre(forall(b, i -> b[i] > -10));
    }

    public static void quickcheck_test2(int[] b) {
        pre(~(-a * c) == (79 & 41));
        pre(a > 0);
        pre(forall(b, i -> b[i] >= -10)); // note the >=, whereas quickcheck_test1 has >.
    }

    public static int simple_eval1() {
        pre(~(-5 * 2) == (79 & 41)); // evaluates to 9 == 9
    }

    public static int simple_eval2() {
        pre(~(-5 * 2) == (79 & 40)); // evaluates to 9 == 8
    }

    public static int simple_eval3(int a) {
        pre(~(a * 2) == (79 & 40)); // can't evaluate due to undefined variable
    }

    public static float real1(float a) {
        pre(a >= (2 - 1 + 1));
        a += a;
        post(a >= (4 - 3 + 3));
    }

    public static float real2(float a) {
        pre(a > 2 || a == 2);
        a = a * 2;
        post(a > 4 || a == 4);
    }

    public static int mymin(int[] a, int b) {
        assert forall(a, i -> {
            return a[i] > b;
        });
        return 0;
    }

    public static void swap(int[] a, int i, int j) {
        int temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    public static void simple1(int[] a, int i, int j) {
        pre(i >= 0 && j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
    }

    public static void simple2(int[] a, int i, int j) {
        pre(a.length > 0 && i >= 0 && j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
    }

    public static void swap_spec1(int[] a, int i, int j) {
        pre(a != null);
        pre(a.length > 0);
        pre(i >= 0);
        pre(j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
    }

    public static void swap_spec2(int[] a, int i, int j) {
        pre(a != null && a[0] == 0);
        pre(a != null && a.length > 0 && i >= 0 && j > 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
    }

    public static void swap_spec3(int[] a, int i, int j) {
        // pre(a == null);
        pre(a.length > 0);
        pre(i >= 0);
        pre(j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
    }

    public static void swap_spec4(int[] a, int i, int j) {
        pre(a.length > 0);
        pre(i >= 0);
        pre(j >= 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai);
        post(a[i] == oldaj);
    }

    public static int getMax(int[] a) {
        if (a.length == 0)
            throw new IllegalArgumentException();
        int m = a[0];
        for (int i = 1; i < a.length; i++)
            m = a[i] > m ? a[i] : m;
        return m;
    }

    public static void getMax_spec1(int[] a) {
        pre(a != null && a.length > 0);
        int retval = getMax(a);
        post(exists(a, i -> a[i] == retval) && forall(a, i -> a[i] <= retval));
    }

    public static void getMax_spec2(int[] a) {
        pre(a != null && a.length > 0);
        int retval = getMax(a);
        post(exists(a, i -> a[i] == retval) && forall(a, i -> a[i] < retval));
    }

    public static void null1(int[] a, int x) {
        pre(a != null);
        post(true);
    }

    public static void null2(int[] a, int x) {
        pre(exists(a, i -> a[i] != 0x1337));
        post(true);
    }

    public static void null3(int[] a) {
        pre(a == null && a[0] > a[1]);
        post(true);
    }

    public static void blob1(int[] a) {
        pre(forall(a, i -> {
            return a[i] == 0;
        }));
        post(true);
    }

    public static void test1(int[] a) {
        pre(exists(a, i -> a[i + 1] > a[i]));
        post(true);
    }

    public static void test1_(int[] a) {
        pre(exists(a, i -> a[i + 1] > a[i] && (a.length > i + 1)));
        post(true);
    }

    public static void test2(int[] a) {
        pre(false);
        //pre(exists(a, i -> a[i+1] >= a[i]));
        post(true);
    }

    //http://www.cs.uu.nl/docs/vakken/pc/1617/supplements/proofasg_1617.pdf
    public static void sorted1(int[] a) {
        pre(forall(a, i -> forallr(a, i, a.length, j -> a[i] <= a[j])));
        post(true);
    }

    public static void sorted2(int[] a) {
        pre(forall(a, i -> forallr(a, i, a.length, j -> a[i] < a[j])));
        post(true);
    }

    public static void sorted3(int[] a) {
        pre(forall(a, i -> forallr(a, i + 1, a.length, j -> a[i] < a[j])));
        post(true);
    }

    public static void sorted4(int[] a) {
        pre(forall(a, i -> forallr(a, i + 1, a.length, j -> a[i] <= a[j])));
        post(true);
    }

    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }

    public static void arr1(double[] a) {
        pre(a.length == 2 && forall(a, i -> forallr(a, i, a.length, j -> a[i] <= a[j])));
        post(true);
    }

    public static void arr2(double[] a) {
        // pre(a.length == 2);
        // pre(forall(a, i -> forallr(a, i, a.length, j -> a[j] >= a[i])));
        pre(a.length == 2 && forall(a, i -> forallr(a, i+1, a.length, j -> a[i] < a[j] + 1)));
        post(true);
    }
}
