package nl.uu.impress;

//Importing the EDSL like this is required for the parser!
import static nl.uu.impress.specchecker.EDSL.*;

public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }

    public static void empty1() {}
    public static void empty2() {}

    public static void quickcheck_test1(int[] b, int a, int c) {
        pre(~(-a * c) == (79 & 41));
        pre(a > 0);
        pre(forall(b, i -> b[i] > -10));
    }

    public static void quickcheck_test2(int[] b, int a, int c) {
        pre(~(-a * c) == (79 & 41));
        pre(a > 0);
        pre(forall(b, i -> b[i] >= -10)); // note the >=, whereas quickcheck_test1 has >.
    }

    public static void simple_eval1() {
        pre(~(-5 * 2) == (79 & 41)); // evaluates to 9 == 9
    }

    public static void simple_eval2() {
        pre(~(-5 * 2) == (79 & 40)); // evaluates to 9 == 8
    }

    public static void simple_eval3(int a) {
        pre(~(a * 2) == (79 & 40)); // can't evaluate due to undefined variable
    }

    public static void real11(float a) {
        pre(a >= (2 - 1 + 1));
        a += a;
        post(a >= (4 - 3 + 3));
    }
    public static void real12(float a) {
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
    }

    public static void null2(int[] a, int x) {
        pre(exists(a, i -> a[i] != 0x1337));
    }

    public static void null3(int[] a) {
        pre(a == null && a[0] > a[1]);
    }

    public static void blob1(int[] a) {
        pre(forall(a, i -> a[i] == 0));
    }

    public static void test1(int[] a) {
        pre(exists(a, i -> a[i + 1] > a[i]));
    }

    public static void test1_(int[] a) {
        pre(exists(a, i -> a[i + 1] > a[i] && (a.length > (i + 1))));
    }

    public static void test2(int[] a) {
        //pre(exists(a, i -> a[i+1] >= a[i]));
    }

    //http://www.cs.uu.nl/docs/vakken/pc/1617/supplements/proofasg_1617.pdf
    public static void sorted1(int[] a) {
        pre(forall(a, i -> forallr(a, i, a.length, j -> a[i] <= a[j])));
    }

    public static void sorted2(int[] a) {
        pre(forall(a, i -> forallr(a, i, a.length, j -> a[i] < a[j])));
    }

    public static void sorted3(int[] a) {
        pre(forall(a, i -> forallr(a, i + 1, a.length, j -> a[i] < a[j])));
    }

    public static void sorted4(int[] a) {
        pre(forall(a, i -> forallr(a, i + 1, a.length, j -> a[i] <= a[j])));
    }


    public static void arr1(double[] a) {
        pre((a.length == 2) && forall(a, i -> forallr(a, i, a.length, j -> a[i] <= a[j])));
    }
    public static void arr2(double[] a) {
        pre(a.length == 2 && forall(a, i -> forallr(a, i+1, a.length, j -> a[i] < (a[j] + 1))));
    }

    public static void imp1(int[] b) {
      pre(b.length == 5);
      post(forall(b, i -> imp(i > 2, b[i] > 0)));
    }
    public static void imp2(int[] b) {
      pre(b.length == 5);
      post(forall(b, i -> imp(i > 3, b[i] > 0)));
    }

    public static void varIntro11(int[] b) {
      pre(b.length == 5);
      post(forall(b, i -> imp(i > 3, with(b[i], y -> y >= 0))));
    }
    public static void varIntro12(int[] b) {
      pre(b.length == 5);
      post(forall(b, i -> imp(i > 3, with(b[i], x -> x > 0))));
    }

    public static void varIntro21(int y) {
      pre(with(1, z -> (y == (z + 1))));
    }
    public static void varIntro22(int y) {
      pre(with(50 - 49 * (2/2), x -> (y == (x - 10 + 11))));
    }

    public static void varIntro31(int y) {
      pre(with(1, x -> x > 0));
    }
    public static void varIntro32(int y) {
      pre(with(10, x -> with(1, x -> with(0, y -> x > y))));
    }

    public static void arr11(int[] a) {
        pre(imp(a.length == 1, forall(a, i -> a[i] > 0)));
        post(1.0 == (2.1 - 1.1));
    }
    public static void arr12(int[] a) {
        pre(imp((1 + a.length) == 2 , a[0] >= 1));
    }

    public static void varIntro41(int[] a) {
      pre(with(a[0], x -> a[0] == x));
    }
    public static void varIntro42(int y) {}

    public static void array2d11(int[][] a) {
      pre(a.length == 1);
      pre(with(a[0], a0 -> a0.length == 1));
    }
    public static void array2d12(int[][] a) {
      pre(a.length == (2 - 1));
      pre(with(a[1 - 1], aa -> aa.length == (2 - 1)));
    }

    public static void array2d21(int[][] a) {
      pre(a.length == 1);
      pre(a[0].length == 1);
    }
    public static void array2d22(int[][] a) {
      pre(a.length == (2 - 1));
      pre(a[1-1].length == (3 - 1));
    }

    public static void eq11(boolean b1, boolean b2) {
      pre(imp((b1 && b2) == true, b1 == b2));
    }
    public static void eq12(boolean b1, boolean b2) {}

    public static void real21(double a) {
        pre((1.0 + 0.1) == 1.1);
    }
    public static void real22(double a) {}
}
