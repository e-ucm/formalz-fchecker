package nl.uu;
import static nl.uu.impress.EDSL.*;

public class Arrays {

    // 1) Simple array arithmetic
    public static void swap1_1(int[] a, int i, int j) {
        pre(i >= 0 && j >= 0);
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
    }
    public static void swap2_1(int[] a, int i, int j) {
        pre(i >= 0);
        pre(j > 0 || (i + 1) * j >= 0);
        swap(a, i, j);
        post(a[i] == oldaj);
        post(a[j] == oldai);
    }

    // 2) Quantifying over array (max)
    public static void max1_2(int[] a) {
        pre(a != null && a.length > 0);
        int retval = getMax(a);
        post(exists(a, i -> a[i] == retval));
        post(forall(a, i -> a[i] <= retval));
    }
    public static void max2_2(int[] a) {
        pre(a != null && a.length >= 1);
        int retval = getMax(a);
        post(exists(a, i -> (retval * 2) == (a[i] + a[i])));
        post(!exists(a, i -> a[i] > retval));
    }

    // 3) Quantifying over array (sort)
    public static void sort1_3(int[] a) {
        pre(a != null && a.length >= 0);
        int[] sa = a.sort();
        post(forall(a, i ->
                forallr(a,  i + 1, a.length, j -> a[i] <= a[j])));
    }
    public static void sort2_3(int[] a) {
        pre(a != null && a.length > -1);
        int[] sa = a.sort();
        post(forall(a, i ->
                !existsr(a, i + 1, a.length, j -> a[i] > a[j])));
    }

    // 4) Quantifying over array (unique sort)
    public static void sort1_4(int[] a) {
        pre(a != null && a.length > 1);
        pre(forall(a, i ->
              forall(a, j -> i == j || a[i] != a[j])));
        int[] sa = a.sort();
        post(a.length > 1);
        post(forallr(a, 0, a.length - 1, i -> a[i] < a[i + 1]));
    }
    public static void sort2_4(int[] a) {
        pre(a != null);
        pre(a.length >= 2);
        pre(forallr(a, 0, a.length, i ->
              forall(a, j -> i == j || a[i] != a[j])));
        int[] sa = a.sort();
        post(a.length >= 2);
        post(forallr(a, 1, a.length, i -> a[i] > a[i - 1]));
    }
}
