import static nl.uu.impress.specchecker.EDSL.*;

public class Arrays2d {

    // 1) Simple array arithmetic
    public static void swap1_1(int[][] a, int i, int j) {
        pre(i >= 0 && j >= 0);
        swap(a, i, j);
        post(a[j][i] == oldai && a[i][j] == oldaj);
    }
    public static void swap2_1(int[][] a, int i, int j) {
        pre(i >= 0);
        pre(j > 0 || (i + 1) * j >= 0);
        swap(a, i, j);
        post(a[i][j] == oldaj);
        post(a[j][i] == oldai);
    }

    // 2) Quantifying over arrays (max)
    public static void max1_2(int[][] a) {
        pre(a != null && a.length > 0);
        pre(forall(a, i -> a[i] != null && a[i].length > 0));
        int retval = getMax(a);
        post(exists(a, i -> exists(a[i], j -> a[i][j] == retval)));
        post(forall(a, i -> forall(a[i], j -> a[i][j] <= retval)));
    }
    public static void max2_2(int[][] a) {
        pre(a != null && a.length >= 1);
        pre(forall(a, i -> a[i] != null && a[i].length >= 1));
        int retval = getMax(a);
        post(exists(a, i -> exists(a[i], j -> (a[i][j] + a[i][j]) == (retval * 2))));
        post(!exists(a, i -> exists(a[i], j -> a[i][j] > retval)));
    }

    // 3) Quantifying over array (sort by length)
    public static void sort1_3(int[][] a, int[][] sa) {
        pre(a != null);
        pre(forall(a, i -> a[i] != null));
        int[][] sa = sort_by_length(a);
        post(
          forall(sa, i ->
            forallr(sa, i, sa.length, j ->
              sa[i].length <= sa[j].length
        )));
    }
    public static void sort2_3(int[][] a, int[][] sa) {
        pre(a != null);
        pre(forall(a, i -> a[i] != null));
        int[][] sa = sort_by_length(a);
        post(
          forall(sa, i ->
            forallr(sa, i, sa.length, j ->
              sa[i].length <= sa[j].length
        )));
    }

    // 5) Quantifying over array (sort each inner array)
   /* public static void sort1_3(int[][] a, int[][] sa) {
        pre(a != null);
        pre(forall(a, i -> a[i] != null && a[i].length >= 0));
        int[][] sa = a.sort();
        post(
          forall(sa, i ->
            forall(sa[i], j ->
              forallr(sa[i], j + 1, sa[i].length, k ->
                sa[i][j] <= sa[i][k]
        ))));
    }
    public static void sort2_3(int[][] a, int[][] sa) {
        pre(a != null);
        pre(forall(a, i -> a[i] != null && a[i].length > -1));
        int[][] sa = a.sort();
        post(
          forall(sa, i ->
            forall(sa[i], j ->
              !existsr(sa[i], j + 1, sa[i].length, k ->
                sa[i][j] > sa[i][k]
        ))));
    }*/

    // 6) Quantifying over array (unique sort each inner array)
    /*public static void sort1_4(int[][] a, int[][] sa) {
        pre(a != null);
        pre(forall(a, i -> a[i] != null && a[i].length > 1));
        pre(
          forall(a, i ->
            forall(a[i], j ->
              forall(a[i], k ->
                j == k || a[i][j] != a[i][k]
        ))));
        int[][] sa = a.sort();
        post(forall(sa, i -> sa[i].length > 1));
        post(
          forall(sa, i ->
            forallr(sa[i], 0, sa[i].length - 1, j ->
              sa[i][j] < sa[i][j + 1]
        )));
    }
    public static void sort2_4(int[][] a, int[][] sa) {
        pre(a != null);
        pre(forall(a, i -> a[i] != null && a[i].length >= 2));
        pre(
          forall(a, i ->
            forallr(a[i], 0, a[i].length, j ->
              forall(a[i], k ->
                j == k || a[i][j] != a[i][k]
        ))));
        int[][] sa = a.sort();
        post(forall(sa, i -> sa[i].length >= 2));
        post(
          forall(sa, i ->
            forallr(sa[i], 1, sa[i].length, j ->
              sa[i][j] > sa[i][j + 1]
        )));
    }*/
}
