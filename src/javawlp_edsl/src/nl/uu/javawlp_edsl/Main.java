package nl.uu.javawlp_edsl;

import com.sun.javaws.exceptions.InvalidArgumentException;

import java.io.Console;

public class Main {

    public interface IntPred {
        boolean invoke(int n);
    }

    public static boolean g_forall(int aLength, int rBegin, int rEnd, IntPred pred) {
        for (int index = rBegin; index < rEnd; index++) {
            //TODO: determine if pred should be called with indices outside of the array range
            //NOTE: only relevant for the runtime implementation
            if (index < 0 || index >= aLength)
                continue; //assert false;
            if (!pred.invoke(index))
                return false;
        }
        return true;
    }

    public static boolean g_exists(int aLength, int rBegin, int rEnd, IntPred pred) {
        for (int index = rBegin; index < rEnd; index++) {
            //TODO: determine if pred should be called with indices outside of the array range
            //NOTE: only relevant for the runtime implementation
            if (index < 0 || index >= aLength)
                continue; //assert false;
            if (pred.invoke(index))
                return true;
        }
        return false;
    }

    public static boolean forall(Object[] array, IntPred pred) {
        return g_forall(array.length, 0, array.length, pred);
    }

    public static boolean forall(int[] array, IntPred pred) {
        return g_forall(array.length, 0, array.length, pred);
    }

    public static boolean forallr(Object[] array, int rBegin, int rEnd, IntPred pred) {
        return g_forall(array.length, rBegin, rEnd, pred);
    }

    public static boolean exists(Object[] array, IntPred pred) {
        return g_exists(array.length, 0, array.length, pred);
    }

    public static boolean exists(int[] array, IntPred pred) {
        return g_exists(array.length, 0, array.length, pred);
    }

    public static boolean existsr(Object[] array, int rBegin, int rEnd, IntPred pred) {
        return g_exists(array.length, rBegin, rEnd, pred);
    }

    public static void pre(boolean pre) {
        assert pre;
    }

    public static void post(boolean post) {
        assert post;
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

    public static void blob1(int[] a)
    {
        pre(forall(a, i -> { return a[i] == 0; }));
        post(true);
    }

    public static void test1(int[] a)
    {
        pre(exists(a, i -> a[i+1] > a[i]));
        post(true);
    }

    public static void test2(int[] a)
    {
        pre(false);
        //pre(exists(a, i -> a[i+1] >= a[i]));
        post(true);
    }

    public static void swap_spec2(int[] a, int i, int j) {
        pre(a != null && a[0] == 0);
        pre(a != null && a.length > 0 && i >= 0 && j > 0);
        // introducing vars to remember old values
        int oldai = a[i], oldaj = a[j];
        swap(a, i, j);
        post(a[j] == oldai && a[i] == oldaj);
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

    public static void main(String[] args) {
        assert forall(args, i -> args[i].toLowerCase().equals(args[i]));
        assert forallr(args, 1, 3, i -> args[i].contains("x"));

        assert !exists(args, i -> !args[i].toLowerCase().equals(args[i]));
        assert !existsr(args, 1, 3, i -> !args[i].contains("x"));

        System.out.println("Hello, world!");
    }
}
