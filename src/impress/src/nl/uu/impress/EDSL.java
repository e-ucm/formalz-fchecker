package nl.uu.impress;

public class EDSL {
    public interface IntPred {
        boolean invoke(int n);
    }

    private static boolean g_forall(int aLength, int rBegin, int rEnd, IntPred pred) {
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

    private static boolean g_exists(int aLength, int rBegin, int rEnd, IntPred pred) {
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

    public static boolean forallr(int[] array, int rBegin, int rEnd, IntPred pred) {
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

    public static boolean existsr(int[] array, int rBegin, int rEnd, IntPred pred) {
        return g_exists(array.length, rBegin, rEnd, pred);
    }

    public static void pre(boolean pre) {
        assert pre;
    }

    public static void post(boolean post) {
        assert post;
    }
}
