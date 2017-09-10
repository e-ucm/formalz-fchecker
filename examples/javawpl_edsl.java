package nl.uu.javawpl_edsl;

import java.io.Console;

public class Main {

    public interface IntPred {
        boolean invoke(int n);
    }

    public static boolean forall(Object[] array, IntPred pred) {
        for(int index = 0; index < array.length; index++)
            if(!pred.invoke(index))
                return false;
        return true;
    }

    public static boolean forallr(Object[] array, int rBegin, int rEnd, IntPred pred) {
        for(int index = rBegin; index < rEnd; index++) {
            //TODO: determine if pred should be called with indices outside of the array range
            //NOTE: only relevant for the runtime implementation
            if(index < 0 || index >= array.length)
                continue; //assert false;
            if(!pred.invoke(index))
                return false;
        }
        return true;
    }

    public static boolean exists(Object[] array, IntPred pred) {
        for(int index = 0; index < array.length; index++)
            if(pred.invoke(index))
                return true;
        return false;
    }

    public static boolean existsr(Object[] array, int rBegin, int rEnd, IntPred pred) {
        for(int index = rBegin; index < rEnd; index++) {
            //TODO: determine if pred should be called with indices outside of the array range
            //NOTE: only relevant for the runtime implementation
            if(index < 0 || index >= array.length)
                continue; //assert false;
            if(pred.invoke(index))
                return true;
        }
        return false;
    }

    public static void main(String[] args) {
        assert forall(args, i -> args[i].toLowerCase().equals(args[i]));
        assert forallr(args, 1, 3, i -> args[i].contains("x"));

        assert !exists(args, i -> !args[i].toLowerCase().equals(args[i]));
        assert !existsr(args, 1, 3, i -> !args[i].contains("x"));

        System.out.println("Hello, world!");
    }
}
