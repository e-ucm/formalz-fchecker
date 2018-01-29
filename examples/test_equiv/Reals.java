package nl.uu;
import static nl.uu.impress.EDSL.*;

public class Reals {

    // 1) Simple real arithmetic
    public static float real1_1(float a) {
        pre(a >= (2 - 1 + 1));
        a += a;
        post(a >= (4 - 3 + 3));
    }
    public static float real2_1(float a) {
        pre(a > 2 || a == 2);
        a = a * 2;
        post(a > 4 || a == 4);
    }
    public static float real3_1(float a) {
        pre(a > 1);
        pre(a > 2 || a == 2);
        a = a * 2;
        a = a / 2;
        a = a * 2;
        post(a > 4 || a == 4);
        post(a > 3);
    }

    // 2) Different Java precision (irrelevant for LIR)
    public static float real1_2(float a, double b) {
        pre(a % b == 0 && a == 7.0);
        c = a / b;
        post(c == 1.0 || c == 7.0);
    }
    public static float real2_2(float a, double b) {
        pre(a % b == 0 || false);
        pre(a == 3 * 10 - 23 && true);
        c = a / b;
        post(c == 7/(1 + 5.1 - 5.1) || c == 1.0/(1.0 * 15 / 15));
    }

    // 3) Mix ints
    public static float real1_3(float a, int b) {
        pre(a > b && a < b + .3);
        b += 1;
        post(a < b - .7);
    }
    public static float real2_3(float a, int b) {
        pre(a > b);
        pre(a - (10 * .3 / 10) < b);
        b += 1;
        post(a + (.7 * (10 - 9.0)) < b);
    }
}
