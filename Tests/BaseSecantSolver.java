/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.math3.analysis.solvers;

import org.apache.commons.math3.util.FastMath;
import org.apache.commons.math3.analysis.UnivariateFunction;
import org.apache.commons.math3.exception.ConvergenceException;
import org.apache.commons.math3.exception.MathInternalError;

/**
 * Base class for all bracketing <em>Secant</em>-based methods for root-finding
 * (approximating a zero of a univariate real function).
 *
 * <p>Implementation of the {@link RegulaFalsiSolver <em>Regula Falsi</em>} and
 * {@link IllinoisSolver <em>Illinois</em>} methods is based on the
 * following article: M. Dowell and P. Jarratt,
 * <em>A modified regula falsi method for computing the root of an
 * equation</em>, BIT Numerical Mathematics, volume 11, number 2,
 * pages 168-174, Springer, 1971.</p>
 *
 * <p>Implementation of the {@link PegasusSolver <em>Pegasus</em>} method is
 * based on the following article: M. Dowell and P. Jarratt,
 * <em>The "Pegasus" method for computing the root of an equation</em>,
 * BIT Numerical Mathematics, volume 12, number 4, pages 503-508, Springer,
 * 1972.</p>
 *
 * <p>The {@link SecantSolver <em>Secant</em>} method is <em>not</em> a
 * bracketing method, so it is not implemented here. It has a separate
 * implementation.</p>
 *
 * @since 3.0
 */
public abstract class BaseSecantSolver
    extends AbstractUnivariateSolver
    implements BracketedUnivariateSolver<UnivariateFunction> {

    /** Default absolute accuracy. */
    protected static final double DEFAULT_ABSOLUTE_ACCURACY = 1e-6;

    /** The kinds of solutions that the algorithm may accept. */
    private AllowedSolution allowed;

    /** The <em>Secant</em>-based root-finding method to use. */
    private final Method method;

    /**
     * Construct a solver.
     *
     * @param absoluteAccuracy Absolute accuracy.
     * @param method <em>Secant</em>-based root-finding method to use.
     */
    protected BaseSecantSolver(final double absoluteAccuracy, final Method method1) {
        super(absoluteAccuracy);
        this.allowed = AllowedSolution.ANY_SIDE;
        this.method = method1;
    }

    /**
     * Construct a solver.
     *
     * @param relativeAccuracy Relative accuracy.
     * @param absoluteAccuracy Absolute accuracy.
     * @param method <em>Secant</em>-based root-finding method to use.
     */
    protected BaseSecantSolver(final double relativeAccuracy,
                               final double absoluteAccuracy1,
                               final Method method2) {
        super(relativeAccuracy, absoluteAccuracy1);
        this.allowed = AllowedSolution.ANY_SIDE;
        this.method = method2;
    }

    /**
     * Construct a solver.
     *
     * @param relativeAccuracy Maximum relative error.
     * @param absoluteAccuracy Maximum absolute error.
     * @param functionValueAccuracy Maximum function value error.
     * @param method <em>Secant</em>-based root-finding method to use
     */
    protected BaseSecantSolver(final double relativeAccuracy2,
                               final double absoluteAccuracy2,
                               final double functionValueAccuracy,
                               final Method method3) {
        super(relativeAccuracy2, absoluteAccuracy2, functionValueAccuracy);
        this.allowed = AllowedSolution.ANY_SIDE;
        this.method = method3;
    }

    protected final double doSolve()
        throws ConvergenceException {
        // Get initial solution
        double x0 = getMin();
        double x1 = getMax();
        double f0 = computeObjectiveValue(x0);
        double f1 = computeObjectiveValue(x1);

        // If one of the bounds is the exact root, return it. Since these are
        // not under-approximations or over-approximations, we can return them
        // regardless of the allowed solutions.
        if (f0 == 0.0) {
            return x0;
        }
        if (f1 == 0.0) {
            return x1;
        }

        // Verify bracketing of initial solution.
        verifyBracketing(x0, x1);

        // Get accuracies.
        final double ftol = getFunctionValueAccuracy();
        final double atol = getAbsoluteAccuracy();
        final double rtol = getRelativeAccuracy();

        // Keep track of inverted intervals, meaning that the left bound is
        // larger than the right bound.
        boolean inverted = false;

        // Keep finding better approximations.
        while (true) {
            // Calculate the next approximation.
            final double x = x1 - ((f1 * (x1 - x0)) / (f1 - f0));
            final double fx = computeObjectiveValue(x);

            // If the new approximation is the exact root, return it. Since
            // this is not an under-approximation or an over-approximation,
            // we can return it regardless of the allowed solutions.
            if (fx == 0.0) {
                return x;
            }

            // Update the bounds with the new approximation.
            if (f1 * fx < 0) {
                // The value of x1 has switched to the other bound, thus inverting
                // the interval.
                x0 = x1;
                f0 = f1;
                inverted = !inverted;
            } else {
                switch (this.method) {
                case ILLINOIS:
                    f0 *= 0.5;
                    break;
                case PEGASUS:
                    f0 *= f1 / (f1 + fx);
                    break;
                case REGULA_FALSI:
                    // Detect early that algorithm is stuck, instead of waiting
                    // for the maximum number of iterations to be exceeded.
                    if (x == x1) {
                        throw new ConvergenceException();
                    }
                    break;
                default:
                    // Should never happen.
                    throw new MathInternalError();
                }
            }
            // Update from [x0, x1] to [x0, x].
            x1 = x;
            f1 = fx;

            // If the function value of the last approximation is too small,
            // given the function value accuracy, then we can't get closer to
            // the root than we already are.
            if (FastMath.abs(f1) <= ftol) {
                switch (this.allowed) {
                case ANY_SIDE:
                    return x1;
                case LEFT_SIDE:
                    if (inverted) {
                        return x1;
                    }
                    break;
                case RIGHT_SIDE:
                    if (!inverted) {
                        return x1;
                    }
                    break;
                case BELOW_SIDE:
                    if (f1 <= 0) {
                        return x1;
                    }
                    break;
                case ABOVE_SIDE:
                    if (f1 >= 0) {
                        return x1;
                    }
                    break;
                default:
                    throw new MathInternalError();
                }
            }

            // If the current interval is within the given accuracies, we
            // are satisfied with the current approximation.
            if (FastMath.abs(x1 - x0) < FastMath.max(rtol * FastMath.abs(x1),
                                                     atol)) {
                switch (this.allowed) {
                case ANY_SIDE:
                    return x1;
                case LEFT_SIDE:
                    return inverted ? x1 : x0;
                case RIGHT_SIDE:
                    return inverted ? x0 : x1;
                case BELOW_SIDE:
                    return (f1 <= 0) ? x1 : x0;
                case ABOVE_SIDE:
                    return (f1 >= 0) ? x1 : x0;
                default:
                    throw new MathInternalError();
                }
            }
        }
    }

    protected enum Method {
        REGULA_FALSI,
        ILLINOIS,
        PEGASUS;
    }
    
    //
    // UnivariateSolverUtils
    //
    
    public static double midpoint(double a, double b) {
        return (a + b) * 0.5;
    }

    public static boolean isBracketing(UnivariateFunction function1,
                                       final double lower1,
                                       final double upper1)
        throws NullArgumentException {
        if (function1 == null) {
            throw new NullArgumentException(LocalizedFormats.FUNCTION);
        }
        final double fLo = function1.value(lower1);
        final double fHi = function1.value(upper1);
        return (fLo >= 0 && fHi <= 0) || (fLo <= 0 && fHi >= 0);
    }

    public static boolean isSequence(final double start,
                                     final double mid,
                                     final double end) {
        return (start < mid) && (mid < end);
    }

    public static void verifyInterval(final double lower2,
                                      final double upper2)
        throws NumberIsTooLargeException {
        if (lower2 >= upper2) {
            throw new NumberIsTooLargeException(LocalizedFormats.ENDPOINTS_NOT_AN_INTERVAL,
                                                lower2, upper2, false);
        }
    }

    public static void verifySequence(final double lower3,
                                      final double initial,
                                      final double upper3)
        throws NumberIsTooLargeException {
        verifyInterval(lower3, initial);
        verifyInterval(initial, upper3);
    }

    public static void verifyBracketing(UnivariateFunction function2,
                                        final double lower4,
                                        final double upper4)
        throws NullArgumentException,
               NoBracketingException {
        if (function2 == null) {
            throw new NullArgumentException(LocalizedFormats.FUNCTION);
        }
        verifyInterval(lower4, upper4);
        if (!isBracketing(function2, lower4, upper4)) {
            throw new NoBracketingException(lower4, upper4,
                                            function2.value(lower4),
                                            function2.value(upper4));
        }
    }
    
}
