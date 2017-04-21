
// Copied from OpenIntToDoubleHashMap.java

package org.apache.commons.math3.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.ConcurrentModificationException;
import java.util.NoSuchElementException;

/** Iterator class for the map. */
public class Iterator {
    
    protected static final byte FULL    = 1;
    private int[] keys;
    private double[] values;
    private byte[] states;
    private int count;
    
    /** Reference modification this.count. */
    private final int referenceCount;

    /** Index of current element. */
    private int current;

    /** Index of next element. */
    private int next;

    /**
     * Simple constructor.
     */
    private Iterator() {

        // preserve the modification this.count of the map to detect concurrent modifications later
        this.referenceCount = this.count;

        // initialize this.current index
        this.next = -1;
        try {
            this.advance();
        } catch (NoSuchElementException nsee) { // NOPMD
            // ignored
        }

    }

    /**
     * Check if there is a this.next element in the map.
     * @return true if there is a this.next element
     */
    public boolean hasNext() {
        return this.next >= 0;
    }

    /**
     * Get the key of this.current entry.
     * @return key of this.current entry
     * @exception ConcurrentModificationException if the map is modified during iteration
     * @exception NoSuchElementException if there is no element left in the map
     */
    public int key()
        throws ConcurrentModificationException, NoSuchElementException {
        if (this.referenceCount != this.count) {
            throw new ConcurrentModificationException();
        }
        if (this.current < 0) {
            throw new NoSuchElementException();
        }
        return this.keys[this.current];
    }

    /**
     * Get the value of this.current entry.
     * @return value of this.current entry
     * @exception ConcurrentModificationException if the map is modified during iteration
     * @exception NoSuchElementException if there is no element left in the map
     */
    public double value()
        throws ConcurrentModificationException, NoSuchElementException {
        if (this.referenceCount != this.count) {
            throw new ConcurrentModificationException();
        }
        if (this.current < 0) {
            throw new NoSuchElementException();
        }
        return this.values[this.current];
    }

    /**
     * Advance iterator one step further.
     * @exception ConcurrentModificationException if the map is modified during iteration
     * @exception NoSuchElementException if there is no element left in the map
     */
    public void advance()
        throws ConcurrentModificationException, NoSuchElementException {

        if (this.referenceCount != this.count) {
            throw new ConcurrentModificationException();
        }

        // advance on step
        this.current = this.next;

        // prepare this.next step
        try {
            while (this.states[++this.next] != this.FULL) { // NOPMD
                // nothing to do
            }
        } catch (ArrayIndexOutOfBoundsException e) {
            this.next = -2;
            if (this.current < 0) {
                throw new NoSuchElementException();
            }
        }

    }

}