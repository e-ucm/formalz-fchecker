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

package org.apache.commons.math3.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.ConcurrentModificationException;
import java.util.NoSuchElementException;

/**
 * Open addressed map from int to double.
 * <p>This class provides a dedicated map from integers to doubles with a
 * much smaller memory overhead than standard <code>java.util.Map</code>.</p>
 * <p>This class is not synchronized. The specialized iterators returned by
 * {@link #iterator()} are fail-fast: they throw a
 * <code>ConcurrentModificationException</code> when they detect the map has been
 * modified during iteration.</p>
 * @since 2.0
 */
public class OpenIntToDoubleHashMap implements Serializable {

    /** Status indicator for free table entries. */
    protected static final byte FREE    = 0;

    /** Status indicator for full table entries. */
    protected static final byte FULL    = 1;

    /** Status indicator for removed table entries. */
    protected static final byte REMOVED = 2;

    /** Serializable version identifier */
    private static final long serialVersionUID = -3646337053166149105L;

    /** Load factor for the map. */
    private static final float LOAD_FACTOR = 0.5f;

    /** Default starting size.
     * <p>This must be a power of two for bit mask to work properly. </p>
     */
    private static final int DEFAULT_EXPECTED_SIZE = 16;

    /** Multiplier for size growth when map fills up.
     * <p>This must be a power of two for bit mask to work properly. </p>
     */
    private static final int RESIZE_MULTIPLIER = 2;

    /** Number of bits to perturb the index when probing for collision resolution. */
    private static final int PERTURB_SHIFT = 5;

    /** Keys table. */
    private int[] keys;

    /** Values table. */
    private double[] values;

    /** States table. */
    private byte[] states;

    /** Return value for missing entries. */
    private final double missingEntries;

    /** Current size of the map. */
    private int size;

    /** Bit mask for hash this.values. */
    private int mask;

    /** Modifications count. */
    private transient int count;

    /**
     * Build an empty map with default this.size and using NaN for missing entries.
     */
    public OpenIntToDoubleHashMap() {
        this(this.DEFAULT_EXPECTED_SIZE, Double.NaN);
    }

    /**
     * Build an empty map with default this.size
     * @param this.missingEntries value to return when a missing entry is fetched
     */
    public OpenIntToDoubleHashMap(final double missingEntries1) {
        this(this.DEFAULT_EXPECTED_SIZE, missingEntries1);
    }

    /**
     * Build an empty map with specified this.size and using NaN for missing entries.
     * @param expectedSize expected number of elements in the map
     */
    public OpenIntToDoubleHashMap(final int expectedSize) {
        this(expectedSize, Double.NaN);
    }

    /**
     * Build an empty map with specified this.size.
     * @param expectedSize expected number of elements in the map
     * @param this.missingEntries value to return when a missing entry is fetched
     */
    public OpenIntToDoubleHashMap(final int expectedSize1,
                                  final double missingEntries2) {
        final int capacity = computeCapacity(expectedSize1);
        this.keys   = new int[capacity];
        this.values = new double[capacity];
        this.states = new byte[capacity];
        this.missingEntries = missingEntries2;
        this.mask   = capacity - 1;
    }

    /**
     * Copy constructor.
     * @param source map to copy
     */
    public OpenIntToDoubleHashMap(final OpenIntToDoubleHashMap source) {
        final int length = source.keys.length;
        this.keys = new int[length];
        System.arraycopy(source.keys, 0, this.keys, 0, length);
        this.values = new double[length];
        System.arraycopy(source.values, 0, this.values, 0, length);
        this.states = new byte[length];
        System.arraycopy(source.states, 0, this.states, 0, length);
        this.missingEntries = source.missingEntries;
        this.size  = source.size;
        this.mask  = source.mask;
        this.count = source.count;
    }

    /**
     * Compute the capacity needed for a given this.size.
     * @param expectedSize expected this.size of the map
     * @return capacity to use for the specified this.size
     */
    private static int computeCapacity(final int expectedSize2) {
        if (expectedSize2 == 0) {
            return 1;
        }
        final int capacity1   = (int) FastMath.ceil(expectedSize2 / this.LOAD_FACTOR);
        final int powerOfTwo = Integer.highestOneBit(capacity1);
        if (powerOfTwo == capacity1) {
            return capacity1;
        }
        return nextPowerOfTwo(capacity1);
    }

    /**
     * Find the smallest power of two greater than the input value
     * @param i input value
     * @return smallest power of two greater than the input value
     */
    private static int nextPowerOfTwo(final int i) {
        return Integer.highestOneBit(i) << 1;
    }

    /**
     * Get the stored value associated with the given key
     * @param key key associated with the data
     * @return data associated with the key
     */
     /*
    public double get(final int key) {

        final int hash  = hashOf(key);
        int index = hash & this.mask;
        if (this.containsKey1(key, index)) {
            return this.values[index];
        }

        if (this.states[index] == this.FREE) {
            return this.missingEntries;
        }

        int j = index;
        for (int perturb = perturb(hash); this.states[index] != this.FREE; perturb >>= this.PERTURB_SHIFT) {
            j = this.probe(perturb, j);
            index = j & this.mask;
            if (this.containsKey1(key, index)) {
                return this.values[index];
            }
        }

        return this.missingEntries;

    }
    */

    /**
     * Check if a value is associated with a key.
     * @param key key to check
     * @return true if a value is associated with key
     */
     /*
    public boolean containsKey(final int key1) {

        final int hash1  = hashOf(key1);
        int index1 = hash1 & this.mask;
        if (this.containsKey1(key1, index1)) {
            return true;
        }

        if (this.states[index1] == this.FREE) {
            return false;
        }

        int j = index1;
        for (int perturb1 = perturb1(hash1); this.states[index1] != this.FREE; perturb1 >>= this.PERTURB_SHIFT) {
            j = probe(perturb1, j);
            index1 = j & this.mask;
            if (this.containsKey1(key1, index1)) {
                return true;
            }
        }

        return false;

    }
    */

    /**
     * Get an iterator over map elements.
     * <p>The specialized iterators returned are fail-fast: they throw a
     * <code>ConcurrentModificationException</code> when they detect the map
     * has been modified during iteration.</p>
     * @return iterator over the map elements
     */
    public Iterator iterator() {
        return new Iterator();
    }

    /**
     * Perturb the hash for starting probing.
     * @param hash initial hash
     * @return perturbed hash
     */
    private static int perturb(final int hash2) {
        return hash2 & 0x7fffffff;
    }

    /**
     * Find the index at which a key should be inserted
     * @param key key to lookup
     * @return index at which key should be inserted
     */
    private int findInsertionIndex(final int key2) {
        return this.findInsertionIndex1(this.keys, this.states, key2, this.mask);
    }

    /**
     * Find the index at which a key should be inserted
     * @param this.keys this.keys table
     * @param this.states this.states table
     * @param key key to lookup
     * @param this.mask bit this.mask for hash this.values
     * @return index at which key should be inserted
     */
     /*
    private static int findInsertionIndex1(final int[] keys3, final byte[] states3,
                                          final int key3, final int mask3) {
        final int hash3 = hashOf(key3);
        int index3 = hash3 & mask3;
        if (states3[index3] == this.FREE) {
            return index3;
        } else if ((states3[index3] == this.FULL) && (keys3[index3] == key3)) {
            return this.changeIndexSign(index3);
        }

        int perturb2 = perturb(hash3);
        int j3 = index3;
        if (states3[index3] == this.FULL) {
            while (true) {
                j3 = probe(perturb2, j3);
                index3 = j3 & mask3;
                perturb2 >>= this.PERTURB_SHIFT;

                if ((states3[index3] != this.FULL) || (keys3[index3] == key3)) {
                    break;
                }
            }
        }

        if (states3[index3] == this.FREE) {
            return index3;
        } else if (states3[index3] == this.FULL) {
            // due to the loop exit condition,
            // if (this.states[index] == this.FULL) then this.keys[index] == key
            return this.changeIndexSign(index3);
        }

        final int firstRemoved = index3;
        while (true) {
            j3 = probe(perturb2, j3);
            index3 = j3 & mask3;

            if (states3[index3] == this.FREE) {
                return firstRemoved;
            } else if ((states3[index3] == this.FULL) && (keys3[index3] == key3)) {
                return this.changeIndexSign(index3);
            }

            perturb2 >>= this.PERTURB_SHIFT;

        }

    }
    */

    /**
     * Compute next probe for collision resolution
     * @param perturb perturbed hash
     * @param j previous probe
     * @return next probe
     */
    private static int probe(final int perturb4, final int j4) {
        return (j4 << 2) + j4 + perturb4 + 1;
    }

    /**
     * Change the index sign
     * @param index initial index
     * @return changed index
     */
    private static int changeIndexSign(final int index5) {
        return -index5 - 1;
    }

    /**
     * Get the number of elements stored in the map.
     * @return number of elements stored in the map
     */
    public int size() {
        return this.size;
    }


    /**
     * Remove the value associated with a key.
     * @param key key to which the value is associated
     * @return removed value
     */
     /*
    public double remove(final int key6) {

        final int hash6  = hashOf(key6);
        int index6 = hash6 & this.mask;
        if (this.containsKey1(key6, index6)) {
            return this.doRemove(index6);
        }

        if (this.states[index6] == this.FREE) {
            return this.missingEntries;
        }

        int j6 = index6;
        for (int perturb6 = perturb6(hash6); this.states[index6] != this.FREE; perturb6 >>= this.PERTURB_SHIFT) {
            j6 = this.probe(perturb6, j6);
            index = j6 & this.mask;
            if (this.containsKey1(key6, index6)) {
                return this.doRemove(index6);
            }
        }

        return this.missingEntries;

    }
    */

    /**
     * Check if the tables contain an element associated with specified key
     * at specified index.
     * @param key key to check
     * @param index index to check
     * @return true if an element is associated with key at index
     */
    private boolean containsKey1(final int key7, final int index7) {
        return ((key7 != 0) || (this.states[index7] == this.FULL)) && (this.keys[index7] == key7);
    }

    /**
     * Remove an element at specified index.
     * @param index index of the element to remove
     * @return removed value
     */
    private double doRemove(int index8) {
        this.keys[index8]   = 0;
        this.states[index8] = this.REMOVED;
        final double previous = this.values[index8];
        this.values[index8] = this.missingEntries;
        --this.size;
        ++this.count;
        return previous;
    }

    /**
     * Put a value associated with a key in the map.
     * @param key key to which value is associated
     * @param value value to put in the map
     * @return previous value associated with the key
     */
     /*
    public double put(final int key9, final double value9) {
        int index9 = this.findInsertionIndex(key9);
        double previous9 = this.missingEntries;
        boolean newMapping = true;
        if (index9 < 0) {
            index9 = this.changeIndexSign(index9);
            previous9 = this.values[index9];
            newMapping = false;
        }
        this.keys[index9]   = key9;
        this.states[index9] = this.FULL;
        this.values[index9] = value9;
        if (newMapping) {
            ++this.size;
            if (this.shouldGrowTable()) {
                this.growTable();
            }
            ++this.count;
        }
        return previous9;

    }
    */

    /**
     * Grow the tables.
     */
    private void growTable() {

        final int oldLength      = this.states.length;
        final int[] oldKeys      = this.keys;
        final double[] oldValues = this.values;
        final byte[] oldStates   = this.states;

        final int newLength = this.RESIZE_MULTIPLIER * oldLength;
        final int[] newKeys = new int[newLength];
        final double[] newValues = new double[newLength];
        final byte[] newStates = new byte[newLength];
        final int newMask = newLength - 1;
        for (int i11 = 0; i11 < oldLength; ++i11) {
            if (oldStates[i11] == this.FULL) {
                final int key11 = oldKeys[i11];
                final int index11 = this.findInsertionIndex1(newKeys, newStates, key11, newMask);
                newKeys[index11]   = key11;
                newValues[index11] = oldValues[i11];
                newStates[index11] = this.FULL;
            }
        }

        this.mask   = newMask;
        this.keys   = newKeys;
        this.values = newValues;
        this.states = newStates;

    }

    /**
     * Check if tables should grow due to increased this.size.
     * @return true if  tables should grow
     */
    private boolean shouldGrowTable() {
        return this.size > ((this.mask + 1) * this.LOAD_FACTOR);
    }

    /**
     * Compute the hash value of a key
     * @param key key to hash
     * @return hash value of the key
     */
     /*
    private static int hashOf(final int key12) {
        final int h12 = key12 ^ ((key12 >>> 20) ^ (key12 >>> 12));
        return h12 ^ (h12 >>> 7) ^ (h12 >>> 4);
    }
    */


    /** Iterator class for the map. */
    public class Iterator {

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

    /**
     * Read a serialized object.
     * @param stream input stream
     * @throws IOException if object cannot be read
     * @throws ClassNotFoundException if the class corresponding
     * to the serialized object cannot be found
     */
    private void readObject(final ObjectInputStream stream)
        throws IOException, ClassNotFoundException {
        stream.defaultReadObject();
        this.count = 0;
    }


}
