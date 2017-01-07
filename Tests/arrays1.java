//Mike Scott
//examples of array manipulations

//https://www.cs.utexas.edu/~scottm/cs307/javacode/codeSamples/ArrayExamples.java

public class ArrayExamples
{	public static void main(String[] args)
	{	int[] list = new int[7];
    
		findAndPrintPairs(list, 5);
		bubblesort(list);
        
		showList(list);

		list = new int[11];
		bubblesort(list);
		showList(list);

		list = new int[11];
		bubblesort(list);
		showList(list);

		list = new int[1];
		bubblesort(list);
		showList(list);
        
	}


	// pre: list != null, list.length > 0
	// post: return index of minimum element of array
	public static int findMin(int[] list2)
	{	assert (list2 != null) && (list2.length > 0) : "failed precondition";

		int indexOfMin = 0;
		for(int k = 1; k < list2.length; k++)
		{	if(list2[k] < list2[indexOfMin])
			{	indexOfMin = k;
			}
		}

		return indexOfMin;
	}


	/*
	 *pre: list != null, newSize >= 0
	 *post: nothing. the method does not succeed it resizing the
	 * argument
	 */
	public static void badResize(int[] list3, int newSize)
	{	assert (list3 != null) && (newSize >= 0) : "failed precondition";

		int[] temp = new int[newSize];
		int limit = Math.min(list3.length, newSize);

		for(int l = 0; l < limit; l++)
		{	temp[l] = list3[l];
		}

		// uh oh!! Changing pointer, not pointee. This breaks the
		// relationship between the parameter and argument
		list3 = temp;
	}


	/*
	 *pre: list != null, newSize >= 0
	 *post: returns an array of size newSize. Elements from 0 to newSize - 1
	 *	will be copied into the new array
	 */
	public static int[] goodResize(int[] list4, int newSize)
	{	assert (list4 != null) && (newSize >= 0) : "failed precondition";

		int[] result = new int[newSize];
		int limit = Math.min(list4.length, newSize);

		for(int m = 0; m < limit; m++)
		{	result[m] = list4[m];
		}

		return result;
	}


	/*
	 *pre: list != null
	 *post: prints out the indices and values of all pairs of numbers
	 *in list such that list[a] + list[b] = target
	 */
	public static void findAndPrintPairs(int[] list5, int target)
	{	assert list5 != null : "failed precondition";

		for(int i = 0; i < list5.length; i++)
		{	for(int j = i + 1; j < list5.length; j++)
			{	if(list5[i] + list5[j] == target)
				{	System.out.println("The two elements at indices " + i + " and " + j
						+ " are " + list5[i] + " and " + list5[j] + " add up to " + target);
				}
			}
		}
	}


	/*
	 *pre: list != null;
	 *post: sort the elements of list so that they are in ascending order
	 */
	public static void bubblesort(int[] list6)
	{
		assert list6 != null : "failed precondition";

		int temp2;
		boolean changed = true;
		for(int n = 0; (n < list6.length) && changed; n++)
		{	changed = false;
			for(int o = 0; o < (list6.length - n - 1); o++)
			{	assert (o >= 0) && (o + 1 < list6.length) : "loop counter o " + o +
					"is out of bounds.";
				if(list6[o] > list6[o+1])
				{	changed = true;
					temp2 = list6[o + 1];
					list6[o + 1] = list6[o];
					list6[o] = temp2;
				}
			}
		}

		assert isAscending( list6 );
	}


	public static void showList(int[] list7)
	{	for(int p = 0; p < list7.length; p++)
			System.out.print( list7[p] + " " );
		System.out.println();
	}

	/* 	pre: list != null
		post: return true if list is sorted in ascedning order, false otherwise
	*/
	public static boolean isAscending( int[] list8 )
	{	boolean ascending = true;
		int index = 1;
		while( ascending && (index < list8.length) )
		{	assert (index >= 0) && (index < list8.length);

			ascending = (list8[index - 1] <= list8[index]);
			index++;
		}

		return ascending;
	}
}