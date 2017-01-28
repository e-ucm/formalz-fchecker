// http://www.sanfoundry.com/csharp-program-stack-push-pop/

/*
 * C# Program to Implement Stack with Push and Pop operations
 */

class Program
{
    static void main(String[] args)
    {         
      stack st = new stack();
      while (true)
        {
            System.out.println("\nStack MENU(size -- 10)");
            System.out.println("1. Add an element");
            System.out.println("2. See the Top element.");
            System.out.println("3. Remove top element.");
            System.out.println("4. Display stack elements.");
            System.out.println("5. Exit");
            System.out.print("Select your choice: ");
            int choice = System.in.read();
            switch (choice)
            {
                case 1:
                    System.out.println("Enter an Element : ");
                        st.Push(System.in.read());
                        break;
 
                    case 2: System.out.printf("Top element is: {0}", st.Peek());
                        break;
 
                    case 3: System.out.printf("Element removed: {0}", st.Pop());
                        break;
 
                    case 4: st.Display();
                        break;
 
                    case 5: System.exit(1);
                        break;
                }
                System.in.read();
            }
        }
    }
 
    interface StackADT
    {
        boolean isEmpty();
        void Push(Object element);
        Object Pop();
        Object Peek();
        void Display();
    }
    
    class stack implements StackADT
    {
        public int StackSize;
        public int top;
        Object[] item;
        
        public stack()
        {
            this.StackSize = 10;
            this.item = new Object[this.StackSize];
            this.top = -1;
        }
        public stack(int capacity)
        {
        	this.StackSize = capacity;
            this.item = new Object[this.StackSize];
            this.top = -1;
        }
        public boolean isEmpty()
        {
            if (this.top == -1) return true;
 
            return false;
        }
        public void Push(Object element)
        {
            if (this.top == (this.StackSize - 1))
            {
                System.out.println("Stack is full!");
            }
 
            else
            {
 
            	this.item[++this.top] = element;
                System.out.println("Item pushed successfully!");
            }
        }
        
        public Object Pop()
        {
            if (this.isEmpty())
            {
                System.out.println("Stack is empty!");
                return "No elements";
            }
            else
            {
                return this.item[this.top--];
            }
        }
        public Object Peek()
        {
            if (this.isEmpty())
            {
                System.out.println("Stack is empty!");
                return "No elements";
            }
            else
            {
                return this.item[this.top];
            }
        }
 
 
        public void Display()
        {
            for (int i = this.top; i > -1; i--)
            {
 
                System.out.printf("Item {0}: {1}", (i + 1), this.item[i]);
            }
        }   
}
