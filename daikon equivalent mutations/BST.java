// http://www.sanfoundry.com/csharp-program-binary-search-tree-linked-list/

/*
 * C# Program to Implement Binary Search Tree using Linked List
 */

class Node
{
    public int item;
    public Node leftc;
    public Node rightc;
    
    public Node(int item, Node leftc, Node rightc)
    {
        this.item = item;
        this.leftc = leftc;
        this.rightc = rightc;
    }
    
    public Node()
    {}
    
    public void display()
    {
    	//System.out.print("[");
    	//System.out.print(this.item);
    	//System.out.print("]");
    }
}

class Tree
{
    public Node root;
    
    public Tree()
    { 
        this.root = null; 
    }
    public Node ReturnRoot()
    {
        return this.root;
    }
    public void Insert(int id)
    {
        Node newNode = new Node();
        newNode.item = id;
        if (this.root == null)
        	this.root = newNode;
        else
        {
            Node current = this.root;
            Node parent;
            
            while (true)
            {
                parent = current;
                if (id < current.item)
                {
                    current = current.leftc;
                    if (current == null)
                    {
                        parent.leftc = newNode;
                        return;
                    }
                }
                else
                {
                    current = current.rightc;
                    if (current == null)
                    {
                        parent.rightc = newNode;
                        return;
                    }
                }
            }
        }
    }
    public void Preorder(Node root1)
    {
        if (root1 != null)
        {
        	//System.out.print(root1.item + " ");
            Preorder(root1.leftc);
            Preorder(root1.rightc);
        }
    }
    public void Inorder(Node root2)
    {
        if (root2 != null)
        {
            Inorder(root2.leftc);
            //System.out.print(root2.item + " ");
            Inorder(root2.rightc);
        }
    }
    public void Postorder(Node root3)
    {
        if (root3 != null)
        {
            Postorder(root3.leftc);
            Postorder(root3.rightc);
            //System.out.print(root3.item + " ");
        }
    }
}

class Program
{
    static void main(String[] args)
    {
        Tree theTree = new Tree();
        theTree.Insert(20);
        theTree.Insert(25);
        theTree.Insert(45);
        theTree.Insert(15);
        theTree.Insert(67);
        theTree.Insert(43);
        theTree.Insert(80);
        theTree.Insert(33);
        theTree.Insert(67);
        theTree.Insert(99);
        theTree.Insert(91);            
        //System.out.println("Inorder Traversal : ");
        theTree.Inorder(theTree.ReturnRoot());
        //System.out.println(" ");
        //System.out.println();
        //System.out.println("Preorder Traversal : ");
        theTree.Preorder(theTree.ReturnRoot());
        //System.out.println(" ");
        //System.out.println();
        //System.out.println("Postorder Traversal : ");
        theTree.Postorder(theTree.ReturnRoot());
        //System.out.println(" ");
        //System.in.read();
    }
}