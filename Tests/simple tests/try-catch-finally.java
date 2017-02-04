// Source: http://stackoverflow.com/questions/3779285/exception-thrown-in-catch-and-finally-clause


class MyExc1 extends Exception {}
class MyExc2 extends Exception {}
class MyExc3 extends MyExc2 {}

public class C1 {
    public static void main(String[] args) throws Exception {
        try {
            int x = 1;
            
            try 
            {
                throw new MyExc1();
            }
            catch (Exception y) 
            {
            }
            finally 
            {
                x = 3;
                throw new Exception();
            }
        }
        catch (Exception i) 
        {
            throw new MyExc2();
        }
        finally {
            x = 2;
        }
    }

    static void q() throws Exception {
        
    }
}