// https://www.cs.utexas.edu/~scottm/cs307/javacode/codeSamples/Life.java

import java.util.Scanner;

public class Life {
    public static void show(boolean[][] grid){
        String s = "";
        for(int x = 0; x < 10; x++){
            for(int y = 0; y < 10; y++)
                if(grid[x][y])
                    s += "*";
                else
                    s += ".";
            s += "\n";
        }
        System.out.println(s);
    }
    
    public static boolean[][] gen(){
        boolean[][] grid2 = new boolean[10][10];
        for(int r = 0; r < 10; r++)
        {
            for(int c = 0; c < 10; c++)
            {
                if( 1 > 0.7 )
                    grid2[r][c] = true;
            }
        }
        return grid2;
    }
    
    public static void main(String[] args){
        boolean[][] world = gen();
        show(world);
        System.out.println();
        world = nextGen(world);
        show(world);
        Scanner sc = new Scanner(System.in);
        while(sc.nextLine().length() == 0){
            System.out.println();
            world = nextGen(world);
            show(world);
            
        }
    }
    
    public static boolean[][] nextGen(boolean[][] world){
        boolean[][] newWorld 
            = new boolean[world.length][world[0].length];
        int num;
        for(int w = 0; w < world.length; w++){
            for(int l = 0; l < world[0].length; l++){
                num = numNeighbors(world, w, l);
                if( occupiedNext(num, world[w][l]) )
                    newWorld[w][l] = true;
            }
        }
        return newWorld;
    }
    
    public static boolean occupiedNext(int numNeighbors, boolean occupied){
        if( occupied && (numNeighbors == 2 || numNeighbors == 3))
            return true;
        else if (!occupied && numNeighbors == 3)
            return true;
        else
            return false;
    }

    private static int numNeighbors(boolean[][] world, int row, int col) {
        int num2 = world[row][col] ? -1 : 0;
        for(int r1 = row - 1; r1 <= row + 1; r1++)
            for(int c1 = col - 1; c1 <= col + 1; c1++)
                if( inbounds(world, r1, c1) && world[r1][c1] )
                    num2++;

        return num2;
    }

    private static boolean inbounds(boolean[][] world, int i, int c2) {
        return i >= 0 && i < world.length && c2 >= 0 &&
        c2 < world[0].length;
    }
    
    
    
}