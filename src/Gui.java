/**
 * Created by clement on 12/12/15.
 */

import javax.swing.*;
import java.awt.*;
import java.util.Arrays;

class Gui extends JFrame{

    private int[] result;
    private int[] head; // = {1,   3,    6,   9,  11,   14,  18,  22,  25, 28,   32,  36,  39,  41,  44,  47, 49};
    private int[] x; // =    {0,   0,    0,   0,  200, 200, 200, 200, 450, 450, 450, 450, 750, 750, 750, 750};
    private int[] y; // =    {0, 350,  600,  950,   0, 350, 600, 950,   0, 350, 600, 950,   0, 350, 600, 950};
    private int[] succ; // = {2, 5, 1, 3, 6, 2, 4, 7, 3, 8, 1, 6, 9, 2, 5, 7, 10, 3, 6, 8, 11, 4, 7, 12, 5, 10, 13, 6, 9, 11, 14, 7, 10, 12, 15, 8, 11, 16, 9, 14, 10, 13, 15, 11, 14, 16, 12, 15};

    /**
     * Constructor
     * @param head head array
     * @param succ successors array
     * @param x x coordinates
     * @param y y coordinates
     */
    public Gui(int[] head, int[] succ, int[] x, int[] y, int[] result){
        this.head = head;
        this.x =x;
        this.y = y;
        this.succ = succ;
        this.result = result;
        JPanel panel = new JPanel();
        getContentPane().add(panel);
        setSize(1280,720);
    }

    public void paint(Graphics g) {
        super.paint(g);
        Graphics2D g2 = (Graphics2D) g;
        int circleSize = 20; //Size of the circles containing the numbers
        int fontSize = 12;
        g2.setStroke(new BasicStroke(2)); //Stroke boldness
        g2.setFont(new Font("Arial", Font.BOLD,fontSize ));

        int acc = 0; //Accumulator giving the curent index (is incremented as we advance in the successor array)

        for (int u = 0; u < head.length-1; u++) {
            for (int j = 0; j < head[u+1]-head[u]; j++) {
                switch(result[acc]%3) { //TODO pas le trois en brut
                    case 0 :
                        g2.setColor(Color.RED);
                        break;
                    case 1 :
                        g2.setColor(Color.BLUE);
                        break;
                    case 2 :
                        g2.setColor(Color.GREEN);
                        break;

                }
                g2.drawLine(50 + x[u] / 2, 50 + y[u] / 2, 50 + x[succ[acc]-1] / 2, 50 + y[succ[acc]-1] / 2);
                acc++;
            }
        }

        for (int i = 0; i < x.length; i++) {
            g2.setColor(Color.white); //We draw white circles (so the lines don't cross the circles)
            g2.fillOval(-circleSize/2 +50+ x[i] / 2, -circleSize/2+50 + y[i] / 2,circleSize,circleSize);
        }
        for (int i = 0; i < x.length; i++) {
            g2.setColor(Color.black);
            g2.drawOval(-circleSize/2 +50+ x[i] / 2, -circleSize/2+50 + y[i] / 2,circleSize,circleSize);
            g2.drawString(Integer.toString(i+1),-fontSize/2 + 50+ x[i] / 2, fontSize/2 +50 + y[i] / 2);
        }
    }

    public static void main(String []args){
        String fileName = "input.txt";

        //We get the array of array of int from the scala compiled into java bytecode (Main$), MODULE$ is the location of its singleton
        int values[][] = Main$.MODULE$.initGraphe(fileName);

        //We copy the arrays into new arrays (they all begin/end with added 0s for some reason)
        int[] head = Arrays.copyOfRange(values[1], 1, values[1].length);
        int[] succ = Arrays.copyOfRange(values[2], 1, values[2].length-1);
        int[] x = Arrays.copyOfRange(values[5], 0, values[5].length);
        int[] y = Arrays.copyOfRange(values[6], 0, values[6].length);

        //We launch the algorithm (I need to give it an array of String even if it is never used)
        String compulsoryParams[] = {"a","b"};
        Main$.MODULE$.main(compulsoryParams);

        //After the computation we take the result
        int[] result = Main$.MODULE$.result();

        Gui window =new Gui(head, succ, x ,y, result);
        window.setVisible(true);
    }
}