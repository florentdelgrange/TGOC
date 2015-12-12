/**
 * Created by clement on 12/12/15.
 */
import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;

class Gui extends JFrame{


    public static int[] head = {1,   3,    6,   9,  11,   14,  18,  22,  25, 28,   32,  36,  39,  41,  44,  47, 49};
    public static int[] x =    {0,   0,    0,   0,  200, 200, 200, 200, 450, 450, 450, 450, 750, 750, 750, 750};
    public static int[] y =    {0, 350,  600,  950,   0, 350, 600, 950,   0, 350, 600, 950,   0, 350, 600, 950};
    public static int[] succ = {2, 5, 1, 3, 6, 2, 4, 7, 3, 8, 1, 6, 9, 2, 5, 7, 10, 3, 6, 8, 11, 4, 7, 12, 5, 10, 13, 6, 9, 11, 14, 7, 10, 12, 15, 8, 11, 16, 9, 14, 10, 13, 15, 11, 14, 16, 12, 15};
    public Gui(){
        JPanel panel=new JPanel();
        getContentPane().add(panel);
        setSize(1280,720);
    }

    public void paint(Graphics g) {
        super.paint(g);  // fixes the immediate problem.
        Graphics2D g2 = (Graphics2D) g;
        int circleSize = 20;
        int fontSize = 12;
        g2.setStroke(new BasicStroke(2));
        g2.setFont(new Font("Arial", Font.BOLD,fontSize ));

        int acc = 0;
        System.out.print("test"+head.length);
        for (int u = 0; u < 17-1; u++) { //len de head - 1
            System.out.println(u );
            for (int j = 0; j < head[u+1]-head[u]; j++) {

                g2.drawLine(50 + x[u] / 2, 50 + y[u] / 2, 50 + x[succ[acc]-1] / 2, 50 + y[succ[acc]-1] / 2);
                acc++;
            }
        }

        for (int i = 0; i < 16; i++) { //len de x
            g2.setColor(Color.white);
            g2.fillOval(-circleSize/2 +50+ x[i] / 2, -circleSize/2+50 + y[i] / 2,circleSize,circleSize);
        }
        for (int i = 0; i < 16; i++) { //len de x
            g2.setColor(Color.black);
            g2.drawOval(-circleSize/2 +50+ x[i] / 2, -circleSize/2+50 + y[i] / 2,circleSize,circleSize);
            g2.drawString(Integer.toString(i+1),-fontSize/2 + 50+ x[i] / 2, fontSize/2 +50 + y[i] / 2);
        }
    }

    public static void main(String []args){
        Gui s=new Gui();
        s.setVisible(true);
    }
}